/*******************************************************************************
 * Copyright (c) 2025 ARCAD Software.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     ARCAD Software - initial API and implementation
 *******************************************************************************/
package com.arcadsoftware.rest;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;

import org.restlet.Client;
import org.restlet.Context;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.data.ChallengeResponse;
import org.restlet.data.ChallengeScheme;
import org.restlet.data.CharacterSet;
import org.restlet.data.ClientInfo;
import org.restlet.data.Form;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Preference;
import org.restlet.data.Protocol;
import org.restlet.data.Reference;
import org.restlet.data.Status;
import org.restlet.representation.FileRepresentation;
import org.restlet.representation.Representation;
import org.restlet.representation.StringRepresentation;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.osgi.ILoggedPlugin;
import com.arcadsoftware.osgi.ISODateFormater;
import com.arcadsoftware.rest.connection.NegotiateAuthenticationHelper;
import com.arcadsoftware.rest.internal.Messages;

/**
 * Client access to web services.
 * 
 * <p>
 * This class provide a unique entry point to the Client REST API.
 * 
 * <ul>
 * <li>Support authentication with login+password and other protocols (SPNEGO and OIDC).
 * <li>Proxy configuration.
 * <li>SSL management, for HTTPS connection.
 * <li>POST and PUT without parameters (locked by some proxy).
 * <li>Allow to get WADL documentation of web-services.
 * <li>In GET request bypass query size limit by using POST method instead (Require AFS web-service).
 * <li>Allow to use only GET/POST methods to simulate all methods PUT/DELETE/OPTIONS (Require AFS web-service).
 * <li>Retry request when network error, or timeout, occurs.
 * <li>Diagnose HTTP Error.
 * </ul>
 */
public class WebServiceAccess {

	public final static String URI_SEPARATOR = "/"; //$NON-NLS-1$

	/**
	 * Current LANGUAGE from the current JVM locale.
	 */
	public static final Language LANGUAGE = new Language(Locale.getDefault().getLanguage() + '-' + Locale.getDefault().getCountry());
	
	/**
	 * Current general LANGUAGE (i.e. locale language, without country specification).
	 */
	public static final Language LANGUAGE_GENERAL = new Language(Locale.getDefault().getLanguage());
	
	private static final Preference<MediaType> MEDIATYPE_XML = new Preference<MediaType>(MediaType.APPLICATION_XML);
	private static final Preference<MediaType> MEDIATYPE_JSON = new Preference<MediaType>(MediaType.APPLICATION_JSON);
	private static final Preference<Language> PREFERENCE_LANGUAGE = new Preference<Language>(LANGUAGE);
	private static final Preference<Language> PREFERENCE_LANGUAGE_GENERAL = new Preference<Language>(LANGUAGE_GENERAL, 0.5F);
	private static final Preference<CharacterSet> PREFERENCE_CHARSET = new Preference<CharacterSet>(CharacterSet.UTF_8);
	private static final Preference<MediaType> PREFERENCE_ALLMEDIATYPES = new Preference<MediaType>(MediaType.ALL);
	private static final Preference<CharacterSet> PREFERENCE_ALLCHARSETS = new Preference<CharacterSet>(CharacterSet.ALL);
	// System Property used to inject a minimal attributes into empty POST/PUT requests.
	private static final boolean ALLOWEMPTYPOSTS = "true".equalsIgnoreCase(System.getProperty("com.arcadsoftware.allowemptypost")); //$NON-NLS-1$ //$NON-NLS-2$
	// System Property used to force all "non POST/GET" method to go through GET/POST methods.
	private static final boolean METHODPROXY = "true".equalsIgnoreCase(System.getProperty(BaseResource.METHODPROXIED)); //$NON-NLS-1$
	// System Property used to force the usage of GET body for parameters
	private static final int DEFAULTQUERYLIMIT;
	private static final Representation EMPTYPARAM;
	
	static {
        // Define a default "empty" form representation (non empty form are require with PUT/POST method by some Proxy and firewall.
		Form form = new Form();
		form.add("_empty_", "none"); //$NON-NLS-1$ //$NON-NLS-2$
		EMPTYPARAM = form.getWebRepresentation();
		DEFAULTQUERYLIMIT = Integer.getInteger("com.arcadsoftware.httpquerylimit", 15000);
	}
	
	
	private final ILoggedPlugin activator;
	private final RestConnectionParameters parameters;
	// TODO Be able to store many server addresses (cluster) with failover rules...
	private String address;
	private String defaultServeraddress;
	private Client client;
	private int latency;
	private int retry;
	private String login;
	private char[] password;
	private boolean useSSO;
	private String token;
	private volatile XStreamCompact xs;
	private int queryLimit = DEFAULTQUERYLIMIT;
	private boolean json;

	/**
	 * Create a new WSA object with no log and default parameters.
	 * 
	 */
	public WebServiceAccess() {
		this(null, null);
	}
	
	/**
	 * Create a new WSA object with the given parameters, note that any future modification of the parameters will be not taken into account by this WSA object.
	 * 
	 * @param activator may be null.
	 * @param parameters may be null.
	 */
	public WebServiceAccess(final ILoggedPlugin activator, RestConnectionParameters parameters) {
		super();
		this.activator = activator;
		if (parameters == null) {
			this.parameters = new RestConnectionParameters(activator);
		} else {
			this.parameters = parameters.clone();
		}
	}
	
	/**
	 * Create a Server access interface for a specific plugin.
	 * 
	 * @param activator Activator instance used to log debug informations.
	 */
	public WebServiceAccess(final ILoggedPlugin activator) {
		this(activator, null);
	}

	/**
	 * Create a Server access interface for a specific plugin.
	 * 
	 * <p>
	 * Use this version to instantiate an particular SSL trusted connection, if the server does not use an 
	 * "authority Certified" certificates and require a Client SSL Authentication.
	 * 
	 * <p>
	 * <b>WARNING</b>: Note that the current implementation require that the KeyStore contain a unique key pair, and 
	 * if this key is encrypted the password must be the same as the keyStore one.
	 * 
	 * @param activator Activator instance used to log debug informations.
	 * @param truststore A truststore file path of JKS type.
	 * @param truststorepass The truststore password.
	 * @param keystore AKeyStore file path (JKS Type) that contain the key pair used to authenticated the client.
	 * @param keystorepass the keyStore password, and the key pair password too (if encrypted).
	 * @param keypass the key password.
	 * @param ignoreHostName Define if the hostname from the TLS certificate will be ignored or not.
	 * @deprecated Use {@link #WebServiceAccess(ILoggedPlugin, RestConnectionParameters)}
	 */
	@Deprecated
	public WebServiceAccess(final ILoggedPlugin activator, File truststore, char[] truststorepass, File keystore, char[] keystorepass, char[] keypass, boolean ignoreHostName) {
		this(activator, null);
		if (truststore != null) {
			parameters.setTrustStore(truststore, truststorepass);
		}
		if (keystore != null) {
			parameters.setKeyStore(keystore, keystorepass, keypass);
		}
		parameters.setIgnoreHostName(ignoreHostName);  
	}

	/**
	 * Create a Server access interface for a specific plugin.
	 * 
	 * <p>
	 * Use this version to instantiate an particular SSL trusted connection, if the server does not use an 
	 * "authority Certified" certificates and require a Client SSL Authentication.
	 * 
	 * <p>
	 * <b>WARNING</b>: Note that the current implementation require that the KeyStore contain a unique key pair, and 
	 * if this key is encrypted the password must be the same as the keyStore one.
	 * 
	 * @param activator Activator instance used to log debug informations.
	 * @param truststore A truststore file path of JKS type.
	 * @param truststorepass The truststore password.
	 * @param keystore AKeyStore file path (JKS Type) that contain the key pair used to authenticated the client.
	 * @param keystorepass the keyStore password, and the key pair password too (if encrypted).
	 * @param ignoreHostName Define if the hostname from the TLS certificate will be ignored or not.
	 * @deprecated Use {@link #WebServiceAccess(ILoggedPlugin, RestConnectionParameters)}
	 */
	@Deprecated
	public WebServiceAccess(final ILoggedPlugin activator, String truststore, String truststorepass, String keystore, String keystorepass, boolean ignoreHostName) {
		this(activator, null);
		if (truststore != null) {
			if (truststorepass != null) {
				parameters.setTrustStore(new File(truststore), truststorepass.toCharArray());
			} else {
				parameters.setTrustStore(new File(truststore), null);
			}
		}
		if (keystore != null) {
			if (keystorepass != null) {
				parameters.setKeyStore(new File(keystore), keystorepass.toCharArray(), null);
			}
		}
		parameters.setIgnoreHostName(ignoreHostName);  
	}
	
	/**
	 * Create a Server access interface for a specific plugin.
	 * 
	 * <p>
	 * Use this version to instantiate an particular SSL trusted connection, if the server does not use an 
	 * "authority Certified" certificates and require a Client SSL Authentication.
	 * 
	 * <p>
	 * <b>WARNING</b>: Note that the current implementation require that the KeyStore contain a unique key pair, and 
	 * if this key is encrypted the password must be the same as the keyStore one.
	 * 
	 * @param activator Activator instance used to log debug informations.
	 * @param truststore A truststore file path of JKS type.
	 * @param truststorepass The truststore password.
	 * @param keystore AKeyStore file path (JKS Type) that contain the key pair used to authenticated the client.
	 * @param keystorepass the keyStore password, and the key pair password too (if encrypted).
	 * @deprecated Use {@link #WebServiceAccess(ILoggedPlugin, RestConnectionParameters)}
	 */
	@Deprecated
	public WebServiceAccess(final ILoggedPlugin activator, String truststore, String truststorepass, String keystore, String keystorepass) {
		this(activator, truststore, truststorepass, keystore, keystorepass, false);
	}
	
	/**
	 * Create a Server access interface for a specific plugin.
	 * 
	 * <p>
	 * Use this version to instantiate an particular SSL trusted connection, if the server does not use an "authority Certified" certificates.
	 * 
	 * @param activator Activator instance used to log debug informations.
	 * @param truststore A truststore file path of JKS type.
	 * @param truststorepass The truststore password.
	 * @param ignoreHostName Define if the hostname from the TLS certificate will be ignored or not.
	 * @deprecated Use {@link #WebServiceAccess(ILoggedPlugin, RestConnectionParameters)}
	 */
	@Deprecated
	public WebServiceAccess(final ILoggedPlugin activator, String truststore, String truststorepass, boolean ignoreHostName) {
		this(activator, truststore, truststorepass, null, null, ignoreHostName);
	}
	
	/**
	 * Create a Server access interface for a specific plugin.
	 * 
	 * <p>
	 * Use this version to instantiate an particular SSL trusted connection, if the server does not use an "authority Certified" certificates.
	 * 
	 * @param activator Activator instance used to log debug informations.
	 * @param truststore A truststore file path of JKS type.
	 * @param truststorepass The truststore password.
	 * @deprecated Use {@link #WebServiceAccess(ILoggedPlugin, RestConnectionParameters)}
	 */
	@Deprecated
	public WebServiceAccess(final ILoggedPlugin activator, String truststore, String truststorepass) {
		this(activator, truststore, truststorepass, null, null, false);
	}

	/**
	 * Create a Server access interface for a specific plugin.
	 * 
	 * @param activator Activator instance used to log debug informations.
	 * @param serveraddress The server URL
	 * @param login User login to access to secured services.
	 * @param password User password to access to secured services.
	 */
	public WebServiceAccess(final ILoggedPlugin activator, String serveraddress, String login, char[] password) {
		this(activator);
		this.defaultServeraddress = serveraddress;
		this.login = login;
		this.password = password;
	}

	/**
	 * Create a Server access interface for a specific plugin.
	 * 
	 * @param activator Activator instance used to log debug informations.
	 * @param serveraddress The server URL
	 * @param login User login to access to secured services.
	 * @param password User password to access to secured services.
	 * @deprecated use {@link #WebServiceAccess(ILoggedPlugin, String, String, char[])}
	 */
	@Deprecated
	public WebServiceAccess(final ILoggedPlugin activator, String serveraddress, String login, String password) {
		this(activator, serveraddress, login, password.toCharArray());
	}

	/**
	 * Set the XML serializer used to serialize the parameters. 
	 * @param xs
	 */
	public void setXStream(XStreamCompact xs) {
		synchronized (this) {
			this.xs = xs;
		}
	}
	
	/**
	 * @return the XStream serializer used to serialize complex parameters. 
	 */
	public synchronized XStreamCompact getXStream() {
		if (xs == null) {
			xs = new XStreamCompact(WebServiceAccess.class.getClassLoader());
		}
		return xs;
	}
	
	/**
	 * Return the best server address usable at the moment.
	 * 
	 * <p>
	 * This method is not intended to be extended, see <code>loadServerAddress()</code>.
	 * 
	 * @return and HTTP URL, never null.
	 */
	public final String getServerAddress() {
		if (address == null) {
			client = null;
			// Check system properties...
			String a = null;
			try {
				a = loadServerAddress();
			} catch (Exception e) {
				if (activator != null) {
					activator.debug(Messages.WebServiceAccess_Error_Address, e);
				}
			}
			if ((a == null) || (a.length() == 0)) {
				// The very default server Address.
				a = System.getProperty("com.arcadsoftware.server", //$NON-NLS-1$
						"http://localhost:5252"); //$NON-NLS-1$
			}
			if (a.endsWith("/")) { //$NON-NLS-1$
				a = a.substring(0, a.length() - 1);
			}
			if (a.endsWith(":80")) { //$NON-NLS-1$
				a = a.substring(0, a.length() - 3);
			}
			String al = a.toLowerCase();
			if (!al.startsWith("http://") && !al.startsWith("https://")) { //$NON-NLS-1$  //$NON-NLS-2$
				a = "http://" + a; //$NON-NLS-1$
			}
			address = a;
		}
		return address;
	}

	/**
	 * Load the server address from configuration.
	 * <p>
	 * Default implementation return the default Server Address, witch is null.
	 * 
	 * <p>
	 * If null id returned then the server address is obtained from the system property
	 * <code>com.arcadsoftware.server</code>. If this property is not set then the very default address
	 * "http://localhost:5252/" is used.
	 * 
	 * @return the server address.
	 */
	protected String loadServerAddress() {
		return defaultServeraddress;
	}

	/**
	 * Reset the currently used server address.
	 */
	public void clearAdress() {
		address = null;
		// Force client reinititialization !
		client = null;
	}

	/**
	 * Define the time between two request retry to the server.
	 * 
	 * @param latency
	 */
	public void setLatency(int latency) {
		this.latency = latency;
	}

	/**
	 * @return the time between to request retry to the server.
	 */
	public int getLatency() {
		return latency;
	}

	/**
	 * State if this client prefer response in JSON (or in XML).
	 * 
	 * <p>
	 * Default is XML.
	 * 
	 * @return
	 */
	public boolean isUseJson() {
		return json;
	}

	/**
	 * Define if this client prefer response in JSON (or in XML).
	 * 
	 * <p>
	 * Default is XML.
	 * 
	 * @param json
	 */
	public void setUseJson(boolean json) {
		this.json = json;
	}
	
	/**
	 * Set the number of retry to success a request to the server.
	 * 
	 * @param retry
	 */
	public void setRetry(int retry) {
		this.retry = retry;
	}

	/**
	 * @return the number of tentative to success a request.
	 */
	public int getRetry() {
		return retry;
	}

	/**
	 * @param defaultServeraddress
	 *            the defaultServeraddress to set
	 * @see #loadServerAddress()
	 */
	public void setDefaultServeraddress(String defaultServeraddress) {
		this.defaultServeraddress = defaultServeraddress;
		address = null;
	}

	/**
	 * @return the default Server Address
	 * @see #loadServerAddress()
	 */
	public String getDefaultServeraddress() {
		return defaultServeraddress;
	}

	/**
	 * If true the HTTP Client will attempt an SSO Connection to the server instead of sending login and password.
	 *  
	 * @return
	 */
	public boolean isUseSSO() {
		return useSSO;
	}

	/**
	 * If true the HTTP Client will attempt an SSO Connection to the server instead of sending login and password.
	 * 
	 * @param useSSO
	 */
	public void setUseSSO(boolean useSSO) {
		this.useSSO = useSSO;
	}

	/**
	 * Define the size limit (Byte) of the QUERY part of request. If a query go over 
	 * this size then it is automatically transformer into a FORM into the Request BODY.
	 * 
	 * <p>
	 * A Value of zero force the usage of the request body for all query. 
	 * 
	 * <p>
	 * Note that the usage of a Request BODY is not standard in HTTP for GET requests.
	 * 
	 * @return A negative value disable the mechanism.
	 */
	public int getQueryLimit() {
		return queryLimit;
	}

	/**
	 * Define the size limit (Byte) of the QUERY part of request. If a query go over 
	 * this size then it is automatically transformer into a FORM into the Request BODY.
	 * 
	 * <p>
	 * A Value of zero force the usage of the request body for all query. 
	 * 
	 * <p>
	 * Note that the usage of a Request BODY is not standard in HTTP for GET requests.
	 * 
	 * @param queryLimit
	 */
	public void setQueryLimit(int queryLimit) {
		this.queryLimit = queryLimit;
	}

	/**
	 * 
	 * @return the protocol associated with the stored address.
	 */
	protected Protocol getProtocol() {
		String a = getServerAddress().toLowerCase();
		if (a.startsWith("http:")) { //$NON-NLS-1$
			return Protocol.HTTP;
		}
		if (a.startsWith("https:")) { //$NON-NLS-1$
			return Protocol.HTTPS;
		}
		return Protocol.valueOf(a); // to test !
	}

	/**
	 * Get a valid client to connect to the server.
	 * 
	 * @return
	 */
	protected Client getClient() {
		if (client == null) {
			synchronized (this) {
				if (client == null) {
					client = initializeClient();
				}
			}
		}
		return client;
	}

	/**
	 * Small stop of current thread before to retry to call the server.
	 */
	protected void sleep() {
		if (latency > 0) {
			try {
				Thread.sleep(latency);
			} catch (InterruptedException e) {}
		}
	}

	/**
	 * Set the User login used to connect to the server.
	 * 
	 * @param login
	 */
	public void setLogin(String login) {
		this.login = login;
	}

	/**
	 * Set the User password used to connect to the server.
	 * 
	 * @param password
	 */
	public void setPassword(char[] password) {
		Crypto.clear(this.password);
		this.password = password;
	}

	/**
	 * Get the User login used to connect to the Server.
	 * 
	 * @return
	 */
	public String getLogin() {
		return login;
	}
	
	/**
	 * Get the current OAuth Token (useful for refreshing it).
	 * 
	 * @return if null other authentication method will be used.
	 */
	public String getToken() {
		return token;
	}

	/**
	 * Set the current OAuth authorization token.
	 * If null other authentication method will be used.
	 * 
	 * @param token
	 */
	public void setToken(String token) {
		this.token = token;
	}

	
	/**
	 * Prepare a secured request to the server.
	 * 
	 * <p>
	 * Default implementation put a login and password with HTTP Basic header.
	 * 
	 * @param request
	 *            the request to send to the server.
	 * @return the converted request to send.
	 */
	protected Request preprocess(Request request) {
		if (useSSO) {
			request.setChallengeResponse(new NegotiateAuthenticationHelper(activator, getServerAddress()).createChallengeResponse());
		} else if (token != null) {
			ChallengeResponse oauth = new ChallengeResponse(ChallengeScheme.HTTP_OAUTH_BEARER);
			oauth.setRawValue(token);
			request.setChallengeResponse(oauth);
		} else if ((login != null) && (login.length() > 0)) {
			request.setChallengeResponse(new ChallengeResponse(ChallengeScheme.HTTP_BASIC, login, password));
		}
		return request;
	}

	/**
	 * Post-process a secured request to the server.
	 * 
	 * <p>
	 * Default implementation does not do anything.
	 * 
	 * @param response
	 *            the server returned response.
	 * @return the converted response.
	 */
	protected Response postprocess(Response response) {
		return response;
	}

	/**
	 * Set the proxy authentication scheme.
	 * 
	 * <p>
	 * Calling this method will activate the Proxy configuration. To reset this configuration call <code>restProxy()</code>

	 * <p>
	 * Activating Proxy parameters where not necessary is not recommended. It may produce performance draw back.
	 * 
	 * @param host
	 *            Proxy server hostname, can be null. In both case a ProxySelector will be set to detect Proxy configuration from host.
	 * @param port
	 *            Proxy server port, can be zero (for default port number).
	 * @param login
	 *            login to the proxy server, can be null if no authentication is required.
	 * @param password
	 *            password to the proxy server, can be null if no authentication is required.
	 */
	public void setProxy(String host, int port, String login, char[] password) {
		parameters.setProxy(host, port);
		parameters.setProxylogin(login, password);
		// Force client reinititialization !
		client = null;
	}
	
	/**
	 * Deactivate Proxy setting.
	 * 
	 * <p>
	 * Activating Proxy parameters where not necessary is not recommended. It may produce performance draw back.
	 */
	public void resetProxy() {
		parameters.setProxy(null, 0);
		parameters.setProxylogin(null, null);
		// Force client reinititialization !
		client = null;
	}

	/**
	 * @return the proxy host address.
	 */
	public String getProxyHost() {
		return parameters.getProxyHost();
	}

	/**
	 * @return the proxy port address.
	 */
	public int getProxyPort() {
		return parameters.getProxyPort();
	}

	/**
	 * @return the proxy login.
	 */
	public String getProxyLogin() {
		return parameters.getProxyLogin();
	}

	/**
	 * @return the proxy password.
	 */
	public char[] getProxyPwd() {
		return parameters.getProxyPassword();
	}

	/**
	 * The Reference is a mutable Uniform Resource Identifier (URI).
	 * 
	 * @param servicePath the path of the requested resource.
	 * @return a reference to the server.
	 */
	public Reference getServerReference(String servicePath) {
		if ((servicePath == null) || (servicePath.length() == 0)) {
			servicePath = "/"; //$NON-NLS-1$
		} else if (!(servicePath.charAt(0) == '/')) {
			servicePath = '/' + servicePath;
		}
		return new Reference(getServerAddress() + servicePath);
	}

	/**
	 * Initialize the client object according to local needs.
	 * 
	 * @return the client object
	 */
	private Client initializeClient() {
		final ArrayList<Protocol> protocols = new ArrayList<Protocol>(1);
		protocols.add(getProtocol());
		// Force the usage of the Internal Client implementation.
		// HTTP Client configuration.
		return parameters.init(new Client(new Context(), protocols, "org.restlet.engine.connector.HttpClientHelper")); //$NON-NLS-1$
	}

	/**
	 * Get the corresponding URL to the specified path on the server.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @return
	 * @throws MalformedURLException
	 */
	public URL getURL(String servicePath) throws MalformedURLException {
		return getServerReference(servicePath).toUrl();
	}

	/**
	 * Get the web representation (FORM) of the parameters Map.
	 * 
	 * @param parameters
	 *            the parameter map to transform, can be null.
	 * @return null if there is no parameters.
	 */
	protected Representation getParameterRepresentation(Map<String, Object> parameters) {
		if ((parameters == null) || (parameters.size() == 0)) {
			return null;
		}
		// Construct the Form object.
		return getParameterForm(parameters).getWebRepresentation();
	}

	/**
	 * Add the given parameters to the reference query part if and only if the resulting query does not overflow the fixed limit. 
	 * Use this method to passe parameters to a GET method call.
	 * 
	 * <p>
	 * Note that by extension, if there is no parameters this method always return true.
	 * Note also that as soon as the limit is set to zero any parameter fail to be added to the query part of the request.
	 * 
	 * @param reference The URL of the service
	 * @param parameters The parameters of the service.
	 * @return True if the parameter were not too long to be added to the query.
	 */
	protected boolean canParametersAsQuery(Reference reference, Map<String, Object> parameters) {
		if ((parameters != null) && (parameters.size() > 0)) {
			if (queryLimit == 0) {
				return false;
			}
			for(Entry<String, Object> e:parameters.entrySet()) {
				if (e.getValue() != null) {
					reference.addQueryParameter(e.getKey(), e.getValue().toString());
				}
			}
			if ((queryLimit > 0) && (queryLimit < reference.getQuery().length())) {
				reference.setQuery(""); //$NON-NLS-1$
				return false;
			}
		}
		return true;
	}

	/**
	 * Add the parameters to the request QUERY.
	 * 
	 * <p>
	 * NOTE That this method does not check the resulting size of the query.
	 * 
	 * @param reference
	 * @param parameters
	 * @return
	 * @see #canParametersAsQuery(Reference, Map)
	 */
	protected Reference getParametersAsQuery(Reference reference, Map<String, Object> parameters) {
		if ((parameters != null) && (parameters.size() > 0)) {
			for (Entry<String, Object> e: parameters.entrySet()) {
				if (e.getValue() != null) {
					reference.addQueryParameter(e.getKey(), e.getValue().toString());
				}
			}
		}
		return reference;
	}

	/**
	 * Send a GET request to the server.
	 * 
	 * <p>
	 * This request assume that an authentication is required, and follow redirection to get the final response.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @return the response as a string, null if an error occurs. An empty string is returned if an empty entity is
	 *         returned.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public String get(String servicePath) throws ServerErrorException {
		return get(servicePath, true, true);
	}

	/**
	 * Send a simple GET request to the server.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @param secure
	 *            true if a secure connection must be used.
	 * @param redirection
	 *            true if server redirection must be followed. If false the string result can be an URL of the
	 *            destination resource.
	 * @return the response as a string, null if an error occurs. An empty string is returned if an empty entity is
	 *         returned.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public String get(String servicePath, boolean secure, boolean redirection) throws ServerErrorException {
		return proceed(handle(Method.GET, getServerReference(servicePath), null, null, retry, secure, false, redirection, null), null);
	}
	
	/**
	 * Send a simple GET request to the server.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @param secure
	 *            true if a secure connection must be used.
	 * @param redirection
	 *            true if server redirection must be followed. If false the string result can be an URL of the
	 *            destination resource.
	 * @param parameters 
	 *            in a HashTable: key=name of the parameter, value=value of the parameter. Can be null.
	 * @return the response as a string, null if an error occurs. An empty string is returned if an empty entity is
	 *         returned.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public String get(String servicePath, boolean secure, boolean redirection, Map<String, Object> parameters) throws ServerErrorException {
		Reference reference = getServerReference(servicePath);
		if (canParametersAsQuery(reference, parameters)) {
			return proceed(handle(Method.GET, reference, null, null, retry, secure, false, redirection, null), null);
		}
		reference.addQueryParameter(BaseResource.QUERY_METHODPROXIED, Method.GET.getName());
		return proceed(handle(Method.POST, reference, getParameterRepresentation(parameters), null, retry, secure, false, redirection, null), null);
	}
	
	/**
	 * Send a simple GET request to the server.
	 * 
	 * <p>
	 * Follow redirections and use a secured connection.
	 * 
	 * @param servicePath
	 *            the relative web-resource path.
	 * @param calendar
	 *            will be filled with the last modification date of the data...
	 * @return the string representation of the Data.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public String get(String servicePath, Calendar calendar) throws ServerErrorException {
		return proceed(handle(Method.GET, getServerReference(servicePath), null, calendar, retry, true, false, true, null), calendar);
	}

	/**
	 * Get a request result to the server according to the given parameters.
	 * 
	 * <p>
	 * Follow redirections and use a secured connection.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @param parameters
	 *            in a HashTable: key=name of the parameter, value=value of the parameter. Can be null.
	 * @return the response as a string, null if an error occurs. An empty string is returned if an empty entity is
	 *         returned.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public String get(String servicePath, Map<String, Object> parameters) throws ServerErrorException {
		Reference reference = getServerReference(servicePath);
		if (canParametersAsQuery(reference, parameters)) {
			return proceed(handle(Method.GET, reference, null, null, retry, true, false, true, null), null);
		}
		reference.addQueryParameter(BaseResource.QUERY_METHODPROXIED, Method.GET.getName());
		return proceed(handle(Method.POST, reference, getParameterRepresentation(parameters), null, retry, true, false, true, null), null);
	}

	/**
	 * Get a request result to the server according to the given parameters send as a request BODY.
	 * 
	 * <p>
	 * Note that this method use a BODy to the GET request, which is not a standard way to use HTTP.
	 * The web service need to be ready to receive such a request.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @param representation
	 *            The request BODY.
	 * @return the response as a string, null if an error occurs. An empty string is returned if an empty entity is
	 *         returned.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public String get(String servicePath, Representation representation) throws ServerErrorException {
		return proceed(handle(Method.GET, getServerReference(servicePath), representation, null, retry, true, false, true, null), null);
	}

	/**
	 * Get the whole site WADL documentation.
	 * 
	 * <p>
	 * Note that this call may take some time (seconds) so do not call it too often, or if you only require the documentation of one specific web-service.
	 *  
	 * @return null if this site does not implement the OPTIONS method with WADL dialect.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 * @see #options(String)
	 */
	public Representation options() throws ServerErrorException {
		return options(""); //$NON-NLS-1$
	}
	
	
	/**
	 * Get the Web-service WADL definition.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @return null if this web-servide does not implement the OPTIONS method with WADL dialect.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public Representation options(String servicePath) throws ServerErrorException {
		Response response = handle(Method.OPTIONS, getServerReference(servicePath), null, null, retry, true, false, true, MediaType.APPLICATION_WADL);
		// Process the response.
		if (response.getStatus().isSuccess() && response.isEntityAvailable()) {
			return response.getEntity();
		}
		return null;
	}

	/**
	 * Method which allows to create a resource. This method create an "empty" item.
	 * 
	 * <p>
	 * Follow redirections and use a secured connection.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @return the response as a string, null if an error occurs. An empty string is returned if an empty entity is
	 *         returned.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public String post(String servicePath) throws ServerErrorException {
		return proceed(handle(Method.POST, getServerReference(servicePath), null, null, retry, true, false, true, null), null);
	}

	/**
	 * Method which allows to create a resource. A post is sent in order to pass parameters
	 * 
	 * <p>
	 * Follow redirections and use a secured connection.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @param parameters
	 *            in a HashTable: key=name of the parameter, value=value of the parameter
	 * @return the response as a string, null if an error occurs. An empty string is returned if an empty entity is
	 *         returned.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public String post(String servicePath, Map<String, Object> parameters) throws ServerErrorException {
		return proceed(handle(Method.POST, getServerReference(servicePath), getParameterRepresentation(parameters), null, retry, true, false, true, null), null);
	}

	/**
	 * Method which allows to update a binary resource.
	 * 
	 * <p>
	 * Follow redirections and use a secured connection.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @param representation
	 *            the HTTP representation of the data.
	 * @return the response as a string, null if an error occurs. An empty string is returned if an empty entity is
	 *         returned.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public String post(String servicePath, Representation representation) throws ServerErrorException {
		return proceed(handle(Method.POST, getServerReference(servicePath), representation, null, retry, true, false, true, null), null);
	}

	/**
	 * Method which allows to update a binary resource.
	 * 
	 * <p>
	 * Follow redirections and use a secured connection.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @param parameters
	 *            in a HashTable: key=name of the parameter, value=value of the parameter
	 * @param representation
	 *            the HTTP representation of the data.
	 * @return the response as a string, null if an error occurs. An empty string is returned if an empty entity is
	 *         returned.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client bad request, connection
	 *             problem or a server internal error.
	 */
	public String post(String servicePath, Map<String, Object> parameters, Representation representation) throws ServerErrorException {
		return proceed(handle(Method.POST, getParametersAsQuery(getServerReference(servicePath), parameters), representation, null, retry, true, false, true, null), null);
	}

	/**
	 * Method which allows to update a binary resource.
	 * 
	 * <p>
	 * Follow redirections and use a secured connection.
	 * 
	 * @param serviceReference
	 *            The service reference (may be on other servers).
	 * @param representation
	 * @return
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public String post(Reference serviceReference, Representation representation) throws ServerErrorException {
		return proceed(handle(Method.POST, serviceReference, representation, null, retry, true, false, true, null), null);
	}

	/**
	 * Method which allows to update a resource.
	 * 
	 * <p>
	 * Follow redirections and use a secured connection.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @param parameters
	 *            in a HashTable: key=name of the parameter, value=value of the parameter
	 * @return the response as a string, null if an error occurs. An empty string is returned if an empty entity is
	 *         returned.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public String put(String servicePath, Map<String, Object> parameters) throws ServerErrorException {
		return proceed(handle(Method.PUT, getServerReference(servicePath), getParameterRepresentation(parameters), null, retry, true, false, true, null), null);
	}

	/**
	 * Method which allows to update a resource.
	 * 
	 * <p>
	 * Follow redirections and use a secured connection.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @param body
	 *            The text representation of the resource.
	 * @return
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public String put(String servicePath, String body) throws ServerErrorException {
		return proceed(handle(Method.PUT, getServerReference(servicePath), new StringRepresentation(body), null, retry, true, false, true, null), null);
	}

	/**
	 * Method which allows to update a binary resource.
	 * 
	 * <p>
	 * Follow redirections and use a secured connection.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @param representation
	 *            the HTTP representation of the data.
	 * @return
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public String put(String servicePath, Representation representation) throws ServerErrorException {
		return proceed(handle(Method.PUT, getServerReference(servicePath), representation, null, retry, true, false, true, null), null);
	}

	/**
	 * Method which allows to update a binary resource.
	 * 
	 * <p>
	 * Do <b>not</b> follow redirections but use a secured connection.
	 * 
	 * @param serviceReference
	 * @param representation
	 * @return
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public String put(Reference serviceReference, Representation representation) throws ServerErrorException {
		return proceed(handle(Method.PUT, serviceReference, representation, null, retry, true, false, false, null), null);
	}

	/**
	 * Update a resource that do not need any parameters. This can be a "touch" modification (just change the timestamp)
	 * or an update with parameter code into the URL.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @return the response as a string, null if an error occurs. An empty string is returned if an empty entity is
	 *         returned.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public String put(String servicePath, boolean retries) throws ServerErrorException {
		if (retries) {
			return proceed(handle(Method.PUT, getServerReference(servicePath), null, null, retry, true, false, true, null), null);
		}
		return proceed(handle(Method.PUT, getServerReference(servicePath), null, null, 0, true, false, true, null), null);
	}

	/**
	 * Method which allows to delete a resource.
	 * 
	 * <p>
	 * Follow redirections.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @param parameters
	 *            the parameter to pass to the delete service, can be null.
	 * @return the response as a string, null if an error occurs. An empty string is returned if an empty entity is
	 *         returned.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public String delete(String servicePath, Map<String, Object> parameters) throws ServerErrorException {
		return delete(getServerReference(servicePath),parameters);
	}

	/**
	 * Method which allows to delete a resource.
	 * 
	 * <p>
	 * Follow redirections.
	 * 
	 * @param reference
	 * 			The service references may be hosted onto different server.
	 * @param parameters
	 *            the parameter to pass to the delete service, can be null.
	 * @return the response as a string, null if an error occurs. An empty string is returned if an empty entity is
	 *         returned.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public String delete(Reference reference, Map<String, Object> parameters) throws ServerErrorException {
		if (canParametersAsQuery(reference, parameters)) {
			return proceed(handle(Method.DELETE, reference, null, null, retry, true, false, true, null), null);
		}
		reference.addQueryParameter(BaseResource.QUERY_METHODPROXIED, Method.DELETE.getName());
		return proceed(
				handle(Method.POST, reference, getParameterRepresentation(parameters), null,
						retry, true, false, true, null), null);
	}

	/**
	 * Method which allows to delete a resource. DO FOLLOW REDITECTIONS.
	 * 
	 * @param reference
	 * @throws ServerErrorException
	 * @see {@link #delete(Reference, Map)}
	 */
	public void deleteRaw(Reference reference) throws ServerErrorException {
		handle(Method.DELETE, reference, null, null, retry, false, true, true, null);
	}
	
	/**
	 * Low level interface return a raw response representation.
	 * 
	 * <p>
	 * This service is not intended to be secured.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @param date
	 *            the last version of the object that is already downloaded from the server. Can be null.
	 * @return The Response representation if any.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public Representation getRaw(String servicePath, Date date) throws ServerErrorException {
		return getRaw(getServerReference(servicePath), date, null);
	}

	/**
	 * Low level interface return a raw response representation.
	 * 
	 * <p>
	 * Follow redirections.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @param date
	 * @return The Response representation if any.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public Representation getSecuredRaw(String servicePath, Date date) throws ServerErrorException {
		return getSecuredRaw(servicePath, date, null);
	}
	
	/**
	 * Low level interface return a raw response representation.
	 * 
	 * <p>
	 * Follow redirections.
	 * 
	 * <p>
	 * <b>The returned Representation must read (or exhausted) and released.</b>
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @param date The last modification date of a previously downloaded result. May be null.
	 * @param mediaType the requested MetiaType.
	 * @return The Response representation if any.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public Representation getSecuredRaw(String servicePath, Date date, MediaType mediaType) throws ServerErrorException {
		GregorianCalendar calendar = null;
		if (date != null) {
			calendar = new GregorianCalendar();
			calendar.setTime(date);
		}
		Response response = handle(Method.GET, getServerReference(servicePath), null, calendar, retry, true, true, true, mediaType);
		// Process the response.
		if (response.getStatus().isSuccess() && response.isEntityAvailable()) {
			return response.getEntity();
		}
		return null;
	}

	/**
	 * Low level interface return a raw response representation.
	 * 
	 * <p>
	 * Follow redirections.
	 * 
	 * <p>
	 * <b>The returned Representation must read (or exhausted) and released.</b>
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @param parameters
	 *            The service parameters.
	 * @return The Response representation if any.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client bad request, connection
	 *             problem or a server internal error.
	 */
	public Representation getSecuredRaw(String servicePath, Map<String, Object> parameters) throws ServerErrorException {
		Reference reference = getServerReference(servicePath);
		Response response;
		if (canParametersAsQuery(reference, parameters)) {
			response = handle(Method.GET, reference, null, null, retry, true, true, true, null);
		} else {
			reference.addQueryParameter(BaseResource.QUERY_METHODPROXIED, Method.GET.getName());
			response = handle(Method.POST, reference, getParameterRepresentation(parameters), null, retry, true, true, true, null);
		}
		// Process the response.
		if (response.getStatus().isSuccess() && response.isEntityAvailable()) {
			return response.getEntity();
		}
		return null;
	}

	/**
	 * Low level interface return a raw response representation.
	 * 
	 * <p>
	 * This service target a secured web-service.
	 * 
	 * <p>
	 * Follow redirections.
	 * 
	 * <p>
	 * <b>The returned Representation must be read (or exhausted) and released.</b>
	 * 
	 * @param path
	 *            the service reference
	 * @param parameters
	 *            the service parameters
	 * @param date
	 *            Used to set the last modification flag (in this case a null response state that the client possess a
	 *            most recent version). Null if any version must be returned.
	 * @param mediaType The strictly requested mediaType, or null if none.
	 * @return The Response representation if any.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client bad request, connection
	 *             problem or a server internal error.
	 */
	public Representation getRaw(String path, Map<String, Object> parameters, Date date, MediaType mediaType) throws ServerErrorException {
		GregorianCalendar calendar = null;
		if (date != null) {
			calendar = new GregorianCalendar();
			calendar.setTime(date);
		}
		Reference reference = getServerReference(path);
		Response response;
		if (canParametersAsQuery(reference, parameters)) {
			response = handle(Method.GET, reference, null, calendar, retry, true, true, true, mediaType);
		} else {
			reference.addQueryParameter(BaseResource.QUERY_METHODPROXIED, Method.GET.getName());
			response = handle(Method.POST, reference, getParameterRepresentation(parameters), calendar, retry, true, true, true, mediaType);
		}
		// Process the response.
		if (response.getStatus().isSuccess() && response.isEntityAvailable()) {
			return response.getEntity();
		}
		return null;
	}

	/**
	 * Low level interface return a raw response representation.
	 * 
	 * <p>
	 * This service is not intended to be secured.
	 * 
	 * <p>
	 * Follow redirections.
	 * 
	 * @param reference
	 *            the service reference
	 * @param date
	 *            Used to set the last modification flag (in this case a null response state that the client possess a
	 *            most recent version).
	 * @return The Response representation if any.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public Representation getRaw(Reference reference, Date date) throws ServerErrorException {
		return getRaw(reference, date, null);
	}
	
	/**
	 * Low level interface return a raw response representation.
	 * 
	 * <p>
	 * This service is not intended to be secured.
	 * 
	 * <p>
	 * Follow redirections.
	 * 
	 * <p>
	 * <b>The returned Representation must read (or exhausted) and released.</b>
	 * 
	 * @param reference
	 *            the service reference
	 * @param date
	 *            Used to set the last modification flag (in this case a null response state that the client possess a
	 *            most recent version).
	 * @param mediaType The strictly requested mediaType.
	 * @return The Response representation if any.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public Representation getRaw(Reference reference, Date date, MediaType mediaType) throws ServerErrorException {
		GregorianCalendar calendar = null;
		if (date != null) {
			calendar = new GregorianCalendar();
			calendar.setTime(date);
		}
		Response response = handle(Method.GET, reference, null, calendar, retry, false, true, true, mediaType);
		// Process the response.
		if (response.getStatus().isSuccess() && response.isEntityAvailable()) {
			return response.getEntity();
		}
		return null;
	}

	/**
	 * Call a proxy service, a redirection to another service, and return this redirection reference.
	 * 
	 * @param path
	 *            the redirection service URL.
	 * @return null if this service is not a redirection service.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 * @deprecated use {@link #getRedirection(Method, String)}
	 */
	public Reference getRedirection(String path) throws ServerErrorException {
		return getRedirection(Method.GET, path);
	}

	/**
	 * Call a proxy service, a redirection to another service, and return this redirection reference.
	 * 
	 * @param method the HTTP method used to get the redirection.
	 * @param path
	 *            the redirection service URL.
	 * @return null if this service is not a redirection service.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public Reference getRedirection(Method method, String path) throws ServerErrorException {
		Response response = handle(method, getServerReference(path), null, null, retry, true, true, false, null);
		if (response.getStatus().isRedirection()) {
			Reference ref = response.getLocationRef();
			if (ref.isRelative()) {
				ref.setBaseRef(getServerReference("/")); //$NON-NLS-1$
			}
			if (response.isEntityAvailable()) {
				try {
					response.getEntity().exhaust();
					response.getEntity().release();
				} catch (IOException e) {
					if (activator != null) {
						activator.debug(e);
					}
				}
			}
			return ref;
		} else if (!response.getStatus().isSuccess()) {
			// Error managed by arcad: a specific explanation of the error is sent
			if (response.isEntityAvailable()) {
				// NOTE: It is really important to read the entity body to release the connection.
				// error managed by arcad: a specific explanation of the error is sent
				Representation representation = response.getEntity();
				if (representation != null) {
					try {
						throw new ServerErrorException(response.getStatus().getCode(), representation.getText());
					} catch (IOException e) {
						if (activator != null) {
							activator.debug(e);
						}
					} finally {
						representation.release();
					}
				}
			}
			// Error managed by restlet
			throw new ServerErrorException(response.getStatus().getCode(), //
					new ErrorMessageBean(response.getStatus().getDescription()));
		}
		if (response.isEntityAvailable()) {
			// NOTE: It is really important to read the entity body to release the connection.
			try {
				response.getEntity().exhaust();
				response.getEntity().release();
			} catch (IOException e) {
				if (activator != null) {
					activator.debug(e);
				}
			}
		}
		return null;
	}

	/**
	 * Touch a server resource to know if it has been changed since <code>date</code>.
	 * 
	 * <p>
	 * If the resource has not be changed then the method return true. In this case loading the resource is not useful.
	 * 
	 * <p>
	 * Follow service redirections.
	 * 
	 * @param servicePath
	 *            the server relative web-resource path.
	 * @param date
	 *            The current version of the local representation.
	 * @return True is the client date is most recent than the server date.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public boolean head(String servicePath, Date date) throws ServerErrorException {
		return head(getServerReference(servicePath), date);
	}

	/**
	 * Touch a server resource to know if it has been changed since <code>date</code>.
	 * 
	 * <p>
	 * If the resource has not be changed then the method return true. In this case loading the resource is not useful.
	 * 
	 * <p>
	 * Follow service redirections.
	 * 
	 * @param reference
	 *            the service reference (may be on other server).
	 * @param date
	 *            The current version of the local representation.
	 * @return True is the client date is most recent than the server date.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public boolean head(Reference reference, Date date) throws ServerErrorException {
		if ((date == null) || (date.getTime() == 0)) {
			return false;
		}
		Request request = null;
		Response response = null;
		long t = 0;
		int r = retry;
		while (true) {
			try {
				t = System.currentTimeMillis();
				response = null;
				request = new Request(Method.HEAD, reference);
				request.getConditions().setModifiedSince(date);
				prepareClientInfo(request.getClientInfo());
				// Ask to the client connector to handle the call
				response = getClient().handle(preprocess(request));
				// process the final response.
				if ((response == null) || (response.getStatus() == null)) {
					diagnose(request, response, System.currentTimeMillis() - t, r, null);
					if (r <= 0) {
						return false;
					}
				} else if (response.getStatus().getCode() == Status.REDIRECTION_NOT_MODIFIED.getCode()) {
					return true;
				} else if (response.getStatus().isRedirection()) {
					Reference ref = response.getLocationRef();
					if (ref.isRelative()) {
						ref.setBaseRef(getServerReference("/")); //$NON-NLS-1$
					}
					return head(ref, date);
				} else if (response.getStatus().isSuccess()) {
					return false;
				} else {
					diagnose(request, response, System.currentTimeMillis() - t, r, null);
					if (isDefinitive(response, r)) {
						return false;
					}
				}
			} catch (Throwable e) {
				diagnose(request, response, System.currentTimeMillis() - t, r, e);
				if (r <= 0) {
					throw new ServerErrorException(1005, new ErrorMessageBean(new Date(),
							Messages.WebServiceAccess_Error_Connection, e.getLocalizedMessage()));
				}
			}
			r--;
			sleep();
		}
	}

	/**
	 * Touch a server resource to retrieve its last modification date.
	 * 
	 * <p>
	 * Follow service redirections.
	 * 
	 * @param reference
	 *            the service reference (may be on other server).
	 * @return Null if this service do not provide a last-modification date.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public Date head(String servicePath) throws ServerErrorException {
		return head(getServerReference(servicePath));
	}

	/**
	 * Touch a server resource to retrieve its last modification date.
	 * 
	 * <p>
	 * Follow service redirections.
	 * 
	 * @param reference
	 *            the service reference (may be on other server).
	 * @return Null if this service do not provide a last-modification date.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	public Date head(Reference reference) throws ServerErrorException {
		Request request = null;
		Response response = null;
		long t = 0;
		int r = retry;
		while (true) {
			try {
				t = System.currentTimeMillis();
				response = null;
				request = new Request(Method.HEAD, reference);
				prepareClientInfo(request.getClientInfo());
				// Ask to the client connector to handle the call
				response = getClient().handle(preprocess(request));
				// process the final response.
				if ((response == null) || (response.getStatus() == null)) {
					diagnose(request, response, System.currentTimeMillis() - t, r, null);
					if (r <= 0) {
						if ((response != null) && (response.getEntity() != null)) {
							return response.getEntity().getModificationDate();
						}
						return null;
					}
				} else if (response.getStatus().isRedirection()) {
					Reference ref = response.getLocationRef();
					if (ref.isRelative()) {
						ref.setBaseRef(getServerReference("/")); //$NON-NLS-1$
					}
					return head(ref);
				} else if (!response.getStatus().isError()) {
					if ((response != null) && (response.getEntity() != null)) {
						return response.getEntity().getModificationDate();
					}
					return null;
				} else {
					diagnose(request, response, System.currentTimeMillis() - t, r, null);
					if (isDefinitive(response, r)) {
						return null;
					}
				}
			} catch (Throwable e) {
				diagnose(request, response, System.currentTimeMillis() - t, r, e);
				if (r <= 0) {
					throw new ServerErrorException(1005, new ErrorMessageBean(new Date(), Messages.WebServiceAccess_Error_Connection, e.getLocalizedMessage()));
				}
			}
			r--;
			sleep();
		}
	}

	/**
	 * Return a Form that content the parameters values.
	 * 
	 * <ul>
	 * <li>Dates are represented into an ISO string format.
	 * <li>Complex java object are serialized with XStream.
	 * </ul>
	 * 
	 * @param parameters
	 * @return
	 */
	public Form getParameterForm(Map<String, Object> parameters) {
		Form form = new Form();
		if ((parameters == null) || (parameters.size() == 0)) {
			return form;
		}
		// Construct the Form object.
		for (Entry<String, Object> entry : parameters.entrySet()) {
			Object o = entry.getValue();
			if (o == null) {
				form.add(entry.getKey(), ""); //$NON-NLS-1$
			} else if (o instanceof String) {
				form.add(entry.getKey(), (String) o);
			} else if ((o instanceof Integer) || //
					(o instanceof Boolean) || //
					(o instanceof Float) || //
					(o instanceof Double)) {
				form.add(entry.getKey(), o.toString());
			} else if (o instanceof Date) {
				form.add(entry.getKey(), ISODateFormater.toString((Date) o));
			} else if (o instanceof Calendar) {
				form.add(entry.getKey(), ISODateFormater.toString((Calendar) o));
			} else {
				if (xs == null) {
					synchronized (this) {
						if (xs == null) {
							xs = new XStreamCompact(getClass().getClassLoader());
						}
					}
				}
				form.add(entry.getKey(), xs.toXML(o));
			}
		}
		return form;
	}

	/**
	 * Proceed to the request on the given reference (this be on another server).
	 * 
	 * @param method
	 *            The HTTP method to use.
	 * @param reference
	 *            the reference of the resource to call.
	 * @param body
	 *            the request body.
	 * @param calendar
	 *            the date if the last version obtained from the server, can be null if ignored.
	 * @param retries
	 *            number of retries if request failed (not permanently)
	 * @param secure
	 *            true if a secured connection is needed.
	 * @param raw
	 *            true if any type of response (raw data) is allowed.
	 * @param follow
	 *            true if redirection may be followed.
	 * @return the HTTP response.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	protected Response handle(Method method, Reference reference, Representation body, Calendar calendar, int retries,
			boolean secure, boolean raw, boolean follow, MediaType mediaType) throws ServerErrorException {
		Request request;
		// Support a general "method proxying" used to pass local HTTP restrictions.
		if (METHODPROXY && (!Method.GET.equals(method)) && (!Method.POST.equals(method))) {
			reference.addQueryParameter(BaseResource.QUERY_METHODPROXIED, method.getName());
			// Restlet/Jetty limitation that does not allow to GET request to have a BODY !!!
			request = new Request(Method.POST, reference);
		} else {
			request = new Request(method, reference);
		}
		if (parameters.getProxyLogin() != null) {
			request.setProxyChallengeResponse(new ChallengeResponse(ChallengeScheme.HTTP_BASIC, parameters.getProxyLogin(), parameters.getProxyPassword()));
		}
		if (raw) {
			// Here we accept any representation ...
			request.getClientInfo().getAcceptedMediaTypes().add(PREFERENCE_ALLMEDIATYPES);
			// And any characterset...
			request.getClientInfo().getAcceptedCharacterSets().add(PREFERENCE_ALLCHARSETS);
		}
		// ... but "prepareClientInfo" will add a XML preferred representation.
		prepareClientInfo(request.getClientInfo());
		if (mediaType != null) {
			// Reset any accepted MediaType...
			request.getClientInfo().getAcceptedMediaTypes().clear();
			request.getClientInfo().getAcceptedMediaTypes().add(new Preference<MediaType>(mediaType));
		}
		if (calendar != null) {
			request.getConditions().setModifiedSince(calendar.getTime());
		}
		File file = null;
		if (body != null) {
			request.setEntity(body);
			if (body instanceof FileRepresentation) {
				// We have to store the file in case of redirection to reset the FileRepresentation properly.
				// After it has been exhausted during the first call.
				file = ((FileRepresentation) body).getFile();
			}
		} else if ((!ALLOWEMPTYPOSTS) && (Method.POST.equals(method) || Method.PUT.equals(method) || Method.DELETE.equals(method) || Method.OPTIONS.equals(method))) {
			request.setEntity(EMPTYPARAM);
		}
		if (secure) {
			request = preprocess(request);
		}
		Response response = null;
		long t = 0;
		while (true) {
			try {
				t = System.currentTimeMillis();
				// Ask to the client connector to handle the call
				response = getClient().handle(request);
				// Empty response (Blindage!)
				if (response == null) {
					throw new ServerErrorException(1005, new ErrorMessageBean(new Date(),
							Messages.WebServiceAccess_Error_Connection, null));
				}
				// Error treatment.
				if (response.getStatus().isError()) {
					// Error managed by arcad.
					if (response.isEntityAvailable()) {
						// NOTE: It is really important to read the entity body to release the connection.
						// error managed by arcad: a specific explanation of the error is sent
						Representation representation = response.getEntity();
						if (representation != null) {
							try {
								throw new ServerErrorException(response.getStatus().getCode(), representation.getText());
								// Do not release the representation yet (diagnose need it...)
							} catch (IOException e) {
								if (activator != null) {
									activator.debug(e);
								}
							}
						}
					}
					// Error managed by restlet (or any HTTP server).
					throw new ServerErrorException(response.getStatus().getCode(), //
							new ErrorMessageBean(response.getStatus().getDescription()));
				}
				if (secure) {
					response = postprocess(response);
				}
				// Redirection process.
				if (follow && response.getStatus().isRedirection()) {
					// resource not changed (according to the given Calendar).
					if (Status.REDIRECTION_NOT_MODIFIED.equals(response.getStatus())) {
						return response;
					}
					Reference ref = response.getLocationRef();
					if (ref.isRelative()) {
						ref.setBaseRef(getServerReference("/")); //$NON-NLS-1$
					}
					if (response.isEntityAvailable()) {
						// NOTE: It is really important to read the entity body to release the connection.
						Representation representation = response.getEntity();
						if (representation != null) {
							representation.exhaust();
							representation.release();
						}
					}
					if (file != null) {
						final String dispositionType = body.getDisposition().getType();
						body = new FileRepresentation(file, body.getMediaType());
						body.getDisposition().setType(dispositionType);
					}
					return handle(method, ref, body, calendar, retries, secure, raw, true, mediaType);
				}
				return response;
			} catch (ServerErrorException e) {
				diagnose(request, response, System.currentTimeMillis() - t, retries, null);
				try {
					if (isDefinitive(response, retries)) {
						throw e;
					}
				} finally {
					if (response.isEntityAvailable()) {
						try {
							response.getEntity().exhaust();
						} catch (IOException e1) {
							if (activator != null) {
								activator.debug(e1);
							}
						}
						response.getEntity().release();
					}
				}
			} catch (Throwable e) {
				diagnose(request, response, System.currentTimeMillis() - t, retries, e);
				try {
					if (retries <= 0) {
						throw new ServerErrorException(1005, new ErrorMessageBean(new Date(),
								Messages.WebServiceAccess_Error_Connection, e.getLocalizedMessage()));
					}
				} finally {
					if ((response != null) && response.isEntityAvailable()) {
						try {
							response.getEntity().exhaust();
						} catch (IOException e1) {
							if (activator != null) {
								activator.debug(e1);
							}
						}
						response.getEntity().release();
					}
				}
			}
			// Invariant: r > 0
			retries--;
			sleep();
		}
	}

	private boolean isDefinitive(Response response, int r) {
		return (r <= 0) || //
				((response != null) && //
						(response.getStatus() != null) && //
				// 3XX responses.
				(response.getStatus().isRedirection() ||
				// 405 is given when user do not have the prilivege to access to the method.
						(response.getStatus().getCode() == Status.CLIENT_ERROR_METHOD_NOT_ALLOWED.getCode()) || //
				// 403 is given when user do not have the right to connect to the server.
				(response.getStatus().getCode() == Status.CLIENT_ERROR_FORBIDDEN.getCode()) ||
				// Any Server Error...
				(response.getStatus().getCode() >= Status.SERVER_ERROR_INTERNAL.getCode())));
	}

	/**
	 * Prepare the Client information with all needed parameters.
	 * 
	 * @param clientInfo
	 *            the client information to fill.
	 */
	protected void prepareClientInfo(ClientInfo clientInfo) {
		if (json) {
			clientInfo.getAcceptedMediaTypes().add(MEDIATYPE_JSON);
		} else {
			clientInfo.getAcceptedMediaTypes().add(MEDIATYPE_XML);
		}
		clientInfo.getAcceptedLanguages().add(PREFERENCE_LANGUAGE);
		clientInfo.getAcceptedLanguages().add(PREFERENCE_LANGUAGE_GENERAL);
		clientInfo.getAcceptedCharacterSets().add(PREFERENCE_CHARSET);
	}

	/**
	 * Perform a Response treatment and return a String representation of it.
	 * 
	 * @param response
	 *            The server HTTP response to proceed.
	 * @param calendar
	 *            If not null, will be set to the server representation last modification change.
	 * @return the response as a string. An empty string is returned if an empty entity is returned.
	 * @throws ServerErrorException
	 *             if the server return an error. This error can be due to a client malformed request, connection
	 *             problem or a server internal error.
	 */
	protected String proceed(Response response, Calendar calendar) throws ServerErrorException {
		// process the final response.
		if (response.getStatus().isSuccess()) {
			if (response.isEntityAvailable()) {
				Representation representation = response.getEntity();
				if (representation != null) {
					try {
						Date date = representation.getModificationDate();
						if ((calendar != null) && (date != null)) {
							calendar.setTime(date);
						}
						return representation.getText();
					} catch (IOException e) {
						if (activator != null) {
							activator.debug(e);
						}
					} finally {
						try {
							representation.exhaust();
						} catch (IOException e) {
							if (activator != null) {
								activator.debug(e);
							}
						}
						representation.release();
					}
				}
			}
			// In case of success the result must be not null.
			return ""; //$NON-NLS-1$
		}
		// Process the redirection status
		if (response.getStatus().isRedirection()) {
			// We return the redirection URL, but we should, automatically, recall the server
			// to the correct address to get a result...
			return response.getLocationRef().toString();
			// [ML] We must keep it like this because of the client cache !
		}
		// Blindage...
		// error sent back the server
		if (response.isEntityAvailable()) {
			// NOTE: It is really important to read the entity body to release the connection.
			// error managed by arcad: a specific explanation of the error is sent
			Representation representation = response.getEntity();
			if (representation != null) {
				try {
					Date date = representation.getModificationDate();
					if ((calendar != null) && (date != null)) {
						calendar.setTime(date);
					}
					throw new ServerErrorException(response.getStatus().getCode(), representation.getText());
				} catch (IOException e) {
					if (activator != null) {
						activator.debug(e);
					}
				} finally {
					try {
						representation.exhaust();
					} catch (IOException e) {
						if (activator != null) {
							activator.debug(e);
						}
					}
					representation.release();
				}
			}
		}
		// Error managed by restlet
		throw new ServerErrorException(response.getStatus().getCode(), //
				new ErrorMessageBean(response.getStatus().getDescription()));
	}

	/**
	 * Log error diagnostic.
	 * 
	 * @param request
	 *            Request send to the server, can be null.
	 * @param response
	 *            Response from the server, can be null.
	 * @param duration
	 *            operation duration.
	 * @param trial
	 *            trial number.
	 * @param e
	 *            Exception thrown, can be null.
	 */
	protected void diagnose(Request request, Response response, long duration, int trial, Throwable e) {
		StringBuilder message = new StringBuilder();
		message.append(Messages.WebServiceAccess_Diag_Title);
		message.append(new Date().toString());
		message.append(Messages.WebServiceAccess_Diag_duration);
		message.append(duration);
		message.append(Messages.WebServiceAccess_Diag_trial);
		message.append(trial);
		if ((response != null) && (response.getStatus() != null)) {
			// Ignore 304 "errors".
			if (response.getStatus().getCode() == Status.REDIRECTION_NOT_MODIFIED.getCode()) {
				return;
			}
		}
		message.append(Messages.WebServiceAccess_Diag_serveraddress);
		message.append(address);
		message.append(Messages.WebServiceAccess_Diag_Login);
		message.append(login);
		if (request == null) {
			message.append(Messages.WebServiceAccess_Diag_norequest);
		} else {
			if (request.getResourceRef() == null) {
				message.append(Messages.WebServiceAccess_Diag_noref);
			} else {
				message.append(Messages.WebServiceAccess_Diag_ref);
				message.append(request.getResourceRef().toString());
			}
			if (request.getMethod() != null) {
				message.append(Messages.WebServiceAccess_Diag_method);
				message.append(request.getMethod().toString());
			}
			if (request.getAttributes() == null) {
				message.append(Messages.WebServiceAccess_Diag_noattributes);
			} else {
				message.append(Messages.WebServiceAccess_Diag_attributes);
				message.append(request.getAttributes().toString());
			}
			if (request.getClientInfo() == null) {
				message.append(Messages.WebServiceAccess_Diag_noclient);
			} else {
				message.append(Messages.WebServiceAccess_Diag_clientaddress);
				message.append(request.getClientInfo().getAddress());
				message.append(Messages.WebServiceAccess_Diag_clientport);
				message.append(request.getClientInfo().getPort());
				message.append(Messages.WebServiceAccess_Diag_clientagent);
				message.append(request.getClientInfo().getAgent());
				message.append(Messages.WebServiceAccess_Diag_clientname);
				message.append(request.getClientInfo().getAgentName());
				message.append(Messages.WebServiceAccess_Diag_clientversion);
				message.append(request.getClientInfo().getAgentVersion());
				if (request.getClientInfo().getMainAgentProduct() != null) {
					message.append(Messages.WebServiceAccess_Diag_clientproduct);
					message.append(request.getClientInfo().getMainAgentProduct().getName());
					message.append(' ');
					message.append(request.getClientInfo().getMainAgentProduct().getVersion());
				}
				message.append(Messages.WebServiceAccess_Diag_clientlanguages);
				message.append(request.getClientInfo().getAcceptedLanguages());
				message.append(Messages.WebServiceAccess_Diag_clientmedia);
				message.append(request.getClientInfo().getAcceptedMediaTypes());
				message.append(Messages.WebServiceAccess_Diag_clientcharset);
				message.append(request.getClientInfo().getAcceptedCharacterSets());
				message.append(Messages.WebServiceAccess_Diag_clientencodings);
				message.append(request.getClientInfo().getAcceptedEncodings());
				if (request.isEntityAvailable()) {
					if (request.getEntity() == null) {
						message.append(Messages.WebServiceAccess_Diag_nullentity);
					} else {
						message.append(Messages.WebServiceAccess_Diag_entity);
						try {
							message.append(request.getEntity().getText());
						} catch (IOException eio) {
							message.append(Messages.WebServiceAccess_Diag_error);
							message.append(eio.getLocalizedMessage());
						}
					}
				} else {
					message.append(Messages.WebServiceAccess_Diag_noentity);
				}
			}
		}
		if (response == null) {
			message.append(Messages.WebServiceAccess_Diag_noresponse);
		} else {
			if (response.getStatus() == null) {
				message.append(Messages.WebServiceAccess_Diag_nostatus);
			} else {
				message.append(Messages.WebServiceAccess_Diag_statuscode);
				message.append(response.getStatus().getCode());
				message.append(Messages.WebServiceAccess_Diag_statusname);
				message.append(response.getStatus().getReasonPhrase());
				message.append(Messages.WebServiceAccess_Diag_status);
				message.append(response.getStatus().getDescription());
				message.append(Messages.WebServiceAccess_Diag_statusuri);
				message.append(response.getStatus().getUri());
				if (response.getStatus().getThrowable() != null) {
					message.append(Messages.WebServiceAccess_Diag_statusexp);
					try (StringWriter sw = new StringWriter()) {
						try (PrintWriter pw = new PrintWriter(sw)) {
							response.getStatus().getThrowable().printStackTrace(pw);
						}
						message.append(sw);
					} catch (IOException ee) {
						// Nothing to do here.
					}
				}
				if (response.getStatus().isRedirection()) {
					message.append(Messages.WebServiceAccess_Diag_redirection);
					if (response.getLocationRef() == null) {
						message.append(Messages.WebServiceAccess_Diag_null);
					} else {
						message.append(response.getLocationRef().toString());
					}
				}
			}
			if (response.isEntityAvailable()) {
				if (response.getEntity() == null) {
					message.append(Messages.WebServiceAccess_Diag_nullentity);
				} else {
					message.append(Messages.WebServiceAccess_Diag_entity);
					try {
						message.append(response.getEntity().getText());
					} catch (IOException eio) {
						message.append(Messages.WebServiceAccess_Diag_error);
						message.append(eio.getLocalizedMessage());
					}
				}
			} else {
				message.append(Messages.WebServiceAccess_Diag_noentity);
			}
			if (response.getServerInfo() == null) {
				message.append(Messages.WebServiceAccess_Diag_noserverinfo);
			} else {
				message.append(Messages.WebServiceAccess_Diag_serveraddress);
				message.append(response.getServerInfo().getAddress());
				message.append(Messages.WebServiceAccess_Diag_serverport);
				message.append(response.getServerInfo().getPort());
				message.append(Messages.WebServiceAccess_Diag_serveragent);
				message.append(response.getServerInfo().getAgent());
			}
		}
		if (e != null) {
			message.append(Messages.WebServiceAccess_Diag_error);
			message.append(e.getLocalizedMessage());
		}
		if (parameters.getProxyHost() == null) {
			message.append(Messages.WebServiceAccess_Diag_noproxyrecorded);
		} else {
			message.append(Messages.WebServiceAccess_Diag_proxyhostr);
			message.append(parameters.getProxyHost());
			message.append(Messages.WebServiceAccess_Diag_prosyportr);
			message.append(parameters.getProxyPort());
			message.append(Messages.WebServiceAccess_Diag_proxylogingr);
			message.append(parameters.getProxyLogin());
		}
		String s = System.getProperty("http.proxyHost"); //$NON-NLS-1$
		String p = System.getProperty("http.proxyPort"); //$NON-NLS-1$
		if ((s == null) || (s.length() == 0)) {
			s = System.getProperty("https.proxyHost"); //$NON-NLS-1$
			p = System.getProperty("https.proxyPort"); //$NON-NLS-1$
		}		
		if ((s == null) || (s.length() == 0)) {
			s = parameters.getProxyHost();
			p = Integer.toString(parameters.getProxyPort());
		}
		if ((s == null) || (s.length() == 0)) {
			message.append(Messages.WebServiceAccess_Diag_noproxyactive);
		} else {
			message.append(Messages.WebServiceAccess_Diag_proxyhosta);
			message.append(s);
			message.append(Messages.WebServiceAccess_Diag_proxyporta);
			message.append(p);
			if (parameters.getProxyLogin() == null) {
				message.append(Messages.WebServiceAccess_Diag_proxynoauth);
			} else {
				message.append(Messages.WebServiceAccess_Diag_proxylogina);
				message.append(parameters.getProxyLogin());
			}
		}
		if (activator != null) {
			activator.info(message.toString(), e);
		} else {
			// When used outside of OSGi.
			System.err.println(message.toString());
			if (e != null) {
				e.printStackTrace();
			}
		}
	}
}