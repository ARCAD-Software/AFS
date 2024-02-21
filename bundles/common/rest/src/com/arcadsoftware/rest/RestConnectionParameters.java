/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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
import java.io.Serializable;
import java.security.KeyStore;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLSession;

import org.restlet.Client;
import org.restlet.data.Parameter;
import org.restlet.util.Series;

import com.arcadsoftware.osgi.ILoggedPlugin;
import com.arcadsoftware.rest.internal.Messages;

/**
 * This class may be used to configure the WebServiceAccess Objects.
 * 
 * @author ARCAD Software
 */
public class RestConnectionParameters implements Cloneable, Serializable {

	private static final long serialVersionUID = 3L;
	
	private final ILoggedPlugin activator;
	private final HashMap<String, String> parameters;
	private boolean ignoreHostName;
	private String proxyhost;
	private int proxyport;
	private String proxylogin;
	private char[] proxypwd;
	
	/**
	 * Create a parameter set from a properties object.
	 * 
	 * @param properties
	 */
	public RestConnectionParameters(ILoggedPlugin activator, Map<String, Object> properties) {
		this(activator);
		Object o = properties.get("ignoreHostName"); //$NON-NLS-1$
		ignoreHostName = (o != null) && "true".equalsIgnoreCase(o.toString()); //$NON-NLS-1$
		proxyhost = (String) properties.get("proxyHost"); //$NON-NLS-1$
		o = properties.get("proxyPort"); //$NON-NLS-1$
		if (o != null) {
			try {
				proxyport = Integer.parseInt(o.toString());
			} catch (NumberFormatException e) {}
		}
		proxylogin = (String) properties.get("proxyLogin"); //$NON-NLS-1$
		o = properties.get("proxyPassword"); //$NON-NLS-1$
		if (o != null) {
			proxypwd = o.toString().toCharArray();
		}
		for (Entry<String, Object> p: properties.entrySet()) {
			if (!"ignoreHostName".equalsIgnoreCase(p.getKey()) && (p.getValue() != null)) {
				parameters.put(p.getKey(), p.getValue().toString());
			}
		}
	}
	
	/**
	 * Create an empty preconfigured parameter set with IBMi default parameters if an IBMi JVM is detected
	 */
	public RestConnectionParameters(ILoggedPlugin activator) {
		super();
		this.activator = activator;
		parameters = new HashMap<String, String>();
		if (isIBMJVM()) {
			setDefaultIBMJVMParameters();
		}
	}

	private boolean isIBMJVM() {
		String vendor = System.getProperty("java.vendor"); //$NON-NLS-1$
		return (vendor != null) && vendor.startsWith("IBM"); //$NON-NLS-1$
	}
	
	private void setDefaultIBMJVMParameters() {
		// Set mandatory hardcoded parameters for connection with a JVM in an IBMi environment
		setKeyManagerAlgorithm(System.getProperty("ssl.KeyManagerFactory.algorithm", "PKIX")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		setTrustManagerAlgorithm(System.getProperty("ssl.TrustManagerFactory.algorithm", "PKIX"));
	}

	@Override
	protected RestConnectionParameters clone() {
		RestConnectionParameters result = new RestConnectionParameters(activator);
		result.parameters.putAll(parameters);
		result.ignoreHostName = ignoreHostName;
		result.proxyhost = proxyhost;
		result.proxylogin = proxylogin;
		result.proxyport = proxyport;
		if (proxypwd != null) {
			result.proxypwd = Arrays.copyOf(proxypwd, proxypwd.length);
		}
		return result;
	}
	
	/**
	 * Adds or replaces the disabled cipher suites in the Rest Connection parameters.
	 * @param disabledCiphers the whitespace-separated list of disabled cipher suites
	 */
	public void setDisabledCipherSuites(String disabledCipherSuites) {
		if (disabledCipherSuites != null && disabledCipherSuites.length() > 0) {
			parameters.put("disabledCipherSuites", disabledCipherSuites);
		}
	}
	
	/**
	 * Adds or replaces the disabled SSL/TLS protocol names in the Rest Connection parameters.
	 * @param disabledProtocols the whitespace-separated list of disabled SSL protocols
	 */
	public void setDisabledProtocols(String disabledProtocols) {
		if (disabledProtocols != null && disabledProtocols.length() > 0) {
			parameters.put("disabledProtocols", disabledProtocols);
		}
	}
	
	/**
	 * Adds or replaces the enabled cipher suites in the Rest Connection parameters.
	 * @param enabledCiphers the whitespace-separated list of enabled cipher suites
	 */
	public void setEnabledCipherSuites(String enabledCipherSuites) {
		if (enabledCipherSuites != null && enabledCipherSuites.length() > 0) {
			parameters.put("enabledCipherSuites", enabledCipherSuites);
		}
	}
	
	/**
	 * Adds or replaces the enabled SSL/TLS protocol names in the Rest Connection parameters.
	 * @param enabledProtocols the whitespace-separated list of enabled SSL protocols
	 */
	public void setEnabledProtocols(String enabledProtocols) {
		if (enabledProtocols != null && enabledProtocols.length() > 0) {
			parameters.put("enabledProtocols", enabledProtocols);
		}
	}
	
	/**
	 * Adds or replaces the secure socket protocol name in the Rest Connection parameters.
	 * @param protocol the name of the secure socket protocol
	 */
	public void setProtocol(String protocol) {
		if (protocol != null && protocol.length() > 0) {
			parameters.put("protocol", protocol);
		}
	}
	
	/**
	 * Adds or replaces the name of the SecureRandom algorithm in the Rest Connection parameters.
	 * @param randomAlgorithm the name of the SecureRandom algorithm
	 */
	public void setSecureRandomAlgorithm(String randomAlgorithm) {
		if (randomAlgorithm != null && randomAlgorithm.length() > 0) {
			parameters.put("randomAlgorithm", randomAlgorithm);
		}
	}
	
	/**
	 * Adds or replaces the certificate algorithm for the trust manager in the Rest Connection parameters.
	 * @param trustManagerAlgorithm the trust manager algorithm
	 */
	public void setTrustManagerAlgorithm(String trustManagerAlgorithm) {
		if (trustManagerAlgorithm != null && trustManagerAlgorithm.length() > 0) {
			parameters.put("trustManagerAlgorithm", trustManagerAlgorithm);
		}
	}
	
	/**
	 * Adds or replaces the certificate algorithm for the key manager in the Rest Connection parameters.
	 * @param keyManagerAlgorithm the key manager algorithm
	 */
	public void setKeyManagerAlgorithm(String keyManagerAlgorithm) {
		if (keyManagerAlgorithm != null && keyManagerAlgorithm.length() > 0) {
			parameters.put("keyManagerAlgorithm", keyManagerAlgorithm);
		}
	}

	/**
	 * Set TLS Server parameters.
	 * 
	 * @param trustStore
	 * @param trustStorePassword
	 * @param storeType
	 * @param trustManagerAlgorithm
	 */
	public void setTrustStore(File trustStore, char[] trustStorePassword, String storeType, String trustManagerAlgorithm) {
		if ((trustStore != null) && trustStore.isFile()) {
			parameters.put("truststorePath", trustStore.getAbsolutePath()); //$NON-NLS-1$
			if (trustStorePassword != null) {
				parameters.put("truststorePassword", new String(trustStorePassword)); //$NON-NLS-1$
			}
			if (storeType == null) {
				storeType = KeyStore.getDefaultType();
			}
			if (trustManagerAlgorithm != null) {
				setTrustManagerAlgorithm(trustManagerAlgorithm);
			}
			parameters.put("truststoreType", storeType); //$NON-NLS-1$
			parameters.put("sslContextFactory", "org.restlet.engine.ssl.DefaultSslContextFactory"); //$NON-NLS-1$ //$NON-NLS-2$
		} else {
			parameters.remove("truststorePath"); //$NON-NLS-1$
			parameters.remove("truststorePassword"); //$NON-NLS-1$
			parameters.remove("truststoreType"); //$NON-NLS-1$
			parameters.remove("trustManagerAlgorithm"); //$NON-NLS-1$
			if (!parameters.containsKey("keystorePath")) { //$NON-NLS-1$
				parameters.remove("sslContextFactory"); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
	}

	/**
	 * Set TLS Server parameters.
	 * 
	 * @param trustStore
	 * @param trustStorePassword
	 * @param storeType
	 */
	public void setTrustStore(File trustStore, char[] trustStorePassword, String storeType) {
		setTrustStore(trustStore, trustStorePassword, storeType, null);
	}
	
	/**
	 * Set TLS Server parameters.
	 * 
	 * @param trustStore
	 * @param trustStorePassword
	 */
	public void setTrustStore(File trustStore, char[] trustStorePassword) {
		setTrustStore(trustStore, trustStorePassword, null);
	}
	
	/**
	 * Set TLS Server parameters.
	 * 
	 * @param trustStorePath
	 * @param trustStorePassword
	 * @param storeType
	 * @param trustManagerAlgorithm
	 */
	public void setTrustStore(String trustStorePath, char[] trustStorePassword, String storeType, String trustManagerAlgorithm) {
		if (trustStorePath != null) {
			setTrustStore(new File(trustStorePath), trustStorePassword, storeType, trustManagerAlgorithm);
		}
	}
	
	/**
	 * Set TLS Client security parameters.
	 * 
	 * @param keyStore
	 * @param keyStorePassword
	 * @param keyPassword
	 * @param storeType
	 * @param keyManagerAlgorithm
	 */
	public void setKeyStore(File keyStore, char[] keyStorePassword, char[] keyPassword, String storeType, String keyManagerAlgorithm) {
		if ((keyStore != null) && keyStore.isFile()) {
			parameters.put("keystorePath", keyStore.getAbsolutePath()); //$NON-NLS-1$
			if (keyStorePassword != null) {
				parameters.put("keystorePassword", new String(keyStorePassword)); //$NON-NLS-1$
			}
			if (keyPassword == null) {
				keyPassword = keyStorePassword;
			}
			if (keyPassword != null) {
				parameters.put("keyPassword", new String(keyPassword)); //$NON-NLS-1$
			}
			if (storeType == null) {
				storeType = KeyStore.getDefaultType();
			}
			if (keyManagerAlgorithm != null) {
				setKeyManagerAlgorithm(keyManagerAlgorithm);
			}
			parameters.put("keystoreType", storeType); //$NON-NLS-1$
			parameters.put("needClientAuthentication", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			parameters.put("sslContextFactory", "org.restlet.engine.ssl.DefaultSslContextFactory"); //$NON-NLS-1$ //$NON-NLS-2$
		} else {
			parameters.remove("keystorePath"); //$NON-NLS-1$
			parameters.remove("keystorePassword"); //$NON-NLS-1$
			parameters.remove("keyPassword"); //$NON-NLS-1$
			parameters.remove("keystoreType"); //$NON-NLS-1$
			parameters.remove("keyManagerAlgorithm"); //$NON-NLS-1$
			parameters.remove("needClientAuthentication"); //$NON-NLS-1$ //$NON-NLS-2$
			if (!parameters.containsKey("truststorePath")) { //$NON-NLS-1$
				parameters.remove("sslContextFactory"); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
	}
	
	/**
	 * Set TLS Client security parameters.
	 * 
	 * @param keyStorePath
	 * @param keyStorePassword
	 * @param keyPassword
	 * @param storeType
	 * @param trustManagerAlgorithm
	 */
	public void setKeyStore(String keyStorePath, char[] keyStorePassword, char[] keyPassword, String storeType, String keyManagerAlgorithm) {
		if (keyStorePath != null) {
			setKeyStore(new File(keyStorePath), keyStorePassword, keyPassword, storeType, keyManagerAlgorithm);
		}
	}


	/**
	 * Set TLS Client security parameters.
	 * 
	 * @param keyStore
	 * @param storePassword
	 * @param keyPassword
	 */
	public void setKeyStore(File keyStore, char[] storePassword, char[] keyPassword) {
		setKeyStore(keyStore, storePassword, keyPassword, null);
	}
	
	/**
	 * Set TLS Client security parameters.
	 * 
	 * @param keyStore
	 * @param storePassword
	 * @param keyPassword
	 * @param storeType
	 */
	public void setKeyStore(File keyStore, char[] keyStorePassword, char[] keyPassword, String storeType) {
		setKeyStore(keyStore, keyStorePassword, keyPassword, storeType, null);
	}
	
	/**
	 * @return True if the TLS parameters are activated.
	 */
	public boolean isUseTLS() {
		return parameters.containsKey("truststorePath") || //$NON-NLS-1$
				parameters.containsKey("keystorePath"); //$NON-NLS-1$
	}
	
	/**
	 * Set a specific Restlet parameter value...
	 * 
	 * @param key
	 * @param value
	 */
	public void setParameter(String key, Object value) {
		if (value == null) {
			parameters.remove(key);
		} else {
			parameters.put(key, value.toString());
		}
	}

	/**
	 * If true the URL of the server and the certificate hostname will be not tested.
	 * 
	 * <p>
	 * WARNING: Activating this option may lead to a security breach.
	 * 
	 * @return
	 */
	public boolean isIgnoreHostName() {
		return ignoreHostName;
	}

	/**
	 * If true the URL of the server and the certificate hostname will be not tested.
	 * 
	 * <p>
	 * WARNING: Activating this option may lead to a security breach.
	 * 
	 * @param ignoreHostName
	 */
	public void setIgnoreHostName(boolean ignoreHostName) {
		this.ignoreHostName = ignoreHostName;
	}

	/**
	 * Define the HTTP/HTTPS Proxy host name.
	 * 
	 * <p>
	 * WARNING:With current Restlet implementation 2.4.3, this parameter is a JVM global parameter ! You must st the same value for all REST Connections.
	 *   
	 * @return
	 */
	public String getProxyHost() {
		return proxyhost;
	}

	/**
	 * Define the HTTP/HTTPS Proxy host name.
	 * 
	 * <p>
	 * WARNING:With current Restlet implementation 2.4.3, this parameter is a JVM global parameter ! You must st the same value for all REST Connections.
	 * 
	 * @param proxyhost
	 * @param proxyport
	 */
	public void setProxy(String proxyhost, int proxyport) {
		this.proxyhost = proxyhost;
		// Note that these parameters require the NIO Connector !!!
		if (proxyhost != null) {
			parameters.put("proxyHost", proxyhost); //$NON-NLS-1$
			if (proxyport > 0) {
				parameters.put("proxyPort", Integer.toString(proxyport)); //$NON-NLS-1$
				this.proxyport = proxyport;
			} else if (isUseTLS()) {
				parameters.put("proxyPort", "3129"); //$NON-NLS-1$ //$NON-NLS-2$
				this.proxyport = 3129;
			} else {
				parameters.put("proxyPort", "3128"); //$NON-NLS-1$ //$NON-NLS-2$
				this.proxyport = 3128;
			}
		} else {
			parameters.remove("proxyHost"); //$NON-NLS-1$
			parameters.remove("proxyPort"); //$NON-NLS-1$
		}
	}

	/**
	 * Define the HTTP/HTTPS Proxy TCP-IP port number.
	 * 
	 * <p>
	 * WARNING:With current Restlet implementation 2.4.3, this parameter is a JVM global parameter ! You must st the same value for all REST Connections.
	 * 
	 * @return
	 */
	public int getProxyPort() {
		return proxyport;
	}

	/**
	 * Define the Proxy login used by this connection.
	 * 
	 * @return
	 */
	public String getProxyLogin() {
		return proxylogin;
	}

	/**
	 * Define the Proxy login and password used by this connection.
	 * 
	 * @param proxylogin
	 * @param proxypwd
	 */
	public void setProxylogin(String proxylogin, char[] proxypwd) {
		this.proxylogin = proxylogin;
		this.proxypwd = proxypwd;
	}

	/**
	 * Define the Proxy password used by this connection.
	 * 
	 * @return
	 */
	public char[] getProxyPassword() {
		return proxypwd;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1268;
		if (ignoreHostName) {
			result = 1269;
		}
		result = prime * result + parameters.hashCode();
		if (proxyhost == null) {
			result *= prime;
		} else {
			result = prime * result + proxyhost.hashCode();
		}
		if (proxylogin == null) {
			result *= prime;
		} else {
			result = prime * result + proxylogin.hashCode();
		}
		result = prime * result + proxyport;
		result = prime * result + Arrays.hashCode(proxypwd);
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		RestConnectionParameters other = (RestConnectionParameters) obj;
		if (ignoreHostName != other.ignoreHostName) {
			return false;
		}
		if (!parameters.equals(other.parameters)) {
			return false;
		}
		if (proxyhost == null) {
			if (other.proxyhost != null) {
				return false;
			}
		} else if (!proxyhost.equals(other.proxyhost)) {
			return false;
		}
		if (proxylogin == null) {
			if (other.proxylogin != null) {
				return false;
			}
		} else if (!proxylogin.equals(other.proxylogin)) {
			return false;
		}
		return (proxyport == other.proxyport) && Arrays.equals(proxypwd, other.proxypwd);
	}

	@Override
	public String toString() {
		return "RestConnectionParameters (contains sensitive parameters)"; //$NON-NLS-1$
	}

	/**
	 * Initialize a Restlet Client set of parameters.
	 * @param params
	 */
	protected Client init(final Client client) {
		final Series<Parameter> params = client.getContext().getParameters();
		for (Entry<String, String> e: parameters.entrySet()) {
			params.add(e.getKey(), e.getValue());
		}
		// TODO implement a way to automatically import an unvalidated certificate and popup a query to the End-User...
		if (isUseTLS() && ignoreHostName) {
			client.getContext().getAttributes().put("hostnameVerifier", new HostnameVerifier() { //$NON-NLS-1$
				@Override
				public boolean verify(String urlhost, SSLSession session) {
					if (activator != null) {
						activator.debug(String.format(Messages.WebServiceAccess_SSLHostVerifications, urlhost, session.getPeerHost()));
					}
					return true;
				}
			});
		}
		// Proxy !!!
		// FIXME This should be a local parameter !!!
		if (useProxy()) {
			if (isUseTLS()) {
				System.setProperty("https.proxyHost", proxyhost);
				System.setProperty("https.proxyPort", Integer.toString(proxyport));
			} else {
				System.setProperty("http.proxyHost", proxyhost);
				System.setProperty("http.proxyPort", Integer.toString(proxyport));
			}
			// TODO Ajouter les serveur sans proxy dans la propriété By pass !
		}
		return client;
	}

	/**
	 * @return only is a proxy if configured for this set of parameters.
	 */
	public boolean useProxy() {
		return (proxyhost != null) && !proxyhost.isEmpty();
	}
	
}
