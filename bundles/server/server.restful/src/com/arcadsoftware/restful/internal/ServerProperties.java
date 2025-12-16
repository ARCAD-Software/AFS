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
package com.arcadsoftware.restful.internal;

import java.io.File;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.Dictionary;

import org.osgi.framework.Bundle;
import org.osgi.framework.Constants;
import org.restlet.data.Parameter;
import org.restlet.util.Series;

import com.arcadsoftware.crypt.ConfiguredSSLContext;
import com.arcadsoftware.crypt.Crypto;

public class ServerProperties {
	
	protected static final String PROP_DOMAINNAME = "domainname"; //$NON-NLS-1$;
	protected static final String PROP_PORTNUMBER = "port"; //$NON-NLS-1$;
	protected static final String PROP_PORTSSL = "portssl"; //$NON-NLS-1$;
	private static final String PROP_AUTHOR = "author"; //$NON-NLS-1$;
	private static final String PROP_NAME = "name"; //$NON-NLS-1$;
	private static final String PROP_VERSION = "version"; //$NON-NLS-1$;
	private static final String PROP_OWNER = "owner"; //$NON-NLS-1$;
	private static final String PROP_DESCRIPTION = "description"; //$NON-NLS-1$;
	private static final String PROP_KEYPATH = "keystore"; //$NON-NLS-1$;
	protected static final String PROP_KEYSTOREPWD = "keystorepwd"; //$NON-NLS-1$;
	protected static final String PROP_KEYPWD = "keypwd"; //$NON-NLS-1$;
	private static final String PROP_KEYTYPE = "keytype"; //$NON-NLS-1$;
	private static final String PROP_KEYALIAS = "keyalias"; //$NON-NLS-1$;
	private static final String PROP_CLIENTAUTH = "clientauth"; //$NON-NLS-1$;
	private static final String PROP_TRUSTPATH = "truststore"; //$NON-NLS-1$;
	protected static final String PROP_TRUSTSTOREPWD = "truststorepwd"; //$NON-NLS-1$;
	private static final String PROP_TRUSTSTORETYPE = "truststoretype"; //$NON-NLS-1$;
	private static final String PROP_LOGDISABLED = "log.disabled"; //$NON-NLS-1$;
	private static final String PROP_CONTACTEMAIL = "email"; //$NON-NLS-1$
	private static final String PROP_LICENSE_URL = "license.url"; //$NON-NLS-1$
	private static final String PROP_LICENSE_NAME = "license.name"; //$NON-NLS-1$
	private static final String PROP_COPYRIGHT = "copyright"; //$NON-NLS-1$
	private static final String PROP_URL = "website"; //$NON-NLS-1$
	private static final String PROP_INTERFACE = "interface"; //$NON-NLS-1$
	private static final String PROP_SYSTEMINTERFACE = "com.arcadsoftware.rest.interface"; //$NON-NLS-1$
	private static final String PROP_HTTPVERSION = "http.version"; //$NON-NLS-1$
	private static final String PROP_HTTP3PEMWORKDIR = "http3.pem.directory"; //$NON-NLS-1$
	private static final String PROP_HTTPREQUESTHEADERLIMIT = "http.requestheaderlimit"; //$NON-NLS-1$
	private static final String PROP_HTTPRESPONSEHEADERLIMIT = "http.responseheaderlimit"; //$NON-NLS-1$
	private static final String PROP_MINTHREADS = "threadPool.minThreads"; //$NON-NLS-1$
	private static final String PROP_MAXTHREADS = "threadPool.maxThreads"; //$NON-NLS-1$
	private static final String PROP_THREADSPRIORITY = "threadPool.priority"; //$NON-NLS-1$
	private static final String PROP_THREADSIDLETIMEOUT = "threadPool.idleTimeout"; //$NON-NLS-1$
	private static final String PROP_STOPTIMEOUT = "threadPool.stopTimeout"; //$NON-NLS-1$
	private static final String PROP_ACCEPTORS = "connector.acceptors"; //$NON-NLS-1$
	private static final String PROP_SELECTORS = "connector.selectors"; //$NON-NLS-1$
	private static final String PROP_ACCEPTQUEUESIZE = "connector.acceptQueueSize"; //$NON-NLS-1$
	private static final String PROP_CONNECTORIDLETIMEOUT = "connector.idleTimeout"; //$NON-NLS-1$
	private static final String PROP_CONNECTORSTOPTIMEOUT = "connector.stopTimeout"; //$NON-NLS-1$
	private static final String PROP_HTTPHEADERCACHESIZE = "http.headerCacheSize"; //$NON-NLS-1$
	private static final String PROP_HTTPOUTPUTBUFFERSIZE = "http.outputBufferSize"; //$NON-NLS-1$
	private static final String PROP_LOWRESPERIOD = "com.arcadsoftware.lowResource.period"; //$NON-NLS-1$
	private static final String PROP_LOWRESTHREADS = "lowResource.threads"; //$NON-NLS-1$
	private static final String PROP_LOWRESMAXMEMORY = "lowResource.maxMemory"; //$NON-NLS-1$
	private static final String PROP_LOWRESMAXCONNECTIONS = "lowResource.maxConnections"; //$NON-NLS-1$
	private static final String PROP_LOWRESIDLETIMEOUT = "lowResource.idleTimeout"; //$NON-NLS-1$
	private static final String PROP_LOWRESSTOPTIMEOUT = "lowResource.stopTimeout"; //$NON-NLS-1$
	private static final String PROP_MAXCTN = "maxConnections"; //$NON-NLS-1$
	private static final String PROP_MAXCTNIDLETIMEOUT = "maxConnections.idleTimeout"; //$NON-NLS-1$
	private static final String PROP_SHUTDOWNGRACEFULLY = "shutdown.gracefully"; //$NON-NLS-1$
	private static final String PROP_SHUTDOWNTIMEOUT = "shutdown.timeout"; //$NON-NLS-1$
	private static final String PROP_USEFORWARDEDFORHEADER = "useForwardedForHeader"; //$NON-NLS-1$

	private final int port;
	private final int portssl;
	private final String name;
	private final String author;
	private final String version;
	private final String domainname;
	private final String owner;
	private final String description;
	private final String kStore;
	private final char[] kSPwd;
	private final String kSType;
	private final String keyAlias;
	private final char[] keyPwd;
	private final boolean keyClient;
	private final String tStore;
	private final char[] tSPwd;
	private final String tSType;
	private final String disabledCipherSuites;
	private final String disabledProtocols;
	private final String enabledCipherSuites;
	private final String enabledProtocols;
	private final String keyManagerAlgorithm;
	private final String protocol;
	private final String secureRandomAlgorithm;
	private final String trustManagerAlgorithm;
	private final boolean restletLogDisabled;
	private final String email;
	private final String license;
	private final String licenseURL;
	private final String termsOfService;
	private final String webSite;
	private final String ethernetInterface;
	private final int httpVersion;
	private final String http3PEMWorkdir;
	private final int responseHeaderSize;
	private final int requestHeaderSize;
	private final int minThreads;
	private final int maxThreads;
	private final int threadsPriority;
	private final int threadsIdleTimeout;
	private final int threadsStopTimeout;
	private final int selectors;
	private final int acceptors;
	private final int acceptQueueSize;
	private final int connectorIdleTimeout;
	private final int connectorStopTimeout;
	private final int headerCacheSize;
	private final int outputBufferSize;
	private final int lrPeriod;
	private final boolean lrThreads;
	private final int lrMaxMemory;
	private final int lrMaxConnections;
	private final int lrIdleTimeout;
	private final int lrStopTimeOut;
	private final int maxCtn;
	private final int maxCtnIdleTimeOut;
	private final boolean sdGraceFully;
	private final int sdTimeout;
	private final boolean useForwardedForHeader;
	
	public ServerProperties(final Dictionary<String, ?> properties, final Bundle brandingBundle, final Bundle thisBundle) {
		super();
		port = getPortNumber(properties, PROP_PORTNUMBER);
		portssl = getPortNumber(properties, PROP_PORTSSL);
		ethernetInterface = getProperty(properties, PROP_INTERFACE, System.getProperty(PROP_SYSTEMINTERFACE, null));
		// Branding bundle supersed name, author and version properties
		if (brandingBundle != null) {
			String s = brandingBundle.getHeaders().get(Constants.BUNDLE_NAME);
			if ((s != null) && !s.isEmpty()) {
				name = s;
			} else {
				name = getProperty(properties, PROP_NAME, thisBundle.getHeaders().get(Constants.BUNDLE_NAME));
			}
			s = brandingBundle.getHeaders().get(Constants.BUNDLE_VENDOR);
			if ((s != null) && !s.isEmpty()) {
				author = s;
			} else {
				author = getProperty(properties, PROP_AUTHOR, thisBundle.getHeaders().get(Constants.BUNDLE_VENDOR));
			}
			s = brandingBundle.getHeaders().get(Constants.BUNDLE_VERSION);
			if ((s != null) && !s.isEmpty()) {
				version = s;
			} else {
				version = getProperty(properties, PROP_VERSION, thisBundle.getVersion().toString());
			}
			s = brandingBundle.getHeaders().get(Constants.BUNDLE_DESCRIPTION);
			if ((s != null) && !s.isEmpty()) {
				description = s;
			} else {
				description = getProperty(properties, PROP_DESCRIPTION, thisBundle.getHeaders().get(Constants.BUNDLE_DESCRIPTION));
			}
			s = brandingBundle.getHeaders().get(Constants.BUNDLE_CONTACTADDRESS);
			if ((s != null) && !s.isEmpty()) {
				email = s;
			} else {
				email = getProperty(properties, PROP_CONTACTEMAIL, thisBundle.getHeaders().get(Constants.BUNDLE_CONTACTADDRESS));
			}
			s = brandingBundle.getHeaders().get(Constants.BUNDLE_LICENSE);
			if ((s != null) && !s.isEmpty()) {
				licenseURL = s;
			} else {
				licenseURL = getProperty(properties, PROP_LICENSE_URL, thisBundle.getHeaders().get(Constants.BUNDLE_LICENSE));
			}
			s = brandingBundle.getHeaders().get("Bundle-License-Name"); //$NON-NLS-1$
			if ((s != null) && !s.isEmpty()) {
				license = s;
			} else {
				license = getProperty(properties, PROP_LICENSE_NAME, thisBundle.getHeaders().get("Bundle-License-Name")); //$NON-NLS-1$
			}
			s = brandingBundle.getHeaders().get(Constants.BUNDLE_COPYRIGHT);
			if ((s != null) && !s.isEmpty()) {
				termsOfService = s;
			} else {
				termsOfService = getProperty(properties, PROP_COPYRIGHT, thisBundle.getHeaders().get(Constants.BUNDLE_COPYRIGHT));
			}
			s = brandingBundle.getHeaders().get(Constants.BUNDLE_DOCURL);
			if ((s != null) && !s.isEmpty()) {
				webSite = s;
			} else {
				webSite = getProperty(properties, PROP_URL, thisBundle.getHeaders().get(Constants.BUNDLE_DOCURL));
			}
		} else {
			name = getProperty(properties, PROP_NAME, thisBundle.getHeaders().get(Constants.BUNDLE_NAME));
			author = getProperty(properties, PROP_AUTHOR, thisBundle.getHeaders().get(Constants.BUNDLE_VENDOR));
			version = getProperty(properties, PROP_VERSION, thisBundle.getVersion().toString());
			description = getProperty(properties, PROP_DESCRIPTION, thisBundle.getHeaders().get(Constants.BUNDLE_DESCRIPTION));
			email = getProperty(properties, PROP_CONTACTEMAIL, thisBundle.getHeaders().get(Constants.BUNDLE_CONTACTADDRESS));
			licenseURL = getProperty(properties, PROP_LICENSE_URL, thisBundle.getHeaders().get(Constants.BUNDLE_LICENSE));
			license = getProperty(properties, PROP_LICENSE_NAME, thisBundle.getHeaders().get("Bundle-License-Name")); //$NON-NLS-1$
			termsOfService = getProperty(properties, PROP_COPYRIGHT, thisBundle.getHeaders().get(Constants.BUNDLE_COPYRIGHT));
			webSite = getProperty(properties, PROP_URL, thisBundle.getHeaders().get(Constants.BUNDLE_DOCURL));
		}
		String dn = getProperty(properties, PROP_DOMAINNAME, getLocalhostName());
		if (dn.equalsIgnoreCase("none")) { //$NON-NLS-1$
			if (ethernetInterface == null) {
				domainname = ""; //$NON-NLS-1$
			} else {
				domainname = ethernetInterface;
			}
		} else {
			domainname = dn;
		}
		owner = getProperty(properties, PROP_OWNER, author);
		kStore = getProperty(properties, PROP_KEYPATH, null);
		kSPwd = Crypto.decrypt(getProperty(properties, PROP_KEYSTOREPWD, null));
		kSType = getProperty(properties, PROP_KEYTYPE, null);
		keyAlias = getProperty(properties, PROP_KEYALIAS, null);
		keyPwd = Crypto.decrypt(getProperty(properties, PROP_KEYPWD, null));
		keyClient = getProperty(properties, PROP_CLIENTAUTH, false);
		tStore = getProperty(properties, PROP_TRUSTPATH, null);
		tSPwd = Crypto.decrypt(getProperty(properties, PROP_TRUSTSTOREPWD, null));
		tSType = getProperty(properties, PROP_TRUSTSTORETYPE, null);
		disabledCipherSuites = getProperty(properties, ConfiguredSSLContext.PROP_DISABLEDCIPHERSUITES, null);
		disabledProtocols = getProperty(properties, ConfiguredSSLContext.PROP_DISABLEDPROTOCOLS, null);
		enabledCipherSuites = getProperty(properties, ConfiguredSSLContext.PROP_ENABLEDCIPHERSUITES, null);
		enabledProtocols = getProperty(properties, ConfiguredSSLContext.PROP_ENABLEDPROTOCOLS, null);
		keyManagerAlgorithm = getProperty(properties, ConfiguredSSLContext.PROP_KEYSTORE_ALGO, null);
		protocol = getProperty(properties, ConfiguredSSLContext.PROP_SSL_PREFEREDPROTOCOL, "TLSv1.3"); //$NON-NLS-1$
		secureRandomAlgorithm = getProperty(properties, ConfiguredSSLContext.PROP_SECURERANDOM, null);
		trustManagerAlgorithm = getProperty(properties, ConfiguredSSLContext.PROP_TRUSTSTORE_ALGO, null);
		restletLogDisabled = getProperty(properties, PROP_LOGDISABLED, false);
		httpVersion = getProperty(properties, PROP_HTTPVERSION, 1);
		http3PEMWorkdir = getProperty(properties, PROP_HTTP3PEMWORKDIR, "./configuration/pem3/");
		if (httpVersion == 3) {
			new File(http3PEMWorkdir).mkdirs();
		}
		requestHeaderSize = getProperty(properties, PROP_HTTPREQUESTHEADERLIMIT, Integer.getInteger("com.arcadsoftware.httprequestheaderlimit", 16384)); //$NON-NLS-1$
		responseHeaderSize = getProperty(properties, PROP_HTTPRESPONSEHEADERLIMIT, Integer.getInteger("com.arcadsoftware.httpresponseheaderlimit", 16384)); //$NON-NLS-1$
		minThreads = getProperty(properties, PROP_MINTHREADS, Integer.getInteger("com.arcadsoftware.threadPoolminThreads", 8)); //$NON-NLS-1$
		maxThreads = getProperty(properties, PROP_MAXTHREADS, Integer.getInteger("com.arcadsoftware.threadPool.maxThreads", 200)); //$NON-NLS-1$
		threadsPriority = getProperty(properties, PROP_THREADSPRIORITY, Integer.getInteger("com.arcadsoftware.threadPool.threadsPriority", Thread.NORM_PRIORITY)); //$NON-NLS-1$
		threadsIdleTimeout = getProperty(properties, PROP_THREADSIDLETIMEOUT, Integer.getInteger("com.arcadsoftware.threadPool.idleTimeout", 60000)); //$NON-NLS-1$
		threadsStopTimeout = getProperty(properties, PROP_STOPTIMEOUT, Integer.getInteger("com.arcadsoftware.threadPool.stopTimeout", 5000)); //$NON-NLS-1$
		acceptors = getProperty(properties, PROP_ACCEPTORS, Integer.getInteger("com.arcadsoftware.connector.acceptors", -1)); //$NON-NLS-1$
		selectors = getProperty(properties, PROP_SELECTORS, Integer.getInteger("com.arcadsoftware.connector.selectors", -1)); //$NON-NLS-1$
		acceptQueueSize = getProperty(properties, PROP_ACCEPTQUEUESIZE, Integer.getInteger("com.arcadsoftware.connector.acceptQueueSize", 0)); //$NON-NLS-1$
		connectorIdleTimeout = getProperty(properties, PROP_CONNECTORIDLETIMEOUT, Integer.getInteger("com.arcadsoftware.connector.idleTimeout", 30000)); //$NON-NLS-1$
		connectorStopTimeout = getProperty(properties, PROP_CONNECTORSTOPTIMEOUT, Integer.getInteger("com.arcadsoftware.connector.stopTimeout", 30000)); //$NON-NLS-1$
		headerCacheSize  = getProperty(properties, PROP_HTTPHEADERCACHESIZE, Integer.getInteger("com.arcadsoftware.http.headerCacheSize", 512)); //$NON-NLS-1$
		outputBufferSize = getProperty(properties, PROP_HTTPOUTPUTBUFFERSIZE, Integer.getInteger("com.arcadsoftware.http.outputBufferSize", 32768)); //$NON-NLS-1$
		lrPeriod = getProperty(properties, PROP_LOWRESPERIOD, Integer.getInteger("com.arcadsoftware.lowResource.period", 1000)); //$NON-NLS-1$
		lrThreads = getProperty(properties, PROP_LOWRESTHREADS, Boolean.parseBoolean(System.getProperty("com.arcadsoftware.lowResource.threads", "true"))); //$NON-NLS-1$
		lrMaxMemory = getProperty(properties, PROP_LOWRESMAXMEMORY, Integer.getInteger("com.arcadsoftware.lowResource.maxMemory", 0)); //$NON-NLS-1$
		lrMaxConnections = getProperty(properties, PROP_LOWRESMAXCONNECTIONS, Integer.getInteger("com.arcadsoftware.lowResource.maxConnections",0)); //$NON-NLS-1$
		lrIdleTimeout = getProperty(properties, PROP_LOWRESIDLETIMEOUT, Integer.getInteger("com.arcadsoftware.lowResource.idleTimeout", 1000)); //$NON-NLS-1$
		lrStopTimeOut = getProperty(properties, PROP_LOWRESSTOPTIMEOUT, Integer.getInteger("com.arcadsoftware.lowResource.stopTimeout", 30000)); //$NON-NLS-1$
		maxCtn = getProperty(properties, PROP_MAXCTN, 0);
		maxCtnIdleTimeOut = getProperty(properties, PROP_MAXCTNIDLETIMEOUT, 0);
		sdGraceFully = getProperty(properties, PROP_SHUTDOWNGRACEFULLY, true);
		sdTimeout = getProperty(properties, PROP_SHUTDOWNTIMEOUT, 30000);
		useForwardedForHeader = getProperty(properties, PROP_USEFORWARDEDFORHEADER, Boolean.getBoolean("com.arcadsoftware.useForwardedForHeader"));
	}

	public ServerProperties(final Bundle thisBundle) {
		super();
		port = initPort("com.arcadsoftware.rest.port", "5252"); //$NON-NLS-1$ //$NON-NLS-2$
		portssl = initPort("com.arcadsoftware.rest.sslport", "0"); //$NON-NLS-1$ //$NON-NLS-2$
		ethernetInterface = System.getProperty(PROP_SYSTEMINTERFACE, null);
		name = thisBundle.getHeaders().get(Constants.BUNDLE_NAME);
		author = thisBundle.getHeaders().get(Constants.BUNDLE_VENDOR);
		version = thisBundle.getVersion().toString();
		email = thisBundle.getHeaders().get(Constants.BUNDLE_CONTACTADDRESS);
		description = thisBundle.getHeaders().get(Constants.BUNDLE_DESCRIPTION);
		licenseURL = thisBundle.getHeaders().get(Constants.BUNDLE_LICENSE);
		license = thisBundle.getHeaders().get("Bundle-License-Name"); //$NON-NLS-1$
		termsOfService = thisBundle.getHeaders().get(Constants.BUNDLE_COPYRIGHT);
		webSite = thisBundle.getHeaders().get(Constants.BUNDLE_DOCURL);
		domainname = getLocalhostName();
		owner = author;
		kStore = null;
		kSPwd = null;
		kSType = null;
		keyAlias = null;
		keyPwd = null;
		keyClient = false;
		tStore = null;
		tSPwd = null;
		tSType = null;
		disabledCipherSuites = null;
		disabledProtocols = null;
		enabledCipherSuites = null;
		enabledProtocols = null;
		keyManagerAlgorithm = null;
		protocol = null;
		secureRandomAlgorithm = null;
		trustManagerAlgorithm = null;
		restletLogDisabled = false;
		httpVersion = 1;
		http3PEMWorkdir = null;
		requestHeaderSize = Integer.getInteger("com.arcadsoftware.httprequestheaderlimit", 16384); //$NON-NLS-1$
		responseHeaderSize = Integer.getInteger("com.arcadsoftware.httpresponseheaderlimit", 16384); //$NON-NLS-1$
		minThreads = Integer.getInteger("com.arcadsoftware.threadPoolminThreads", 8); //$NON-NLS-1$
		maxThreads = Integer.getInteger("com.arcadsoftware.threadPool.maxThreads", 200); //$NON-NLS-1$
		threadsPriority = Integer.getInteger("com.arcadsoftware.threadPool.threadsPriority", Thread.NORM_PRIORITY); //$NON-NLS-1$
		threadsIdleTimeout = Integer.getInteger("com.arcadsoftware.threadPool.idleTimeout", 60000); //$NON-NLS-1$
		threadsStopTimeout = Integer.getInteger("com.arcadsoftware.threadPool.stopTimeout", 5000); //$NON-NLS-1$
		acceptors = Integer.getInteger("com.arcadsoftware.connector.acceptors", -1); //$NON-NLS-1$
		selectors = Integer.getInteger("com.arcadsoftware.connector.selectors", -1); //$NON-NLS-1$
		acceptQueueSize = Integer.getInteger("com.arcadsoftware.connector.acceptQueueSize", 0); //$NON-NLS-1$
		connectorIdleTimeout = Integer.getInteger("com.arcadsoftware.connector.idleTimeout", 30000); //$NON-NLS-1$
		connectorStopTimeout = Integer.getInteger("com.arcadsoftware.connector.stopTimeout", 30000); //$NON-NLS-1$
		headerCacheSize  = Integer.getInteger("com.arcadsoftware.http.headerCacheSize", 512); //$NON-NLS-1$
		outputBufferSize = Integer.getInteger("com.arcadsoftware.http.outputBufferSize", 32768); //$NON-NLS-1$
		lrPeriod = Integer.getInteger("com.arcadsoftware.lowResource.period", 1000); //$NON-NLS-1$
		lrThreads = Boolean.parseBoolean(System.getProperty("com.arcadsoftware.lowResource.threads", "true")); //$NON-NLS-1$
		lrMaxMemory = Integer.getInteger("com.arcadsoftware.lowResource.maxMemory", 0); //$NON-NLS-1$
		lrMaxConnections = Integer.getInteger("com.arcadsoftware.lowResource.maxConnections",0); //$NON-NLS-1$
		lrIdleTimeout = Integer.getInteger("com.arcadsoftware.lowResource.idleTimeout", 1000); //$NON-NLS-1$
		lrStopTimeOut = Integer.getInteger("com.arcadsoftware.lowResource.stopTimeout", 30000); //$NON-NLS-1$
		maxCtn = 0;
		maxCtnIdleTimeOut = 0;
		sdGraceFully = true;
		sdTimeout = 30000;
		useForwardedForHeader = Boolean.getBoolean("com.arcadsoftware.useForwardedForHeader");
	}
	
	public void close() {
		Crypto.clear(kSPwd);
		Crypto.clear(tSPwd);
		Crypto.clear(keyPwd);
	}
	
	private int initPort(String propName, String defValue) {
		try {
			return Integer.parseInt(System.getProperty(propName, defValue));
		} catch (NumberFormatException e) {
			return 0;
		}
	}

	/**
	 * Return true if there is not any modifications that require a server restart.
	 * 
	 * <p>
	 * If there is modification they are related to properties that can be changed during the execution of the HTTP(S) server(s).
	 *  
	 * @param oldProperties
	 * @return
	 */
	public boolean isNoMajorModification(ServerProperties oldProperties) {
		return (oldProperties != null) && (port == oldProperties.port) && nullsOrEquals(kStore, oldProperties.kStore) && //
				nullsOrEquals(tStore, oldProperties.tStore) && (portssl == oldProperties.portssl) && nullsOrEquals(protocol, oldProperties.protocol);
	}

	private boolean nullsOrEquals(char[] s1, char[] s2) {
		if (s1 == null) {
			return s2 == null;
		}
		return Arrays.equals(s1, s2);
	}

	private boolean nullsOrEquals(Object s1, Object s2) {
		if (s1 == null) {
			return s2 == null;
		}
		return s1.equals(s2);
	}
	
	private String getLocalhostName() {
		try {
			return InetAddress.getLocalHost().getHostName();
		} catch (UnknownHostException e) {
			// Localhost is always known (it return the local IP if not) !
			return ""; //$NON-NLS-N$
		}
	}

	private String getProperty(final Dictionary<String, ?> properties, final String key, final String defaultValue) {
		final Object value = properties.get(key);
		if (value == null) {
			return defaultValue;
		}
		final String sv = value.toString();
		if (sv.isEmpty()) {
			return defaultValue;
		}
		return sv;
	}

	private boolean getProperty(final Dictionary<String, ?> properties, final String key, final boolean defaultValue) {
		final Object value = properties.get(key);
		if (value == null) {
			return defaultValue;
		}
		final String sv = value.toString();
		if (sv.isEmpty()) {
			return defaultValue;
		}
		return sv.equalsIgnoreCase("true") || sv.equalsIgnoreCase("yes") || sv.equals("1"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}

	private int getProperty(final Dictionary<String, ?> properties, final String key, final int defaultValue) {
		final Object value = properties.get(key);
		if (value == null) {
			return defaultValue;
		}
		if (value instanceof Integer iv) {
			return iv;
		}
		try {
			return Integer.parseInt(value.toString());
		} catch (NumberFormatException e) {
			return defaultValue;
		}
	}
	
	private Integer getPortNumber(final Dictionary<String, ?> properties, final String key) {
		try {
			int p = Integer.parseInt(getProperty(properties, key, "0")); //$NON-NLS-1$
			if ((p >= 0) && (p < 65536)) {
				return p;
			}
		} catch (NumberFormatException e) {}
		return 0;
	}

	public boolean hasDomainName() {
		return (domainname != null) && !domainname.trim().isEmpty();
	}

	public String[] getDomainNames() {
		if ((domainname == null) || domainname.trim().isEmpty()) {
			return new String[0];
		}
		return domainname.split(" "); //$NON-NLS-1$
	}
	
	public boolean isServerInactive() {
		return (port == 0) && (portssl == 0);
	}

	public boolean useHTTPS() {
		return (portssl > 0) &&	(((kStore != null) && (keyAlias != null)) || (keyClient && (tStore != null)));
	}

	@Override
	public String toString() {
		return "ServerProperties [port=" + port + //$NON-NLS-1$
				", portssl=" + portssl + //$NON-NLS-1$
				", name=" + name + //$NON-NLS-1$
				", author=" + author + //$NON-NLS-1$
				", version=" + version + //$NON-NLS-1$
				", domainname=" + domainname + //$NON-NLS-1$
				", owner=" + owner + //$NON-NLS-1$
				", description=" + description + //$NON-NLS-1$
				", kStore=" + kStore + //$NON-NLS-1$
				", keyAlias=" + keyAlias + //$NON-NLS-1$
				", keyClient=" + keyClient + //$NON-NLS-1$
				", tStore=" + tStore + ']'; //$NON-NLS-1$
	}

	@Override
	public int hashCode() {
		return 961 + (31 * port) + portssl;
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof ServerProperties)) {
			return false;
		}
		ServerProperties sp = (ServerProperties) obj;
		return (this == obj) || ((sp.port == port) && //
					nullsOrEquals(sp.domainname, domainname) && //
					nullsOrEquals(sp.name, name) && //
					nullsOrEquals(sp.version, version) && //
					nullsOrEquals(sp.author, author) && //
					nullsOrEquals(sp.owner, owner) && //
					nullsOrEquals(sp.description, description) && //
					nullsOrEquals(sp.kStore, kStore) && //
					nullsOrEquals(sp.tStore, tStore) && //
					nullsOrEquals(sp.kSPwd, kSPwd) && //
					nullsOrEquals(sp.tSPwd, tSPwd) && //
					nullsOrEquals(sp.keyAlias, keyAlias) && //
					nullsOrEquals(sp.kSType, kSType) && //
					nullsOrEquals(sp.tSType, tSType) && //
					nullsOrEquals(sp.keyPwd, keyPwd) && //
					(sp.portssl == portssl) && //
					(sp.keyClient == keyClient) && //
					(sp.restletLogDisabled == restletLogDisabled) && //
					nullsOrEquals(sp.disabledCipherSuites, disabledCipherSuites) && //
					nullsOrEquals(sp.disabledProtocols, disabledProtocols) && //
					nullsOrEquals(sp.enabledCipherSuites, enabledCipherSuites) && //
					nullsOrEquals(sp.enabledProtocols, enabledProtocols) && //
					nullsOrEquals(sp.keyManagerAlgorithm, keyManagerAlgorithm) && //
					nullsOrEquals(sp.protocol, protocol) && //
					nullsOrEquals(sp.secureRandomAlgorithm, secureRandomAlgorithm) && //
					nullsOrEquals(sp.trustManagerAlgorithm, trustManagerAlgorithm));
	}

	public int getPort() {
		return port;
	}

	public int getPortssl() {
		return portssl;
	}

	public String getName() {
		return name;
	}

	public String getAuthor() {
		return author;
	}

	public String getVersion() {
		return version;
	}

	public String getDomainname() {
		return domainname;
	}

	public String getOwner() {
		return owner;
	}

	public String getDescription() {
		return description;
	}

	public String getkStore() {
		return kStore;
	}

	public char[] getkSPwd() {
		return kSPwd;
	}

	public String getkSType() {
		return kSType;
	}

	public String getKeyAlias() {
		return keyAlias;
	}

	public char[] getKeyPwd() {
		return keyPwd;
	}

	public boolean isKeyClient() {
		return keyClient;
	}

	public String gettStore() {
		return tStore;
	}

	public char[] gettSPwd() {
		return tSPwd;
	}

	public String gettSType() {
		return tSType;
	}

	public String getDisabledCipherSuites() {
		return disabledCipherSuites;
	}

	public String getDisabledProtocols() {
		return disabledProtocols;
	}

	public String getEnabledCipherSuites() {
		return enabledCipherSuites;
	}

	public String getEnabledProtocols() {
		return enabledProtocols;
	}

	public String getKeyManagerAlgorithm() {
		return keyManagerAlgorithm;
	}

	public String getProtocol() {
		return protocol;
	}

	public String getSecureRandomAlgorithm() {
		return secureRandomAlgorithm;
	}

	public String getTrustManagerAlgorithm() {
		return trustManagerAlgorithm;
	}

	public boolean isRestletLogDisabled() {
		return restletLogDisabled;
	}

	public String getEmail() {
		return email;
	}

	public String getLicense() {
		return license;
	}

	public String getTermsOfService() {
		return termsOfService;
	}

	public String getWebSite() {
		return webSite;
	}

	public String getLicenseURL() {
		return licenseURL;
	}

	public String getEthernetInterface() {
		return ethernetInterface;
	}

	public void setHTTPSParameters(Series<Parameter> parameters) {
		parameters.add("sslContextFactory", org.restlet.engine.ssl.DefaultSslContextFactory.class.getName()); //$NON-NLS-1$
		parameters.add("keyStorePath", kStore); //$NON-NLS-1$
		parameters.add("keyStorePassword", new String(kSPwd)); //$NON-NLS-1$
		parameters.add("keyPassword", new String(keyPwd)); //$NON-NLS-1$
		if ((kSType != null) && !kSType.isBlank()) {
			parameters.add("keyStoreType", kSType); //$NON-NLS-1$
		}
		if ((disabledCipherSuites != null) && !disabledCipherSuites.isBlank()) {
			parameters.add("disabledCipherSuites", disabledCipherSuites); //$NON-NLS-1$
		}
		if ((disabledProtocols != null) && disabledProtocols.isBlank()) {
			parameters.add("disabledProtocols", disabledProtocols); //$NON-NLS-1$
		}
		if ((enabledCipherSuites != null) && enabledCipherSuites.isBlank()) {
			parameters.add("enabledCipherSuites", enabledCipherSuites); //$NON-NLS-1$
		}
		if ((enabledProtocols != null) && !enabledProtocols.isBlank()) {
			parameters.add("enabledProtocols", enabledProtocols); //$NON-NLS-1$
		}
		if ((keyManagerAlgorithm != null) && !keyManagerAlgorithm.isBlank()) {
			parameters.add("keyManagerAlgorithm", keyManagerAlgorithm); //$NON-NLS-1$
		}
		if ((protocol != null) && !protocol.isBlank()) {
			parameters.add("protocol", protocol); //$NON-NLS-1$
		}
		if ((secureRandomAlgorithm != null) && secureRandomAlgorithm.isBlank()) {
			parameters.add("secureRandomAlgorithm", secureRandomAlgorithm); //$NON-NLS-1$
		}
		if (keyClient && (tStore != null) && !tStore.isBlank()) {
			parameters.add("trustStorePath", tStore); //$NON-NLS-1$
			if (tSPwd != null) {
				parameters.add("trustStorePassword", new String(tSPwd)); //$NON-NLS-1$
			}
			if (tSType != null) {
				parameters.add("trustStoreType", tSType); //$NON-NLS-1$
			}
			if (trustManagerAlgorithm != null) {
				parameters.add("trustManagerAlgorithm", trustManagerAlgorithm); //$NON-NLS-1$
			}
			parameters.add("needClientAuthentication", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		if (httpVersion > 1) {
			if (httpVersion == 3) {
				parameters.add("http.transport.protocols", "HTTP1_1, HTTP2, HTTP3"); //$NON-NLS-1$ //$NON-NLS-2$
				parameters.add("http3.pem.workdir", http3PEMWorkdir); //$NON-NLS-1$
			} else {
				parameters.add("http.transport.protocols", "HTTP1_1, HTTP2"); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		jettyServerFineTuning(parameters);
	}

	public void setHTTPParameters(Series<Parameter> parameters) {
		if (httpVersion > 1) {
			parameters.add("http.transport.protocol", "HTTP2"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		jettyServerFineTuning(parameters);
	}

	private void jettyServerFineTuning(Series<Parameter> parameters) {
		// Jetty HTTP Server fine tuning:
		// parameters specification from https://javadocs.restlet.talend.com/2.4/jse/ext/org/restlet/ext/jetty/JettyServerHelper.html
		parameters.add("http.requestHeaderSize", Integer.toString(requestHeaderSize)); //$NON-NLS-1$ //$NON-NLS-2$
		parameters.add("http.responseHeaderSize", Integer.toString(responseHeaderSize)); //$NON-NLS-1$ //$NON-NLS-2$
		parameters.add("threadPool.minThreads", Integer.toString(minThreads)); //$NON-NLS-1$			
		parameters.add("threadPool.maxThreads", Integer.toString(maxThreads)); //$NON-NLS-1$			
		parameters.add("threadPool.threadsPriority", Integer.toString(threadsPriority)); //$NON-NLS-1$			
		parameters.add("threadPool.idleTimeout", Integer.toString(threadsIdleTimeout)); //$NON-NLS-1$			
		parameters.add("threadPool.stopTimeout", Integer.toString(threadsStopTimeout)); //$NON-NLS-1$			
		parameters.add("connector.acceptors", Integer.toString(acceptors)); //$NON-NLS-1$			
		parameters.add("connector.selectors", Integer.toString(selectors)); //$NON-NLS-1$			
		parameters.add("connector.acceptQueueSize", Integer.toString(acceptQueueSize)); //$NON-NLS-1$			
		parameters.add("connector.idleTimeout", Integer.toString(connectorIdleTimeout)); //$NON-NLS-1$			
		parameters.add("connector.stopTimeout", Integer.toString(connectorStopTimeout)); //$NON-NLS-1$			
		parameters.add("http.headerCacheSize", Integer.toString(headerCacheSize)); //$NON-NLS-1$			
		parameters.add("http.outputBufferSize", Integer.toString(outputBufferSize)); //$NON-NLS-1$			
		parameters.add("lowResource.period", Integer.toString(lrPeriod)); //$NON-NLS-1$
		if (!lrThreads) {
			parameters.add("lowResource.threads", "false"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		parameters.add("lowResource.maxMemory", Integer.toString(lrMaxMemory)); //$NON-NLS-1$			
		parameters.add("lowResource.maxConnections", Integer.toString(lrMaxConnections)); //$NON-NLS-1$			
		parameters.add("lowResource.idleTimeout", Integer.toString(lrIdleTimeout)); //$NON-NLS-1$			
		parameters.add("lowResource.stopTimeout", Integer.toString(lrStopTimeOut)); //$NON-NLS-1$			
		if (useForwardedForHeader) {
			parameters.add("useForwardedForHeader", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		parameters.add("server.maxConnections", Integer.toString(maxCtn)); //$NON-NLS-1$			
		parameters.add("server.maxConnections.idleTimeout", Integer.toString(maxCtnIdleTimeOut)); //$NON-NLS-1$
		if (!sdGraceFully) {
			parameters.add("shutdown.gracefully", "false"); //$NON-NLS-1$ //$NON-NLS-1$
		}
		parameters.add("shutdown.timeout", Integer.toString(sdTimeout)); //$NON-NLS-1$			
	}
}