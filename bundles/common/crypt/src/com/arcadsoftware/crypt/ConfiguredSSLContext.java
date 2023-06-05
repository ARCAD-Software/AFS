/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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
package com.arcadsoftware.crypt;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.SecureRandom;
import java.security.Security;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;
import java.util.ArrayList;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.net.ssl.HandshakeCompletedEvent;
import javax.net.ssl.HandshakeCompletedListener;
import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLServerSocketFactory;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;

import org.bouncycastle.jce.provider.BouncyCastleProvider;

import com.arcadsoftware.crypt.internal.WrappedSockectFactory;
import com.arcadsoftware.crypt.internal.WrapperSSLContext;

/**
 * Allow to create SSL context with an homogeneous set of configuration properties.
 * 
 * <p>
 * This implementation does not stand on the default configuration of the JVM. i.e. No system properties are used to
 * create the SSLContext nor the Socket factories.
 * 
 * <p>
 * By the way some specific TLS related task must be delayed to the actual protocol manager, like the "start TLS" 
 * option of the target Host name verification, as these operations depend on the actual connection protocol.
 * 
 * @author ARCAD Software
 */
public class ConfiguredSSLContext {

	/**
	 * Define the Java Security Provider to use in this configuration.
	 * <p>
	 * May be null, the JVM default Security Provider is used in this case.
	 * <p>
	 * <b>Value Type:</b> String
	 */
	public static final String PROP_SSL_PROVIDER = "ssl.provider"; //$NON-NLS-1$

	/**
	 * Define the preferred SSL protocol to be used.
	 * <p>
	 * May be null, in that case the TLSv1.3 (or newer version) is used.
	 * <p>
	 * <b>Value Type:</b> String
	 */
	public static final String PROP_SSL_PREFEREDPROTOCOL = "ssl.prefered.protocol"; //$NON-NLS-1$

	/**
	 * If true the Protocol used will be StartTLS (if available by the implementation.
	 * <p>
	 * <b>Value Type:</b> Boolean
	 * 
	 * @see #isStartTLS()
	 */
	public static final String PROP_SSL_START = "ssl.start"; //$NON-NLS-1$

	/**
	 * Define the Secured Random algorithm implementation to use during the TLS connection.
	 * <p>
	 * If null, the JVM default SecureRandom implementation is used.
	 * <p>
	 * <b>Value Type:</b> String
	 */
	public static final String PROP_SECURERANDOM = "ssl.random"; //$NON-NLS-1$

	/**
	 * Define a local file system path to a Trusted certificate Store.
	 * <p>
	 * If null no TrustManager will be defined, and if PROP_KEYSTORE_PATH is also null then no SSLContext at all will be
	 * created.
	 * <p>
	 * <b>Value Type:</b> String
	 * 
	 * @see #PROP_KEYSTORE_PATH
	 */
	public static final String PROP_TRUSTSTORE_PATH = "ssl.truststore.path"; //$NON-NLS-1$

	/**
	 * Define the optional password used to load the Trusted Certificate Store.
	 * <p>
	 * <b>Value Type:</b> String
	 * 
	 * @see #PROP_TRUSTSTORE_PATH
	 */
	public static final String PROP_TRUSTSTORE_PWD = "ssl.truststore.pwd"; //$NON-NLS-1$

	/**
	 * Define the Store type used by the Trusted Certificate Store.
	 * <p>
	 * Default value is JKS.
	 * <p>
	 * <b>Value Type:</b> String
	 */
	public static final String PROP_TRUSTSTORE_TYPE = "ssl.truststore.type"; //$NON-NLS-1$

	/**
	 * The algorithm used by the Certificated contained into the Trusted Certificate Store.
	 * <p>
	 * The default value is the JVM default one, e.g. SunX509 with ORACLE JVM or ibmX509 with IBM one.
	 * <p>
	 * <b>Value Type:</b> String
	 */
	public static final String PROP_TRUSTSTORE_ALGO = "ssl.truststore.algorithm"; //$NON-NLS-1$

	/**
	 * Define a local file system path to a Keys Store.
	 * <p>
	 * If null, no KeyManager will be defined, and if PROP_TRUSTSTORE_PATH is also null then no SSLContext at all will
	 * be created.
	 * <p>
	 * <b>Value Type:</b> String
	 * 
	 * @see #PROP_TRUSTSTORE_PATH
	 */
	public static final String PROP_KEYSTORE_PATH = "ssl.keystore.path"; //$NON-NLS-1$

	/**
	 * Define the optional password used to load the Key Store.
	 * <p>
	 * <b>Value Type:</b> String
	 */
	public static final String PROP_KEYSTORE_PWD = "ssl.keystore.pwd"; //$NON-NLS-1$

	/**
	 * Define the Store type used by the Keys Store.
	 * <p>
	 * Default value is JKS.
	 * <p>
	 * <b>Value Type:</b> String
	 */
	public static final String PROP_KEYSTORE_TYPE = "ssl.keystore.type"; //$NON-NLS-1$

	/**
	 * The algorithm used by the Certificated contained into the Key Store.
	 * <p>
	 * The default value is the JVM default one, e.g. SunX509 with ORACLE JVM or inmX509 with IBM one.
	 * <p>
	 * <b>Value Type:</b> String
	 */
	public static final String PROP_KEYSTORE_ALGO = "ssl.keystore.algorithm"; //$NON-NLS-1$

	/**
	 * The password used to access the keys contained into the Keys Store.
	 * <p>
	 * By default the password is assumed to be the same as the Keys Store password.
	 * <p>
	 * <b>Value Type:</b> String
	 * 
	 * @see #PROP_KEYSTORE_PWD
	 */
	public static final String PROP_KEYSTORE_KEYPWD = "ssl.keystore.keypwd"; //$NON-NLS-1$

	/**
	 * Define a list, white space separated, of name of disabled cipher suites.
	 * <p>
	 * <b>Value Type:</b> String
	 */
	public static final String PROP_DISABLEDCIPHERSUITES = "ssl.ciphersuites.disabled"; //$NON-NLS-1$

	/**
	 * Define a list, white space separated, of name of disabled protocols.
	 * 
	 * <p>
	 * <b>Value Type:</b> String
	 */
	public static final String PROP_DISABLEDPROTOCOLS = "ssl.protocols.disabled"; //$NON-NLS-1$

	/**
	 * Define a list, white space separated, of name of explicitly enabled cipher suites.
	 * 
	 * <p>
	 * <b>Value Type:</b> String
	 */
	public static final String PROP_ENABLEDCIPHERSUITES = "ssl.ciphersuites.enabled"; //$NON-NLS-1$

	/**
	 * Define a list, white space separated, of name of explicitly enabled protocols.
	 * 
	 * <p>
	 * <b>Value Type:</b> String
	 */
	public static final String PROP_ENABLEDPROTOCOLS = "ssl.protocols.enabled"; //$NON-NLS-1$

	/**
	 * Define that the SSL Server will request client authentication. This option is only useful in the server mode. The
	 * client authentication setting is one of the following:
	 * 
	 * <ul>
	 * <li>client authentication required
	 * <li>client authentication requested
	 * <li>no client authentication desired
	 * </ul>
	 * 
	 * <p>
	 * Unlike <b>ssl.clientauth.need</b>, if this option is set and the client chooses not to provide authentication
	 * information about itself, the negotiations will continue.
	 * 
	 * <p>
	 * <b>Value Type:</b> Boolean
	 */
	public static final String PROP_WANTCLIENTAUTH = "ssl.clientauth.want"; //$NON-NLS-1$

	/**
	 * Define that the SSL Server will require client authentication. This option is only useful in the server mode. The
	 * client authentication setting is one of the following:
	 * 
	 * <ul>
	 * <li>client authentication required
	 * <li>client authentication requested
	 * <li>no client authentication desired
	 * </ul>
	 * 
	 * <p>
	 * Unlike ssl.clientauth.want, if this option is set and the client chooses not to provide authentication
	 * information about itself, the negotiations will stop and the engine will begin its closure procedure.
	 * 
	 * <p>
	 * <b>Value Type:</b> Boolean
	 */
	public static final String PROP_NEEDCLIENTAUTH = "ssl.clientauth.need"; //$NON-NLS-1$

	/**
	 * Configures the SSL engine to use client (or server) mode when handshaking. This method must be called before any
	 * handshaking occurs. Once handshaking has begun, the mode can not be reset for the life of this engine. Servers
	 * normally authenticate themselves, and clients are not required to do so.
	 * 
	 * <p>
	 * <b>Value Type:</b> String
	 */
	public static final String PROP_USECLIENTMODE = "ssl.client.mode"; //$NON-NLS-1$

	/**
	 * Define if HTTPS connection should verify the URL hostname against the used certificate. Default value is true.
	 * 
	 * <p>
	 * <b>Value Type:</b> Boolean
	 */
	public static final String PROP_VERIFYHOSTNAME = "ssl.hostname.verification"; //$NON-NLS-1$

	// Used to set some default values.
	private static final String DEFAULT_STOREALGO;
	
	static {
		// On the IBM JVM the default Keystore manager certificate algorithm is 
		// IbmX509 and not SunX509 like Restlet define it.
		String vendor = System.getProperty("java.vendor"); //$NON-NLS-1$
		if ((vendor != null) && vendor.startsWith("IBM")) { //$NON-NLS-1$
			DEFAULT_STOREALGO = "IbmX509"; //$NON-NLS-1$
		} else {
			DEFAULT_STOREALGO = "SunX509"; //$NON-NLS-1$
		}
	}
	
	private final HashMap<String, Object> properties;
	private final SSLContext context;
	private final boolean startTLS;
	private final boolean wantClientAuth;
	private final boolean needClientAuth;
	private final boolean forceUseClientMode;
	private final boolean useClientMode;
	private final boolean verifyHostname;
	private final ArrayList<String> cipherSuitesEnabled;
	private final ArrayList<String> cipherSuitesDisabled;
	private final ArrayList<String> protocolsEnabled;
	private final ArrayList<String> protocolsDisabled;

	/**
	 * Create a new TLS configuration.
	 * 
	 * @param props
	 * @throws ConfiguredSSLContextException
	 */
	public ConfiguredSSLContext(Dictionary<String, Object> props) throws ConfiguredSSLContextException {
		super();
		properties = new HashMap<String, Object>();
		if (props != null) {
			Enumeration<String> keys = props.keys();
			while(keys.hasMoreElements()) {
				String k = keys.nextElement();
				if (k.startsWith("ssl.")) { //$NON-NLS-1$
					properties.put(k, props.get(k));
				}
			}
		}
		if (Security.getProperty(BouncyCastleProvider.PROVIDER_NAME) == null) {
			Security.addProvider(new BouncyCastleProvider());
		}
		cipherSuitesEnabled = new ArrayList<String>();
		cipherSuitesDisabled = new ArrayList<String>();
		protocolsEnabled = new ArrayList<String>();
		protocolsDisabled = new ArrayList<String>();
		SSLContext ctx = null;
		if (isActive(props)) {
			String provider = getProp(props, PROP_SSL_PROVIDER, null);
			String protocol = getProp(props, PROP_SSL_PREFEREDPROTOCOL, "TLSv1.3"); //$NON-NLS-1$
			try {
				if (provider == null) {
					ctx = SSLContext.getInstance(protocol);
				} else {
					ctx = SSLContext.getInstance(protocol, provider);
				}
				ctx.init(getKeyManagers(props, provider), getTrustManagers(props, provider),
						getSecureRandom(props, provider));
			} catch (KeyManagementException | NoSuchAlgorithmException | NoSuchProviderException e) {
				throw new ConfiguredSSLContextException(e);
			}
		}
		if (ctx == null) {
			verifyHostname = false;
			startTLS = false;
			context = null;
			wantClientAuth = false;
			needClientAuth = false;
			forceUseClientMode = false;
			useClientMode = false;
		} else {
			verifyHostname = getProp(props, PROP_VERIFYHOSTNAME, true);
			startTLS = getProp(props, PROP_SSL_START, false);
			wantClientAuth = getProp(props, PROP_WANTCLIENTAUTH, false);
			needClientAuth = getProp(props, PROP_NEEDCLIENTAUTH, false);
			forceUseClientMode = !getProp(props, PROP_USECLIENTMODE, "").isEmpty();
			useClientMode = getProp(props, PROP_USECLIENTMODE, false);
			addAllProps(props, PROP_ENABLEDCIPHERSUITES, cipherSuitesEnabled);
			addAllProps(props, PROP_DISABLEDCIPHERSUITES, cipherSuitesDisabled);
			addAllProps(props, PROP_ENABLEDPROTOCOLS, protocolsEnabled);
			addAllProps(props, PROP_DISABLEDPROTOCOLS, protocolsDisabled);
			context = new WrapperSSLContext(ctx, this);
		}
	}

	private void addAllProps(Dictionary<String, Object> props, String propName,	ArrayList<String> list) {
		for(String s: getProp(props, PROP_ENABLEDCIPHERSUITES, "").split(" ")) { //$NON-NLS-1$ //$NON-NLS-2$
			if (!s.isEmpty()) {
				list.add(s.toLowerCase());
			}
		}
	}

	private KeyManager[] getKeyManagers(Dictionary<String, Object> props, String provider)
			throws ConfiguredSSLContextException {
		String ksFileName = getProp(props, PROP_KEYSTORE_PATH, null);
		if (ksFileName == null) {
			return null;
		}
		File ksFile = new File(ksFileName);
		if (!ksFile.isFile()) {
			throw new ConfiguredSSLContextException("Keys Store file not found: " + ksFile.getAbsolutePath());
		}
		String ksPwd = getProp(props, PROP_KEYSTORE_PWD, null);
		if (ksPwd == null) {
			throw new ConfiguredSSLContextException("No password configured for Keys Store, using unprotected Keys Store is not allowed.");
		}
		try {
			KeyStore ks;
			if (provider == null) {
				ks = KeyStore.getInstance(getProp(props, PROP_KEYSTORE_TYPE, "JKS")); //$NON-NLS-1$
			} else {
				ks = KeyStore.getInstance(getProp(props, PROP_KEYSTORE_TYPE, "JKS"), provider); //$NON-NLS-1$
			}
			try (FileInputStream fis = new FileInputStream(ksFile)) {
				ks.load(fis, ksPwd.toCharArray());
			} catch (IOException e) {
				throw new ConfiguredSSLContextException("Keys Store file not accessible: " + ksFile.getAbsolutePath(), e);
			} catch (NoSuchAlgorithmException | CertificateException e) {
				throw new ConfiguredSSLContextException("Error during initialization of Keys Store file: " + ksFile.getAbsolutePath(), e);
			}
			KeyManagerFactory kmf = KeyManagerFactory
					.getInstance(getProp(props, PROP_KEYSTORE_TYPE, KeyManagerFactory.getDefaultAlgorithm()));
			kmf.init(ks, getProp(props, PROP_KEYSTORE_KEYPWD, ksPwd).toCharArray());
			return kmf.getKeyManagers();
		} catch (KeyStoreException | NoSuchProviderException | NoSuchAlgorithmException e) {
			throw new ConfiguredSSLContextException(e);
		} catch (UnrecoverableKeyException e) {
			throw new ConfiguredSSLContextException("Keys Store error, can not access to key in file: " + ksFile.getAbsolutePath(), e);
		}
	}

	private TrustManager[] getTrustManagers(Dictionary<String, Object> props, String provider)
			throws ConfiguredSSLContextException {
		String tsFileName = getProp(props, PROP_TRUSTSTORE_PATH, null);
		if (tsFileName == null) {
			return null;
		}
		File tsFile = new File(tsFileName);
		if (!tsFile.isFile()) {
			throw new ConfiguredSSLContextException(
					"Trusted certificates Store file not found: " + tsFile.getAbsolutePath());
		}
		String tsPwd = getProp(props, PROP_TRUSTSTORE_PWD, null);
		if (tsPwd == null) {
			throw new ConfiguredSSLContextException(
					"No password configured for Trusted certificates Store, using unprotected Keys Store is not allowed.");
		}
		try {
			KeyStore ts;
			if (provider == null) {
				ts = KeyStore.getInstance(getProp(props, PROP_TRUSTSTORE_TYPE, "JKS")); //$NON-NLS-1$
			} else {
				ts = KeyStore.getInstance(getProp(props, PROP_TRUSTSTORE_TYPE, "JKS"), provider); //$NON-NLS-1$
			}
			try (FileInputStream fis = new FileInputStream(tsFile)) {
				ts.load(fis, tsPwd.toCharArray());
			} catch (IOException e) {
				throw new ConfiguredSSLContextException(
						"Trusted certificates Store file not accessible: " + tsFile.getAbsolutePath(), e);
			} catch (NoSuchAlgorithmException | CertificateException e) {
				throw new ConfiguredSSLContextException(
						"Error during initialization of Trusted certificates Store file: " + tsFile.getAbsolutePath(),
						e);
			}
			TrustManagerFactory tmf = TrustManagerFactory
					.getInstance(getProp(props, PROP_TRUSTSTORE_TYPE, TrustManagerFactory.getDefaultAlgorithm()));
			tmf.init(ts);
			return tmf.getTrustManagers();
		} catch (KeyStoreException | NoSuchProviderException | NoSuchAlgorithmException e) {
			throw new ConfiguredSSLContextException(e);
		}
	}

	private SecureRandom getSecureRandom(Dictionary<String, Object> props, String provider)
			throws ConfiguredSSLContextException {
		String random = getProp(props, PROP_SECURERANDOM, null);
		if (random == null) {
			return null;
		}
		try {
			if (provider == null) {
				return SecureRandom.getInstance(random);
			}
			return SecureRandom.getInstance(random, provider);
		} catch (NoSuchAlgorithmException | NoSuchProviderException e) {
			throw new ConfiguredSSLContextException(e);
		}
	}

	private boolean isActive(Dictionary<String, Object> props) {
		// Test that the required props are not null AND not empty
		return (getProp(props, PROP_TRUSTSTORE_PATH, null) != null) || (getProp(props, PROP_KEYSTORE_PATH, null) != null);
	}

	/**
	 * If the value is empty or null then return the defValue.
	 * 
	 * @param props
	 * @param name
	 * @param defValue
	 * @return
	 */
	private String getProp(Dictionary<String, Object> props, String name, String defValue) {
		Object o = props.get(name);
		if (o != null) {
			String s = o.toString().trim();
			if (!s.isEmpty()) {
				return s;
			}
		}
		return defValue;
	}

	private String getProp(Map<String, Object> props, String name, String defValue) {
		Object o = props.get(name);
		if (o != null) {
			String s = o.toString().trim();
			if (!s.isEmpty()) {
				return s;
			}
		}
		return defValue;
	}

	/**
	 * If the value is empty or null then return the defValue.
	 * 
	 * @param props
	 * @param name
	 * @param defValue
	 * @return
	 */
	private Boolean getProp(Dictionary<String, Object> props, String name, boolean defValue) {
		Object o = props.get(name);
		if (o instanceof Boolean) {
			return (Boolean) o;
		}
		if (o != null) {
			try {
				String s = o.toString().trim();
				if (!s.isEmpty()) {
					return s.equalsIgnoreCase("true") || s.equalsIgnoreCase("yes") || s.equals("1"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}
			} catch (NumberFormatException e) {}
		}
		return defValue;
	}

	/**
	 * Returns a SocketFactory object for this context.
	 * 
	 * @return The SSLContext object or null is it is not initialized.
	 */
	public SSLContext getContext() {
		return context;
	}

	/**
	 * Returns a SocketFactory object for this context.
	 * 
	 * @return The SocketFactory object or null if the TLE context is not initialized.
	 */
	public SSLSocketFactory getSocketFactory() {
		if (context == null) {
			return null;
		}
		try {
			return context.getSocketFactory();
		} catch (IllegalStateException e) {
			return null;
		}
	}

	/**
	 * Returns a ServerSocketFactory object for this context.
	 * 
	 * @return the ServerSocketFactory object
	 */
	public SSLServerSocketFactory getServerSocketFactory() {
		if (context == null) {
			return null;
		}
		try {
			return context.getServerSocketFactory();
		} catch (IllegalStateException e) {
			return null;
		}
	}

	/**
	 * Return true if the TLS Configuration specify the usage of the Start TLS protocol, in that case the Socket
	 * connection must be done on the "non" SSL port number.
	 * 
	 * @return
	 */
	public boolean isStartTLS() {
		return startTLS;
	}

	/**
	 * Return true is this SSL server required the client authentication.
	 *   
	 * @return
	 */
	public boolean isWantClientAuth() {
		return wantClientAuth;
	}

	/**
	 * Return true is this SSL server request the client authentication.
	 * 
	 * @return
	 */
	public boolean isNeedClientAuth() {
		return needClientAuth;
	}

	/**
	 * Use the client mode for the SSL handshake.
	 * @return
	 * @see #isForceClientMode()
	 */
	public boolean isUseClientMode() {
		return useClientMode;
	}

	/**
	 * Return true if the {@link #isUseClientMode()} is used to set the Client Mode on.
	 * 
	 * @return
	 */
	public boolean isForceClientMode() {
		return forceUseClientMode;
	}

	/**
	 * Return a sub-list of Cipher suites, according to the enabled and disabled lists.
	 * 
	 * @param supportedCipherSuites
	 * @return
	 */
	public String[] getEnabledCipherSuites(String[] supportedCipherSuites) {
		Set<String> resultSet = new HashSet<>();
		if (supportedCipherSuites != null) {
			for (String supportedCipherSuite : supportedCipherSuites) {
				String s = supportedCipherSuite.toLowerCase();
				if ((cipherSuitesEnabled.isEmpty() || cipherSuitesEnabled.contains(s)) && //
						(cipherSuitesDisabled.isEmpty() || !cipherSuitesDisabled.contains(s))) {
					resultSet.add(supportedCipherSuite);
				}
			}
		}
		return resultSet.toArray(new String[resultSet.size()]);
	}

	/**
	 * Return a sub-list of Protocols, according to the enabled and disabled lists.
	 * 
	 * @param supportedProtocols
	 * @return
	 */
	public String[] getEnabledProtocols(String[] supportedProtocols) {
		Set<String> resultSet = new HashSet<>();
		if (supportedProtocols != null) {
			for (String supportedProtocol : supportedProtocols) {
				String s = supportedProtocol.toLowerCase();
				if ((protocolsEnabled.isEmpty() || protocolsEnabled.contains(s)) && //
						(protocolsDisabled.isEmpty() || !protocolsDisabled.contains(s))) {
					resultSet.add(supportedProtocol);
				}
			}
		}
		return resultSet.toArray(new String[resultSet.size()]);
	}
	
	/**
	 * Get a Host name Verifier for HTTPS connection.
	 * 
	 * <p>
	 * This instance must set to the HttpsURLConnection manually.
	 * 
	 * @return a non null HostnameVerifier instance.
	 */
	public HostnameVerifier getHostnameVerifier() {
		if (verifyHostname) {
			// FIXME This rely on the default JVM implementation, which may be override !
			return HttpsURLConnection.getDefaultHostnameVerifier();
		}
		return new HostnameVerifier() {
			@Override
			public boolean verify(String urlhost, SSLSession session) {
				return true;
			}
		};
	}

	/**
	 * Return true if the peer host name must be verified.
	 * 
	 * @return
	 * @see #getHostnameVerifier()
	 * @see #getHostnameVerifierListener(String...)
	 */
	public boolean isVerifyHostname() {
		return verifyHostname;
	}
	
	/**
	 * Verify that one of the given host names matches the certificate principal.
	 * 
	 * <p>
	 * This listener must be added to the SSLSocket and is dependant from the current connection.
	 * 
	 * <p>
	 * If the verification fail a RuntimeException will be thrown.
	 * 
	 * @param peerHostname the peer host name has it in known when the connection is opened.
	 * @return Always return a valid listener, even if the verification is disabled.
	 */
	public HandshakeCompletedListener getHostnameVerifierListener(final String... peerHostname) {
		return new HandshakeCompletedListener() {
			@Override
			public void handshakeCompleted(HandshakeCompletedEvent event) {
				if (verifyHostname && (peerHostname.length > 0)) {
					HostnameVerifier verifier = getHostnameVerifier();
					for (String hn: peerHostname) {
						if (verifier.verify(hn, event.getSession())) {
							return;
						}
					}
					throw new RuntimeException("The TLS Certificate hostname does not match the target hostname : " + event.getSession().getPeerHost());
				}
			}
		};
	}

	/**
	 * 
	 * @param hostname
	 */
	public void setPeerHostnames(String... hostname) {
		((WrappedSockectFactory) context.getSocketFactory()).setPeerHostnames(hostname);
	}
	
	/**
	 * Return the TLS parameter to set to the Server or Client parameters class of the Restlet API.
	 * @return
	 */
	public Set<Entry<String, String>> getRestletParameters() {
		HashMap<String, String> parameters = new HashMap<String, String>();
		if (context != null) {
			String ksFileName = getProp(properties, PROP_KEYSTORE_PATH, null);
			if ((ksFileName != null) && new File(ksFileName).isFile()) {
				parameters.put("keyStorePath", new File(ksFileName).getAbsolutePath()); //$NON-NLS-1$
				String ksPwd = getProp(properties, PROP_KEYSTORE_PWD, null);
				if (ksPwd != null) {
					parameters.put("keyStorePassword", ksPwd); //$NON-NLS-1$
				}
				String kpwd = getProp(properties, PROP_KEYSTORE_KEYPWD, ksPwd);
				if (kpwd != null) {
					parameters.put("keyPassword", kpwd); //$NON-NLS-1$
				}
				parameters.put("keyStoreType", getProp(properties, PROP_KEYSTORE_TYPE, KeyStore.getDefaultType())); //$NON-NLS-1$
				parameters.put("keyManagerAlgorithm", getProp(properties, PROP_KEYSTORE_ALGO, DEFAULT_STOREALGO)); //$NON-NLS-1$
			}
			String tsFileName = getProp(properties, PROP_TRUSTSTORE_PATH, null);
			if ((tsFileName != null) && new File(tsFileName).isFile()) {
				parameters.put("trustStorePath", new File(tsFileName).getAbsolutePath()); //$NON-NLS-1$
				String tsPwd = getProp(properties, PROP_TRUSTSTORE_PWD, null);
				if (tsPwd != null) {
					parameters.put("trustStorePassword", tsPwd); //$NON-NLS-1$
				}
				parameters.put("trustStoreType", getProp(properties, PROP_TRUSTSTORE_TYPE, KeyStore.getDefaultType())); //$NON-NLS-1$
				parameters.put("trustManagerAlgorithm", getProp(properties, PROP_TRUSTSTORE_ALGO, DEFAULT_STOREALGO)); //$NON-NLS-1$
			}
			parameters.put("protocol", getProp(properties, PROP_SSL_PREFEREDPROTOCOL, "TLSv1.3")); //$NON-NLS-1$ //$NON-NLS-2$
			String s = getProp(properties, PROP_SECURERANDOM, "").trim(); //$NON-NLS-1$
			if (!s.isEmpty()) {
				parameters.put("secureRandomAlgorithm", s); //$NON-NLS-1$
			}
			s = getProp(properties, PROP_ENABLEDCIPHERSUITES, "").trim(); //$NON-NLS-1$
			if (!s.isEmpty()) {
				parameters.put("enabledCipherSuites", s); //$NON-NLS-1$
			}
			s = getProp(properties, PROP_DISABLEDCIPHERSUITES, "").trim(); //$NON-NLS-1$
			if (!s.isEmpty()) {
				parameters.put("disabledCipherSuites", s); //$NON-NLS-1$
			}
			s = getProp(properties, PROP_ENABLEDPROTOCOLS, "").trim(); //$NON-NLS-1$
			if (!s.isEmpty()) {
				parameters.put("enabledProtocols", s); //$NON-NLS-1$
			}
			s = getProp(properties, PROP_DISABLEDPROTOCOLS, "").trim(); //$NON-NLS-1$
			if (!s.isEmpty()) {
				parameters.put("disabledProtocols", s); //$NON-NLS-1$
			}
			parameters.put("needClientAuthentication", Boolean.toString(needClientAuth)); //$NON-NLS-1$
			parameters.put("wantClientAuthentication", Boolean.toString(wantClientAuth)); //$NON-NLS-1$
			parameters.put("sslContextFactory", "org.restlet.engine.ssl.DefaultSslContextFactory"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return parameters.entrySet();
	}
}
