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
package com.arcadsoftware.afs.client.core.connection;

/**
 * This class is used to represent all the parameters for restlet SSL configuration.
 * The following documentation is derived from restlet own documentation.
 * @author ARCAD Software
 */
public interface ITrustStoreProvider {
	
	/**
	 * Returns the path to the trust store file. The trust store path defaults to the system property "javax.net.ssl.trustStore".
	 * @return the path to the trust store file
	 */
	public String getTrustStorePath();	
	
	/**
	 * Sets the path to the trust store. The trust store path defaults to the system property "javax.net.ssl.trustStore".
	 * @param path the path of the trust store to set
	 */
	public void setTrustStorePath(String path);
	
	/**
	 * Returns the password for the trust store. The trust store password defaults to the system property "javax.net.ssl.trustStorePassword".
	 * @return the password for the trust store
	 */
	public char[] getTrustStorePassword();
	
	/**
	 * Sets the password of the trust store. The trust store password defaults to the system property "javax.net.ssl.trustStorePassword".
	 * @param password the password of the trust store to set
	 */
	public void setTrustStorePassword(char[] password);

	/**
	 * Returns the path to the key store file. The key store path defaults to the system property "javax.net.ssl.keyStore" or ${user.home}/.keystore.
	 * @return the path to the key store file
	 */
	public String getKeyStorePath();
	
	/**
	 * Sets the path to the key store file. The key store path defaults to the system property "javax.net.ssl.keyStore" or ${user.home}/.keystore.
	 * @param path the path to the key store file
	 */
	public void setKeyStorePath(String path);

	/**
	 * Returns the password for the key store. The key store password defaults to the system property "javax.net.ssl.keyStorePassword".
	 * @return the password for the key store
	 */
	public char[] getKeyStorePassword();
	
	/**
	 * Sets the key store password. The key store password defaults to the system property "javax.net.ssl.keyStorePassword".
	 * @param password the key store password
	 */
	public void setKeyStorePassword(char[] password);
	
	/**
	 * Returns the password for the key in the key store. The key password defaults to the system property "javax.net.ssl.keyPassword", falling back to javax.net.ssl.keyStorePassword (this system property name is not standard).
	 * @return the password for the key in the key store 
	 */
	public char[] getKeyPassword();
	
	/**
	 * Sets the password of the key in the key store. The key password defaults to the system property "javax.net.ssl.keyPassword", falling back to javax.net.ssl.keyStorePassword (this system property name is not standard).
	 * @param password the password of the key in the key store
	 */
	public void setKeyPassword(char[] password);
	
	/**
	 * Returns the type of the key store. The key store type defaults to the system property "javax.net.ssl.keyStoreType".
	 * @return the type of the key store
	 */
	public String getKeyStoreType();
	
	/**
	 * Sets the type of the key store. The key store type defaults to the system property "javax.net.ssl.keyStoreType".
	 * @param type the type of the key store
	 */
	public void setKeyStoreType(String type);
	
	/**
	 * Returns the type of the trust store. The trust store type defaults to the system property "javax.net.ssl.trustStoreType".
	 * @return the type of the trust store
	 */
	public String getTrustStoreType();
	
	/**
	 * Sets the type of the trust store. The trust store type defaults to the system property "javax.net.ssl.trustStoreType".
	 * @param type the type of the trust store
	 */
	public void setTrustStoreType(String type);

	/**
	 * Returns the name of the certificate algorithm for the key manager. It defaults to the system property "ssl.KeyManagerFactory.algorithm" or "SunX509" if the system property has not been set up.
	 * @return the name of the KeyManager algorithm
	 */
	public String getKeyManagerAlgorithm();
	
	/**
	 * Sets the certificate algorithm for the key manager. It defaults to the system property "ssl.KeyManagerFactory.algorithm" or "SunX509" if the system property has not been set up.
	 * @param keyAlgorithm the KeyManager algorithm
	 */
	public void setKeyManagerAlgorithm(String keyAlgorithm);

	/**
	 * Returns the name of the certificate algorithm for the trust manager. It defaults to the system property "ssl.TrustManagerFactory.algorithm" or "SunX509" if the system property has not been set up.
	 * @return the name of the TrustManager algorithm
	 */
	public String getTrustManagerAlgorithm();
	
	/**
	 * Sets the certificate algorithm for the trust manager. It defaults to the system property "ssl.TrustManagerFactory.algorithm" or "SunX509" if the system property has not been set up.
	 * @param trustAlgorithm the TrustManager algorithm
	 */
	public void setTrustManagerAlgorithm(String trustAlgorithm);

	/**
	 * Returns the whitespace-separated list of disabled cipher suites. It affects the cipher suites manually enabled or the default ones.
	 * @return the whitespace-separated list of disabled cipher suites
	 */
	public String getDisabledCipherSuites();
	
	/**
	 * Sets the list of disabled cipher suites. It affects the cipher suites manually enabled or the default ones.
	 * @param disabledCiphers the whitespace-separated list of disabled cipher suites
	 */
	public void setDisabledCipherSuites(String disabledCiphers);

	/**
	 * Returns the whitespace-separated list of disabled SSL/TLS protocol names. It is used when creating SSL sockets and engines.
	 * @return the whitespace-separated list of disabled protocols
	 */
	public String getDisabledProtocols();
	
	/**
	 * Sets the list of disabled SSL/TLS protocol names. It is used when creating SSL sockets and engines.
	 * @param disabledProtocols the whitespace-separated list of disabled SSL protocols
	 */
	public void setDisabledProtocols(String disabledProtocols);

	/**
	 * Returns the whitespace-separated list of enabled cipher suites. 
	 * @return the whitespace-separated list of enabled cipher suites
	 */
	public String getEnabledCipherSuites();
	
	/**
	 * Sets the list of enabled cipher suites.
	 * @param enabledCiphers the whitespace-separated list of enabled cipher suites
	 */
	public void setEnabledCipherSuites(String enabledCiphers);
	
	/**
	 * Returns the whitespace-separated list of enabled SSL/TLS protocol names. It is used when creating SSL sockets and engines.
	 * @return the whitespace-separated list of enabled protocols
	 */
	public String getEnabledProtocols();

	/**
	 * Sets the list of enabled SSL/TLS protocol names. It is used when creating SSL sockets and engines.
	 * @param enabledProtocols the whitespace-separated list of enabled SSL protocols
	 */
	public void setEnabledProtocols(String enabledProtocols);

	/**
	 * Returns the SSL protocol used when creating the SSLContext, "TLS" by default.
	 * @return the secure socket protocol name
	 */
	public String getProtocol();
	
	/**
	 * Sets the secure socket protocol name, "TLS" by default.
	 * @param protocol the name of the secure socket protocol to use
	 */
	public void setProtocol(String protocol);

	/**
	 * Returns the name of the SecureRandom algorithm. The default value is null, in which case the default SecureRandom would be used.
	 * @return the name of the SecureRandom algorithm
	 */
	public String getSecureRandomAlgorithm();
	
	/**
	 * Sets the SecureRandom algorithm. The default value is null, in which case the default SecureRandom would be used.
	 * @param randomAlgorithm the SecureRandom algorithm
	 */
	public void setSecureRandomAlgorithm(String randomAlgorithm);
	
	/**
	 * Removes all previously defined parameters to fall back in the default configuration.
	 */
	public void resetToDefault();
	
	/**
	 * Saves the current configuration.
	 * @return true if the save was successful, false otherwise
	 */
	public boolean save();
	
}
