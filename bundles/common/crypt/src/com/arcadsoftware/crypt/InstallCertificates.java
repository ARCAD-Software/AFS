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
package com.arcadsoftware.crypt;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

/**
 * This class can be used to retrieve un-trusted certificates from a server connection.
 * Present these certificates to the end-user and add them to the local keystore.
 * 
 * @author ARCAD Software
 */
public class InstallCertificates {
	
	/**
	 * There is no default Key store to load or to save the certificates to.
	 */
	public static final int ERROR_NO_DEFAULT_KEY_STORE_FOUND = 1;
	
	/**
	 * The file associated to the key store does not exist.
	 */
	public static final int ERROR_NO_KEY_STORE_FOUND = 2;
	
	/**
	 * An error occurs during the loading or the initialisation of the key store.
	 */
	public static final int ERROR_KEY_STORE_CREATION = 3;
	
	/**
	 * An error occurs when recordeing the key store to the file system.
	 */
	public static final int ERROR_KEY_STORE_SAVE = 4;

	/**
	 * The provided SSL Algorithm is not 
	 */
	public static final int ERROR_NO_SUCH_ALGORITHM = 5;
	
	/**
	 * The Key store can not be initialized, it may be corrupted or contain unsupported data.
	 */
	public static final int ERROR_KEY_STORE_INITIALISATION = 5;
	
	/**
	 * Error during the importation of the certificate into the key store. 
	 */
	public static final int ERROR_CERTIFICATE_IMPORT = 7;
	
	private String keystoreFileName;
	private String keystoreType;
	private char[] keystorePassPhrase;
	private String SSLAlgorithm;
	private String aliasPrefix;
	private int lastError;
	
	/**
	 * Create a default parameterized Certificate installer.
	 */
	public InstallCertificates() {
		this(null);
	}
	
	/**
	 * Create a Certificate Installer for the given Keys/Trust Store.
	 * 
	 * @param keystoreFileName A new or existant TrustStore.
	 */
	public InstallCertificates(String keystoreFileName) {
		this(keystoreFileName, null, null);
	}

	/**
	 * Create a Certificate Installer for the given Keys/Trust Store.
	 * 
	 * @param keystoreFileName A new or existent TrustStore.
	 * @param keystorePassPhrase the Store password.
	 * @param ketstoreType the store encoding type.
	 */
	public InstallCertificates(String keystoreFileName, char[] keystorePassPhrase, String keystoreType) {
		super();
		setAliasPrefix(null);
		setSSLAlgorithm(null);
		setKeystoreFileName(keystoreFileName);
		setKeystoreType(keystoreType);
		setKeystorePassPhrase(keystorePassPhrase);
	}
	
	private File getFile(String filename) {
		if (filename == null) {
			File file = new File("jssecacerts"); //$NON-NLS-1$
	        if (!file.isFile()) {
	            char SEP = File.separatorChar;
	            File dir = new File(System.getProperty("java.home") + SEP + //$NON-NLS-1$ 
	            		"lib" + SEP + "security"); //$NON-NLS-1$ //$NON-NLS-2$
	            file = new File(dir, "jssecacerts"); //$NON-NLS-1$
	            if (!file.isFile()) {
	                file = new File(dir, "cacerts"); //$NON-NLS-1$
	            }
	        }
            if (file.isFile()) {
            	return file;
            }
            lastError = ERROR_NO_DEFAULT_KEY_STORE_FOUND;
            return null;
		}
		return new File(filename);
	}
	
	/**
	 * Test the validity of the KeyStore (or the default keyStore).
	 * 
	 * @return null if everything is fine, or a error message.
	 */
	public String testKeyStore() {
		File file = getFile(keystoreFileName);
		if ((file == null) || !file.isFile()) {
			return "Incorrect KeyStore file name.";
		}
		try (FileInputStream in = new FileInputStream(file)) {
		       KeyStore ks = KeyStore.getInstance(keystoreType);
		       ks.load(in, keystorePassPhrase);
		} catch (Throwable e) {
			lastError = ERROR_KEY_STORE_CREATION;
			return e.getLocalizedMessage();
		}
		return null;
	}
	
	/**
	 * Test the validity of the KeyStore (or the default keyStore) and the 
	 * existence of the given key.
	 * 
	 * @param alias The private key alias.
	 * @param password The private key password.
	 * @return null if everything is fine, or a error message.
	 */
	public String testKeyStore(String alias, char[] password) {
		File file = getFile(keystoreFileName);
		if ((file == null) || !file.isFile()) {
			return "Incorrect KeyStore file name.";
		}
		try (FileInputStream in = new FileInputStream(file)) {
		       KeyStore ks = KeyStore.getInstance(keystoreType);
		       ks.load(in, keystorePassPhrase);
		       if (ks.getKey(alias, password) == null) {
		    	   return "Key not found.";
		       }
		} catch (Throwable e) {
			lastError = ERROR_KEY_STORE_CREATION;
			return e.getLocalizedMessage();
		}
		return null;
	}
	
	/**
	 * Load a Key Store with from the properties values.
	 * 
	 * @return
	 */
	public KeyStore loadKeyStore() {
		File file = getFile(keystoreFileName);
		if ((file == null) || !file.isFile()) {
			// Creer un nouveau KeyStore vierge.
			try {
		        KeyStore ks = KeyStore.getInstance(keystoreType);
		        ks.load(null, keystorePassPhrase);
		        return ks;
			} catch (Throwable e) {
				lastError = ERROR_KEY_STORE_CREATION;
			}
			return null;
		}
		// Changer le keystore.
		try {
			FileInputStream in = new FileInputStream(file);
			try {
		        KeyStore ks = KeyStore.getInstance(keystoreType);
		        ks.load(in, keystorePassPhrase);
		        return ks;
			} finally {
		        in.close();
			}
		} catch (Throwable e) {
			lastError = ERROR_KEY_STORE_CREATION;
			return null;
		}
	}
	
	/**
	 * Save the given Key Store to the relative properties.
	 * 
	 * @param ks The Key Stroe that will replace the key store file.
	 */
	public void saveKeyStore(KeyStore ks) {
		File file = getFile(keystoreFileName);
		if (file == null) {
	        lastError = ERROR_NO_KEY_STORE_FOUND;
		} else {
			try {
				if (!file.isFile()) {
					file.getParentFile().mkdirs();
				}
				FileOutputStream out = new FileOutputStream(file);
				try {
					ks.store(out, keystorePassPhrase);
				} finally {
					out.close();
				}
			} catch (Throwable e) {
				lastError = ERROR_KEY_STORE_SAVE;
			}
		}
	}
	
	/**
	 * Return the list of all the certificates from the key store.
	 * 
	 * @param ks
	 * @return
	 */
	public List<CertificateInformation> getCertificates() {
		return getCertificates(loadKeyStore());
	}
	
	/**
	 * Return the list of all the certificates from the key store.
	 * 
	 * @param ks
	 * @return
	 */
	public List<CertificateInformation> getCertificates(KeyStore ks) {
		if (ks == null) {
			return null;
		}
		ArrayList<CertificateInformation> result = new ArrayList<CertificateInformation>();
		try {
			Enumeration<String> aliases = ks.aliases();
			while(aliases.hasMoreElements()) {
				try {
					String alias = aliases.nextElement();
					Certificate cer = ks.getCertificate(alias);
					if (cer instanceof X509Certificate) {
						result.add(new CertificateInformation(alias, (X509Certificate) cer));
					}
				} catch (KeyStoreException e) {
				}
			}
		} catch (KeyStoreException e) {

		}
		return result;
	}
	
	/**
	 * Get a list of untrusted certificates.
	 * 
	 * <p>
	 * Trusted certificates are not included into this list.
	 * 
	 * @param server The server host name.
	 * @param port Socket connection port (0 for default HTTPS port number).
	 * @return An empty list if all certificates are trusted.
	 */
	public List<CertificateInformation> getUntrustedCertificates(String server, int port) {
		lastError = 0;
		if (port <= 0) {
			port = 443;
		}
		ArrayList<CertificateInformation> result = new ArrayList<CertificateInformation>();
		KeyStore ks = loadKeyStore();
		if (ks == null) {
			return result;
		}
		try {
			SSLContext context = SSLContext.getInstance(SSLAlgorithm);
	        TrustManagerFactory tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
	        tmf.init(ks);
	        X509TrustManager defaultTrustManager = (X509TrustManager) tmf.getTrustManagers()[0];
	        TrustAnyCertificateManager tm = new TrustAnyCertificateManager(defaultTrustManager, false);
	        context.init(null, new TrustManager[]{tm}, null);
	        SSLSocketFactory factory = context.getSocketFactory();
	        SSLSocket socket = (SSLSocket) factory.createSocket(server, port);
	        socket.setSoTimeout(10000);
	        try {
	            socket.startHandshake();
	            // All certificates are trusted : empty list is returned
	            return result;
	        } catch (SSLException e) {
	        	// There is some untrusted Certificates... 
	        	// Untrusted certificates are stored into tm.getChain()
	        } finally {
	            socket.close();
	        }
	        // TODO Filtrer les certificats qui seraient passés...
	        for(X509Certificate cert: tm.getChain()) {
	        	result.add(new CertificateInformation(cert));
	        }
		} catch (NoSuchAlgorithmException e) {
			lastError = ERROR_NO_SUCH_ALGORITHM;
		} catch (KeyStoreException e) {
			lastError = ERROR_KEY_STORE_INITIALISATION;
		} catch (Throwable e) {
		}
		return result;
	}
	
	/**
	 * Test the given certificate. If not trusted return the corresponding Certificate information.
	 * 
	 * @param certifFile An X509 Certificate file.
	 * @return An empty list if the certificate chain is trusted.
	 */
	public List<CertificateInformation> getUntrustedCertificates(File certifFile) {
		lastError = 0;
		ArrayList<CertificateInformation> result = new ArrayList<CertificateInformation>();
		X509Certificate[] chain = null;
		if ((certifFile == null) || !certifFile.isFile()) {
			return result;
		}
		try {
			try (InputStream inCertif = new FileInputStream(certifFile)) {
				CertificateFactory cf = CertificateFactory.getInstance("X.509");
				chain = new X509Certificate[] {(X509Certificate) cf.generateCertificate(inCertif) };
			}
		} catch (Exception e) {
			return result;
		}
		KeyStore ks = loadKeyStore();
		if (ks == null) {
			return result;
		}
		try {
	        TrustManagerFactory tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
	        tmf.init(ks);
	        X509TrustManager defaultTrustManager = (X509TrustManager) tmf.getTrustManagers()[0];
			try {
				defaultTrustManager.checkServerTrusted(chain, "RSA"); //$NON-NLS-1$
				return result;
			} catch (CertificateException e) {}
			for(X509Certificate cert: chain) {
				result.add(new CertificateInformation(cert));
		    }
		} catch (NoSuchAlgorithmException e) {
			lastError = ERROR_NO_SUCH_ALGORITHM;
		} catch (KeyStoreException e) {
			lastError = ERROR_KEY_STORE_INITIALISATION;
		} catch (Throwable e) {
		}
		return result;

	}

	/**
	 * Add the given Certificates to the Key store.
	 * 
	 * @param certificates
	 */
	public void trustCertificates(List<CertificateInformation> certificates) {
		lastError = 0;
		KeyStore ks = loadKeyStore();
		boolean changed = false;
		for(CertificateInformation cert: certificates) {
			try {
				ks.setCertificateEntry(getAlias(ks, cert.getAlias()), cert.getCertificate());
				changed = true;
			} catch (KeyStoreException e) {
				lastError = ERROR_CERTIFICATE_IMPORT;
			}
		}
		if (changed) {
			saveKeyStore(ks);
		}
	}

	/**
	 * Remove all certificate that match the current <b>aliasPrefix</b>.
	 */
	public void clear() {
		if (aliasPrefix == null) {
			return;
		}
		KeyStore ks = loadKeyStore();
		ArrayList<String> toRemove = new ArrayList<String>();
		try {
			Enumeration<String> e = ks.aliases();
			while(e.hasMoreElements()) {
				String alias = e.nextElement();
				if (alias.startsWith(aliasPrefix)) {
					toRemove.add(alias);
				}
			}
			for(String alias: toRemove) {
				ks.deleteEntry(alias);
			}
		} catch (KeyStoreException e) {

		}
		if (!toRemove.isEmpty()) {
			saveKeyStore(ks);
		}
	}

	private String getAlias(KeyStore ks, String prefix) {
		if (prefix != null) {
			try {
				if (!ks.containsAlias(prefix)) {
					return prefix;
				}
			} catch (KeyStoreException e) {

			}
		} else {
			prefix = aliasPrefix;
		}
		int i = 1;
		try {
			while (ks.containsAlias(prefix + i)) {
				i++;
			}
		} catch (KeyStoreException e) {
		}
		return prefix + i;
	}
	
	/**
	 * Get the KeyStore filename
	 * @return
	 */
	public String getKeystoreFileName() {
		return keystoreFileName;
	}

	/**
	 * Define the KeyStore file.
	 * @param keystoreFileName
	 */
	public void setKeystoreFileName(String keystoreFileName) {
		this.keystoreFileName = keystoreFileName;
	}

	/**
	 * Get the KeyStore type, default to the JVM default.
	 * @return
	 */
	public String getKeystoreType() {
		return keystoreType;
	}

	/**
	 * Define the KeyStore type, pass null to restore the default one (JVM default).
	 * 
	 * @param keystoreType
	 */
	public void setKeystoreType(String keystoreType) {
		if (keystoreType == null) {
			this.keystoreType = KeyStore.getDefaultType();
		} else {
			this.keystoreType = keystoreType;
		}
	}

	/**
	 * Get the KeyStore file password.
	 * @return
	 */
	public char[] getKeystorePassPhrase() {
		return keystorePassPhrase;
	}

	/**
	 * Define the KeyStore file password, pass null to restore the JVM default password.
	 * @param keystorePassPhrase
	 */
	public void setKeystorePassPhrase(char[] keystorePassPhrase) {
		if (keystorePassPhrase == null) {
			throw new NullPointerException("For security reasons, the Keystore Pass Phrase can not be empty.");
		}
		Crypto.clear(this.keystorePassPhrase);
		this.keystorePassPhrase = keystorePassPhrase;
	}

	/**
	 * Get the SSL Algorithm.
	 * @return
	 */
	public String getSSLAlgorithm() {
		return SSLAlgorithm;
	}

	/**
	 * Define the SSL Algorithn, pass null to restore default (TLS).
	 * @param sSLAlgorithm
	 */
	public void setSSLAlgorithm(String sSLAlgorithm) {
		if (sSLAlgorithm == null) {
			SSLAlgorithm = "TLS"; //$NON-NLS-1$
		} else {
			SSLAlgorithm = sSLAlgorithm;
		}
	}

	/**
	 * Get the Alias prefix use to store multiple trusted certificates.
	 * 
	 * @return
	 */
	public String getAliasPrefix() {
		return aliasPrefix;
	}

	/**
	 * Define the Alias prefix use to store multiple trusted certificates.
	 * 
	 * <p>
	 * Pass null to restore the default value ("ARCAD Import - ").
	 * @param aliasPrefix
	 */
	public void setAliasPrefix(String aliasPrefix) {
		if (aliasPrefix == null) {
			this.aliasPrefix = "ARCAD Import - "; //$NON-NLS-1$
		} else {
			this.aliasPrefix = aliasPrefix;
		}
	}

	/**
	 * Return last error code.
	 * 
	 * <p>
	 * See ERROR_* constants from this class.
	 * 
	 * @return zero if there is no error.
	 */
	public int getLastError() {
		return lastError;
	}
	
}
