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
package com.arcadsoftware.afs.client.server.connection;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
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

import com.arcadsoftware.aev.core.ui.tools.GuiFormatTools;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.crypt.CertificateInformation;
import com.arcadsoftware.crypt.TrustAnyCertificateManager;

public class SSLKeyStoreManager {
		
	private static final String[] CERTIFFILE_FILTERS = {"*.*"};
	
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
	private String keystorePassPhrase;
	private String SSLAlgorithm;
	private String aliasPrefix;
	private int lastError;
	private Throwable lastException;
	
	public SSLKeyStoreManager(String keyStoreFilename, String keyStorePassword) {
		super();
		setKeystoreFileName(keyStoreFilename);
		setKeystorePassPhrase(keyStorePassword);
		setAliasPrefix(null);
		setSSLAlgorithm(null);
		setKeystoreType(null);
	}
	
	private File getFile(String filename, boolean exists) {
		if (filename == null) {
			File file = new File("jssecacerts"); //$NON-NLS-1$
	        if (!file.isFile()) {
	            char SEP = File.separatorChar;
	            File dir = new File(System.getProperty("java.home") + SEP + "lib" + SEP + "security"); //$NON-NLS-1$  //$NON-NLS-2$  //$NON-NLS-3$
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
		File file = new File(filename);
		if (file.isFile() || !exists) {
			return file;
		}
        lastError = ERROR_NO_KEY_STORE_FOUND;
		return null;
	}
	
	/**
	 * Load a Key Store with from the properties values.
	 * 
	 * @return
	 */
	public KeyStore loadKeyStore() {
		File file = getFile(keystoreFileName, false);
		if (file == null) {
			return null;
		}
		if (!file.isFile()) {
			// Creer un nouveau KeyStore vierge.
			try {
		        KeyStore ks = KeyStore.getInstance(keystoreType);
		        ks.load(null, keystorePassPhrase.toCharArray());
		        return ks;
			} catch (Throwable e) {
				lastError = ERROR_KEY_STORE_CREATION;
				lastException = e;
				Activator.getInstance().error(e.getLocalizedMessage(),e);
			}
			return null;
		}
		// Changer le keystore.
		try {
			try (FileInputStream in = new FileInputStream(file)) {
		        KeyStore ks = KeyStore.getInstance(keystoreType);
		        ks.load(in, keystorePassPhrase.toCharArray());
		        return ks;
			} 
		} catch (Exception e) {
			lastError = ERROR_KEY_STORE_CREATION;
			Activator.getInstance().error(e.getLocalizedMessage(),e);
			return null;
		}
	}
	
	/**
	 * Save the given Key Store to the relative properties.
	 * 
	 * @param ks
	 */
	public boolean saveKeyStore(KeyStore ks) {
		File file = getFile(keystoreFileName, false);
		if (file != null) {
			try {
				if (!file.isFile()) {
					file.getParentFile().mkdirs();
				}
				FileOutputStream out = new FileOutputStream(file);
				try {
					ks.store(out, keystorePassPhrase.toCharArray());
					return true;
				} finally {
					out.close();
				}
			} catch (Exception e) {
				lastError = ERROR_KEY_STORE_SAVE;
				lastException = e;
				return false;
			}
		}
		return false;
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
			while (aliases.hasMoreElements()) {
				try {
					String alias = aliases.nextElement();
					Certificate cer = ks.getCertificate(alias);
					if (cer instanceof X509Certificate) {
						result.add(new CertificateInformation(alias, (X509Certificate) cer));
					}
				} catch (KeyStoreException e) {
					Activator.getInstance().error(e.getLocalizedMessage(),e);
				}
			}
		} catch (KeyStoreException e) {
			Activator.getInstance().error(e.getLocalizedMessage(),e);
		}
		return result;
	}
	
	/**
	 * Get a list of untrusted certificates.
	 * 
	 * <p>
	 * Trusted certificates are not included into this list.
	 * 
	 * @param server
	 * @param port
	 * @return
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
	        try (SSLSocket socket = (SSLSocket) factory.createSocket(server, port)){
	        	socket.setSoTimeout(10000);
	            socket.startHandshake();
	            // All certificates are trusted : empty list is returned
	            return result;
	        } catch (SSLException e) {
	        	// There is some untrusted Certificates... 
	        	// Untrusted certificates are stored into tm.getChain()
	        } 
	        // TODO Filtrer les certificats qui seraient passés...
	        for(X509Certificate cert: tm.getChain()) {
	        	result.add(new CertificateInformation(cert));
	        }
		} catch (NoSuchAlgorithmException e1) {
			lastError = ERROR_NO_SUCH_ALGORITHM;
			Activator.getInstance().error(e1.getLocalizedMessage(),e1);
		} catch (KeyStoreException e2) {
			lastError = ERROR_KEY_STORE_INITIALISATION;
			Activator.getInstance().error(e2.getLocalizedMessage(),e2);
		} catch (Exception e) {
			Activator.getInstance().error(e.getLocalizedMessage(),e);
		}
		return result;
	}
	
	/**
	 * Get a list of untrusted certificates from certificate files
	 * 
	 * <p>
	 * Trusted certificates are not included into this list.
	 * 
	 * @param server
	 * @param port
	 * @return
	 */
	public ArrayList<CertificateInformation> getUntrustedCertificates(File certifFile) {
		lastError = 0;
		ArrayList<CertificateInformation> result = new ArrayList<CertificateInformation>();
		Certificate certif = null;
		X509Certificate[] chain = null;
		if (certifFile != null && certifFile.isFile()){
			 try (InputStream inCertif = new FileInputStream(certifFile)) {
				
				CertificateFactory cf = CertificateFactory.getInstance("X.509");
				certif = cf.generateCertificate(inCertif);
				chain = new X509Certificate[]{(X509Certificate)certif};
			} catch (CertificateException | IOException e){
				return result;
			}
		}
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
			try {
				tm.checkServerTrusted(chain, "RSA");
			} catch (CertificateException e) {}
			for (X509Certificate cert: tm.getChain()) {
				result.add(new CertificateInformation(cert));
		    }
		} catch (NoSuchAlgorithmException e1) {
			lastError = ERROR_NO_SUCH_ALGORITHM;
			Activator.getInstance().error(e1.getLocalizedMessage(),e1);
		} catch (KeyStoreException e2) {
			lastError = ERROR_KEY_STORE_INITIALISATION;
			Activator.getInstance().error(e2.getLocalizedMessage(),e2);
		} catch (Exception e) {
			Activator.getInstance().error(e.getLocalizedMessage(),e);
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
				ks.setCertificateEntry(getAlias(ks, cert), cert.getCertificate());
				changed = true;
			} catch (KeyStoreException e) {
				lastError = ERROR_CERTIFICATE_IMPORT;
				lastException = e;
				Activator.getInstance().error(e.getLocalizedMessage(),e);
			}
		}
		if (changed) {
			saveKeyStore(ks);
		}
	}
	
	/**
	 * Add the given Certificates to the Key store.
	 * 
	 * @param certificates
	 */
	public boolean trustCertificate(CertificateInformation certificate) {
		lastError = 0;
		KeyStore ks = loadKeyStore();
		try {
			ks.setCertificateEntry(getAlias(ks, certificate), certificate.getCertificate());			
			return saveKeyStore(ks);
		} catch (KeyStoreException e) {			
			lastError = ERROR_CERTIFICATE_IMPORT;
			lastException = e;
			return false;
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
			Activator.getInstance().error(e.getLocalizedMessage(),e);
		}
		if (!toRemove.isEmpty()) {
			saveKeyStore(ks);
		}
	}

	private String getAlias(KeyStore ks, CertificateInformation certificate) {
		String prefix = certificate.getAlias();
		if(prefix == null) {
			prefix = certificate.getSubject();
			if(prefix != null) {
				prefix = prefix.replace("CN=", "");
			}
		}
		if (prefix != null) {
			try {
				if (!ks.containsAlias(prefix)) {
					return prefix;
				}
			}
			catch (KeyStoreException e) {
				Activator.getInstance().error(e.getLocalizedMessage(),e);
			}
		}
		setAliasPrefix(prefix);
		prefix = getAliasPrefix();
		int i = 1;
		try {
			while (ks.containsAlias(prefix + i)) {
				i++;
			}
		} catch (KeyStoreException e) {
			Activator.getInstance().error(e.getLocalizedMessage(),e);
		}
		return prefix + i;
	}
	
	public String getKeystoreFileName() {
		return keystoreFileName;
	}

	public void setKeystoreFileName(String keystoreFileName) {
		this.keystoreFileName = keystoreFileName;
	}

	public String getKeystoreType() {
		return keystoreType;
	}

	public void setKeystoreType(String keystoreType) {
		if (keystoreType == null) {
			this.keystoreType = KeyStore.getDefaultType();
		} else {
			this.keystoreType = keystoreType;
		}
	}

	public String getKeystorePassPhrase() {
		return keystorePassPhrase;
	}

	public void setKeystorePassPhrase(String keystorePassPhrase) {
		this.keystorePassPhrase = keystorePassPhrase;
	}

	public String getSSLAlgorithm() {
		return SSLAlgorithm;
	}

	public void setSSLAlgorithm(String sSLAlgorithm) {
		if (sSLAlgorithm == null) {
			SSLAlgorithm = "TLS"; //$NON-NLS-1$
		} else {
			SSLAlgorithm = sSLAlgorithm;
		}
	}

	public String getAliasPrefix() {
		return aliasPrefix;
	}

	public void setAliasPrefix(String aliasPrefix) {
		if (aliasPrefix == null) {
			this.aliasPrefix = "ARCAD Import - "; //$NON-NLS-1$
		} else {
			this.aliasPrefix = aliasPrefix;
		}
	}

	public int getLastError() {
		return lastError;
	}
	
	public Throwable getLastException() {
		return lastException;
	}
	
	public ArrayList<CertificateInformation>  listCertificates(File certificateFile) {
		ArrayList<CertificateInformation> certificates = getUntrustedCertificates(certificateFile);
		if ((lastError > 0) || (certificates == null) || (certificates.size() == 0)) {
			return null;
		}
		return certificates;
	}
	
	public File chooseCertificateFile() {
		// Open File selector
		String certificateFile = GuiFormatTools.choosefile(Activator.resString("server.connection.certificate.accept.title"), CERTIFFILE_FILTERS);
		if ((certificateFile == null) || certificateFile.isEmpty()) {
			return null;
		}
		return new File(certificateFile);
	}
}
