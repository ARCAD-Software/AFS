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
package com.arcadsoftware.crypt;

import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.security.Key;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.PublicKey;
import java.security.Security;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;
import java.time.Duration;
import java.time.Instant;
import java.util.Date;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;

import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.oiw.OIWObjectIdentifiers;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.BasicConstraints;
import org.bouncycastle.asn1.x509.Extension;
import org.bouncycastle.asn1.x509.SubjectKeyIdentifier;
import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo;
import org.bouncycastle.cert.X509ExtensionUtils;
import org.bouncycastle.cert.X509v3CertificateBuilder;
import org.bouncycastle.cert.jcajce.JcaX509CertificateConverter;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.operator.ContentSigner;
import org.bouncycastle.operator.DigestCalculator;
import org.bouncycastle.operator.OperatorCreationException;
import org.bouncycastle.operator.bc.BcDigestCalculatorProvider;
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder;

/**
 * This utility class allow to facilitate the management of KeyStore content.
 * 
 * @author ARCAD Software
 */
public class KeyStoreManager implements Closeable {

	private final static String DEFAULT_KEYSTORE_TYPE = "PKCS12"; //$NON-NLS-1$ 

	static {
		// Register the BouncyCastle provider.
		if (Security.getProperty(BouncyCastleProvider.PROVIDER_NAME) == null) {
			Security.addProvider(new BouncyCastleProvider());
		}
	}
	
	private final String storePath;
	private final char[] storePass;
	private final KeyStore keystore;
	
	/**
	 * Create a new KeyStore manager.
	 * 
	 * @param storePath the path to the new or existing KeyStore file.
	 * @param storePass the password required to open this pifle (can be null).
	 * @param storeType The Key Store file type.
	 * @throws KeyManagerException if there is an error during the keystore creation or loading..
	 */
	public KeyStoreManager(String storePath, char[] storePass, String storeType) throws KeyManagerException {
		super();
		this.storePath = storePath;
		this.storePass = storePass;
		keystore = loadKeyStore(storePath, storePass, storeType);
	}

	/**
	 * Create a new KeyStore manager using the standart PKCS12 type.
	 * 
	 * @param storePath the path to the new or existing KeyStore file.
	 * @param storePass the password required to open this pifle (can be null).
	 * @throws KeyManagerException if there is an error during the keystore creation or loading..
	 */
	public KeyStoreManager(String storePath, char[] storePass) throws KeyManagerException {
		this(storePath, storePass, DEFAULT_KEYSTORE_TYPE);
	}
	
	/**
	 * Create a new KeyStore manager using the standart PKCS12 type and an empty password.
	 * 
	 * @param storePath the path to the new or existing KeyStore file.
	 * @param storePass the password required to open this pifle (can be null).
	 * @throws KeyManagerException if there is an error during the keystore creation or loading..
	 */
	public KeyStoreManager(String storePath) throws KeyManagerException {
		this(storePath, null, DEFAULT_KEYSTORE_TYPE);
	}
	
	private KeyStore loadKeyStore(String storePath, char[] storePass, String storeType) throws KeyManagerException {
		if (storePath == null) {
			try {
		        KeyStore ks = KeyStore.getInstance(storeType, BouncyCastleProvider.PROVIDER_NAME);
		        ks.load(null, storePass);
		        return ks;
			} catch (Exception e) {
				throw new KeyManagerException(e);
			}
		}
		File file = new File(storePath);
		if (!file.isFile()) {
			try {
		        KeyStore ks = KeyStore.getInstance(storeType, BouncyCastleProvider.PROVIDER_NAME);
		        ks.load(null, storePass);
		        return ks;
			} catch (Exception e) {
				throw new KeyManagerException(e);
			}
		}
		try {
			FileInputStream in = new FileInputStream(file);
			try {
		        KeyStore ks = KeyStore.getInstance(storeType, BouncyCastleProvider.PROVIDER_NAME);
		        ks.load(in, storePass);
		        return ks;
			} finally {
		        in.close();
			}
		} catch (Exception e) {
			throw new KeyManagerException(e);
		}
	}
	
	private Certificate[] generateCertChain(KeyPair keyPair, int validityDays) throws Exception {
		final Instant now = Instant.now();
	    final Date notBefore = Date.from(now);
	    final Date notAfter = Date.from(now.plus(Duration.ofDays(validityDays)));
	    final X500Name x500Name = new X500Name("CN=arcad-software");
	    final X509v3CertificateBuilder certificateBuilder = new X509v3CertificateBuilder(x500Name,
	        BigInteger.valueOf(now.toEpochMilli()),
	        notBefore,
	        notAfter,
	        x500Name,
	        SubjectPublicKeyInfo.getInstance(ASN1Sequence.getInstance(keyPair.getPublic().getEncoded())));
	    certificateBuilder.addExtension(Extension.subjectKeyIdentifier, false, createSubjectKeyId(keyPair.getPublic()));
	    //certificateBuilder.addExtension(Extension.authorityKeyIdentifier, false, createAuthorityKeyId(keyPair.getPublic()));
	    certificateBuilder.addExtension(Extension.basicConstraints, true, new BasicConstraints(true));
	    //certificateBuilder.addExtension(Extension.subjectAlternativeName, false, new GeneralNames(new GeneralName(GeneralName.dNSName, domainName)));
	    final JcaContentSignerBuilder contentSignerBuilder = new JcaContentSignerBuilder("SHA256WithRSAEncryption");
	    final ContentSigner contentSigner = contentSignerBuilder.build(keyPair.getPrivate());
	    return new Certificate[] { new JcaX509CertificateConverter().getCertificate(certificateBuilder.build(contentSigner))};
	}
	
	private SubjectKeyIdentifier createSubjectKeyId(final PublicKey publicKey) throws KeyManagerException {
	    final SubjectPublicKeyInfo publicKeyInfo = SubjectPublicKeyInfo.getInstance(publicKey.getEncoded());
		try {
		    DigestCalculator digCalc = new BcDigestCalculatorProvider().get(new AlgorithmIdentifier(OIWObjectIdentifiers.idSHA1));
		    return new X509ExtensionUtils(digCalc).createSubjectKeyIdentifier(publicKeyInfo);
		} catch (OperatorCreationException e) {
			throw new KeyManagerException(e);
		}
	  }

	/**
	 * Generate a new Key Pair (Private and public key) and store it into the KeyStore.
	 * 
	 * @param alias The alias associated to this Key Pair.
	 * @param keyPass the password used to protect the access to the Private Key.
	 * @param algorithm the algorithm used to generate the key.
	 * @param size the bite size of the key.
	 * @param validityDays the validity limit (in days) of the key.
	 * @throws KeyManagerException
	 */
	public void generateNewKeyPair(String alias, char[] keyPass, String algorithm, int size, int validityDays) throws KeyManagerException {
		try {
			KeyPairGenerator keyGen = KeyPairGenerator.getInstance(algorithm, BouncyCastleProvider.PROVIDER_NAME);
	        keyGen.initialize(size); //4096);
	        KeyPair kp = keyGen.generateKeyPair();
	        keystore.setKeyEntry(alias, kp.getPrivate(), keyPass, generateCertChain(kp, validityDays));
		} catch (Exception e) {
			throw new KeyManagerException(e);
		}
	}

	/**
	 * Generate a new Key Pair (Private and public key) and store it into the KeyStore. The algorithm used is RSA with a 1024 key 
	 * size and a limit of validity set to 10 years.
	 * 
	 * @param alias The alias associated to this Key Pair.
	 * @param keyPass the password used to protect the access to the Private Key.
	 * @throws KeyManagerException
	 */
	public void generateNewKeyPair(String alias, char[] keyPass) throws KeyManagerException {
		generateNewKeyPair(alias, keyPass, "RSA", 1024, 3650);
	}
	
	/**
	 * Export a public key to another KeyStore.
	 * 
	 * <p>
	 * Note that the destination keystore must be different from the surrent one.  
	 *  
	 * @param alias The alias of the key to export (the same alias will be used in the destination).
	 * @param storePath the new, or existing, keystore pass.
	 * @param storePass the password used to open this key store.
	 * @param storeType the type of this key store.
	 * @throws KeyManagerException
	 */
	public void exportPublicKey(String alias, String storePath, char[] storePass, String storeType) throws KeyManagerException {
		KeyStore trustStore = loadKeyStore(storePath, storePass, storeType);
		try {
			Certificate[] certchain = keystore.getCertificateChain(alias);
			if (certchain == null) {
				throw new KeyManagerException("No certificate chain found for the alias: " + alias);
			}
			trustStore.setCertificateEntry(alias, certchain[0]);
		} catch (Exception e) {
			throw new KeyManagerException(e);
		}
		saveKeyStore(trustStore, storePath, storePass);
	}
	
	/**
	 * Export a public key to another KeyStore using the default key store type PKCS12.
	 * 
	 * <p>
	 * Note that the destination keystore must be different from the current one.  
	 *  
	 * @param alias The alias of the key to export (the same alias will be used in the destination).
	 * @param storePath the new, or existing, keystore pass.
	 * @param storePass the password used to open this key store.
	 * @throws KeyManagerException
	 */
	public void exportPublicKey(String alias, String storePath, char[] storePass) throws KeyManagerException {
		exportPublicKey(alias, storePath, storePass, DEFAULT_KEYSTORE_TYPE);
	}
	
	/**
	 * Export a public key to another KeyStore using the default key store type PKCS12 and an empty password.
	 * 
	 * <p>
	 * Note that the destination keystore must be different from the surrent one.  
	 *  
	 * @param alias The alias of the key to export (the same alias will be used in the destination).
	 * @param storePath the new, or existing, keystore pass.
	 * @throws KeyManagerException
	 */
	public void exportPublicKey(String alias, String storePath) throws KeyManagerException {
		exportPublicKey(alias, storePath, null, DEFAULT_KEYSTORE_TYPE);
	}
	
	/**
	 * Get the private key associated with the given alias.
	 *  
	 * @param alias the alias of the private key.
	 * @param keypass the key password
	 * @return
	 * @throws KeyManagerException if an error occurs or if the key is not found, or is not a private key.
	 */
	public byte[] getPrivateKey(String alias, char[] keypass) throws KeyManagerException {
		try {
			Key key = keystore.getKey(alias, keypass);
			if (key instanceof PrivateKey) {
				return ((PrivateKey) key).getEncoded();
			}
		} catch (Exception e) {
			throw new KeyManagerException(e);
		}
		throw new KeyManagerException("No Private Key found for alias: " + alias);
	}
	
	/**
	 * Get the secret key associated with the given alias.
	 *  
	 * @param alias the alias of the secret key.
	 * @param keypass the key password
	 * @return
	 * @throws KeyManagerException if an error occurs or if the key is not found, or is not a secret key.
	 */
	public byte[] getSecretKey(String alias, char[] keypass) throws KeyManagerException {
		try {
			Key key = keystore.getKey(alias, keypass);
			if (key instanceof SecretKey) {
				return ((SecretKey) key).getEncoded();
			}
		} catch (Exception e) {
			throw new KeyManagerException(e);
		}
		throw new KeyManagerException("No Secret Key found for alias: " + alias);
	}
	
	/* Get the public key associated with the given alias.
	 *  
	 * @param alias the alias of the public key.
	 * @param keypass the key password
	 * @return
	 * @throws KeyManagerException if an error occurs or if the key is not found.
	 */
	public byte[] getPublicKey(String alias) throws KeyManagerException {
		try {
			X509Certificate cert = (X509Certificate) keystore.getCertificate(alias);
			cert.checkValidity();
			return cert.getPublicKey().getEncoded();
		} catch (Exception e) {
			throw new KeyManagerException(e);
		}
	}

	/**
	 * Get the certificate of a public key.
	 * 
	 * @param alias
	 * @return
	 * @throws KeyManagerException
	 */
	public Certificate getCertificate(String alias) throws KeyManagerException {
		try {
			X509Certificate cert = (X509Certificate) keystore.getCertificate(alias);
			cert.checkValidity();
			return cert;
		} catch (Exception e) {
			throw new KeyManagerException(e);
		}
	}

	/**
	 * Store a private Key in the keystore (the public key is required to be able to store a private key in a key store.
	 * 
	 * @param alias
	 * @param privateKey
	 * @param publicKey
	 * @param pass
	 * @param validityDays
	 * @throws KeyManagerException
	 */
	public void putPrivateKey(String alias, byte[] privateKey, byte[] publicKey, char[] pass, int validityDays) throws KeyManagerException {
		try {
		    PublicKey pubKey = KeyFactory.getInstance("RSA", BouncyCastleProvider.PROVIDER_NAME).generatePublic(new X509EncodedKeySpec(publicKey));
		    PrivateKey priKey = KeyFactory.getInstance("RSA", BouncyCastleProvider.PROVIDER_NAME).generatePrivate(new PKCS8EncodedKeySpec(privateKey));
			KeyPair kp = new KeyPair(pubKey, priKey);
		    keystore.setKeyEntry(alias, kp.getPrivate(), pass, generateCertChain(kp, validityDays));
		} catch (Exception e) {
			throw new KeyManagerException(e);
		}
	}
	
	public void putSecretKey(String alias, byte[] key, char[] pass) throws KeyManagerException {
		try {
			keystore.setEntry(alias, new KeyStore.SecretKeyEntry(new SecretKeySpec(key, "AES")), new KeyStore.PasswordProtection(pass));
		} catch (KeyStoreException e) {
			throw new KeyManagerException(e);
		}
	}
	
	public void putCertificate(String alias, Certificate cert) throws KeyManagerException {
		try {
			keystore.setCertificateEntry(alias, cert);
		} catch (KeyStoreException e) {
			throw new KeyManagerException(e);
		}
	}
	
	@Override
	public void close() throws IOException {
		try {
			save();
		} catch (KeyManagerException e) {
			throw new IOException(e);
		}
	}
	
	/**
	 * Save the Key Store.
	 * 
	 * @throws KeyManagerException
	 * @see #close()
	 */
	public void save() throws KeyManagerException {
		saveKeyStore(keystore, storePath, storePass);
	}
	
	/**
	 * Export the current Key Store to a different location..
	 * 
	 * @param storePath
	 * @param storePass
	 * @throws KeyManagerException
	 */
	public void saveTo(String storePath, char[] storePass) throws KeyManagerException {
		saveKeyStore(keystore, storePath, storePass);
	}

	private void saveKeyStore(KeyStore keystore, String storePath, char[] storePass) throws KeyManagerException {
		if (storePath != null) {
			try {
				File file = new File(storePath);
				if (!file.isFile()) {
					file.getParentFile().mkdirs();
				}
				FileOutputStream out = new FileOutputStream(file);
				try {
					keystore.store(out, storePass);
				} finally {
					out.close();
				}
			} catch (Exception e) {
				throw new KeyManagerException(e);
			}
		}
	}

	/**
	 * @return the path to the KeyStore file, may be null.
	 */
	public String getStorePath() {
		return storePath;
	}

	/**
	 * @return the KeyStore password, may be null.
	 */
	public char[] getStorePass() {
		return storePass;
	}

	/**
	 * @return the KeyStore associated to this manager.
	 */
	public KeyStore getKeystore() {
		return keystore;
	}
	
}
