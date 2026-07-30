/**
 * Copyright (c) 2026 ARCAD Software.
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
 * 
 */
package com.arcadsoftware.crypt;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.PrivateKey;
import java.security.Security;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.MGF1ParameterSpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.util.Random;
import java.util.zip.Deflater;
import java.util.zip.DeflaterOutputStream;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.CipherOutputStream;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.OAEPParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.PSource;
import javax.crypto.spec.SecretKeySpec;

import org.bouncycastle.jce.provider.BouncyCastleProvider;

/**
 * This OutPutStream allow to create an encrypted and compressed data stream, using 
 * the GZIP compression and an asymetric key for encryption.
 * 
 * @author ARCAD Software
 */
public class EncryptedOutputStream extends OutputStream {
	
	static {
		// Register the BouncyCastle provider.
		if (Security.getProperty(BouncyCastleProvider.PROVIDER_NAME) == null) {
			try {
				Security.addProvider(new BouncyCastleProvider());
			} catch (Exception e) {
				System.err.println("There is a problem with Bouncy Castle (AFS will fall back to JCE implementation): " + e.getLocalizedMessage());
			}
		}
	}

	private final OutputStream subStream;

	/**
	 * Create a new Stream allowing to encrypt a data, using the Master Key.
	 * 
	 * <p>
	 * <strong>Note that this encryption mode can not be shared with other application, and is less strong than the </strong>
	 * 
	 * @param stream The Up stream.
	 * @throws EncryptionException
	 * @throws IOException
	 */
	public EncryptedOutputStream(OutputStream stream) throws EncryptionException, IOException {
		this(Deflater.NO_COMPRESSION, stream);
	}

	/**
	 * Create a new Stream allowing tocompress and encrypt a data, using the Master Key.
	 * 
	 * <p>
	 * <strong>Note that this encryption mode can not be shared with other application, and is less strong than the </strong>
	 * 
	 * @param compression The compression level from 0; for no compression, to 9 best compression.
	 * @param stream The Up stream.
	 * @throws EncryptionException
	 * @throws IOException
	 */
	public EncryptedOutputStream(int compression, OutputStream stream) throws EncryptionException, IOException {
		super();
		SecretKeyFactory f;
		try {
			f = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
		} catch (NoSuchProviderException | SecurityException e1) {
			// This will not work...
			try {
				f = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256"); //$NON-NLS-1$
			} catch (NoSuchAlgorithmException e) {
				throw new EncryptionException(e);
			}
		} catch (NoSuchAlgorithmException e) {
			throw new EncryptionException(e);
		}
		char[] key = Crypto.getMK();
		byte[] kb = Crypto.getBytes(key);
		Crypto.reverse(key);
		byte[] iv = new byte[12];
		byte[] salt = new byte[kb.length - 12];
		try {
	        System.arraycopy(kb, 0, iv, 0, iv.length);
	        System.arraycopy(kb, iv.length, salt, 0, salt.length);
		} finally {
			Crypto.clear(key);
		}
		SecretKey secret;
		try {
			secret = f.generateSecret(new PBEKeySpec(key, salt, 1000, 256)); // key length for AES-256
		} catch (InvalidKeySpecException e) {
			throw new EncryptionException(e);
		}
		secret = new SecretKeySpec(secret.getEncoded(), "AES"); //$NON-NLS-1$
		Cipher cipher;
		try {
			cipher = Cipher.getInstance("AES/GCM/NOPADDING", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
		} catch (NoSuchProviderException | SecurityException e) {
			try {
				cipher = Cipher.getInstance("AES/GCM/NOPADDING"); //$NON-NLS-1$
			} catch (NoSuchAlgorithmException | NoSuchPaddingException e1) {
				throw new EncryptionException(e1);
			}
		} catch (NoSuchAlgorithmException | NoSuchPaddingException e) {
			throw new EncryptionException(e);
		}
		try {
			cipher.init(Cipher.ENCRYPT_MODE, secret, new GCMParameterSpec(128, iv), RandomGenerator.rn);
		} catch (InvalidKeyException | InvalidAlgorithmParameterException e) {
			throw new EncryptionException(e);
		}
		stream = new CipherOutputStream(stream, cipher);
		if (compression > Deflater.NO_COMPRESSION) {
			subStream = new DeflaterOutputStream(stream, new Deflater(compression, true), 512, false);
		} else {
			subStream = stream;
		}
	}
	
	/**
	 * Create a new Stream allowing to encrypt a data, using an assymetric key.
	 * 
	 * @param encryptionKey The Asymetric key used for encryption, an Encoded RSA Private key.
	 * @parem stream the target output stream. 
	 * @throws EncryptionException 
	 * @throws IOException 
	 * 
	 */
	public EncryptedOutputStream(byte[] encryptionKey, OutputStream stream) throws EncryptionException, IOException {
		this(encryptionKey, 500, Deflater.NO_COMPRESSION, stream);
	}
	
	/**
	 * Create a new Stream allowing to compress and encrypt a data, using an assymetric key.
	 * 
	 * @param encryptionKey The Asymetric key used for encryption, an Encoded RSA Private key.
	 * @param iterations The number of iteration of the cipher encryption.
	 * @param compression The compression level from 0; for no compression, to 9 best compression.
	 * @param stream The Up stream.
	 * @throws EncryptionException
	 * @throws IOException
	 */
	public EncryptedOutputStream(byte[] encryptionKey, int iterations, int compression, OutputStream stream) throws EncryptionException, IOException {
		super();
		// Generate the encryption key and write it in stream...
		Cipher cipher;
		try {
			cipher = Cipher.getInstance("RSA/ECB/OAEPWithSHA256AndMGF1Padding", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
		} catch (NoSuchProviderException | SecurityException e) {
			try {
				cipher = Cipher.getInstance("RSA/ECB/OAEPWithSHA256AndMGF1Padding"); //$NON-NLS-1$
			} catch (NoSuchAlgorithmException | NoSuchPaddingException e1) {
				throw new EncryptionException(e1);
			}
		} catch (NoSuchAlgorithmException | NoSuchPaddingException e) {
			throw new EncryptionException(e);
		}
		KeyFactory keyFactory;
		try {
			keyFactory = KeyFactory.getInstance("RSA", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
		} catch (NoSuchProviderException | SecurityException e) {
			try {
				keyFactory = KeyFactory.getInstance("RSA"); //$NON-NLS-1$
			} catch (NoSuchAlgorithmException e1) {
				throw new EncryptionException(e1);
			}
		} catch (NoSuchAlgorithmException e) {
			throw new EncryptionException(e);
		}
		OAEPParameterSpec oaepParameterSpec = new OAEPParameterSpec("SHA-256", "MGF1", MGF1ParameterSpec.SHA256, PSource.PSpecified.DEFAULT); //$NON-NLS-1$ //$NON-NLS-2$
		try {
			PrivateKey pk = keyFactory.generatePrivate(new PKCS8EncodedKeySpec(encryptionKey));
			cipher.init(Cipher.ENCRYPT_MODE, pk, oaepParameterSpec);
		} catch (InvalidKeySpecException | InvalidKeyException e) {
			throw new EncryptionException("The encryption key is invalid.", e);
		} catch (InvalidAlgorithmParameterException e) {
			throw new EncryptionException(e);
		}
		int bs = cipher.getBlockSize();
		if (bs < 80) {
			throw new EncryptionException("The encryption key length is too short.");
		}
		byte[] header = new byte[bs];
		new Random(RandomGenerator.randLong()).nextBytes(header);
		byte[] key = new byte[bs - 70];
		byte[] iv = new byte[12];
		byte[] salt = new byte[58];
        System.arraycopy(header, 0, iv, 0, iv.length);
        System.arraycopy(header, iv.length, key, 0, key.length);
        System.arraycopy(header, iv.length + key.length, salt, 0, salt.length);
		try {
			header = cipher.doFinal(header, 0, header.length);
		} catch (IllegalBlockSizeException | BadPaddingException e) {
			throw new EncryptionException(e);
		}
		stream.write(header);
		SecretKeyFactory f;
		try {
			f = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
		} catch (NoSuchProviderException | SecurityException e1) {
			// This will not work...
			try {
				f = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256"); //$NON-NLS-1$
			} catch (NoSuchAlgorithmException e) {
				throw new EncryptionException(e);
			}
		} catch (NoSuchAlgorithmException e) {
			throw new EncryptionException(e);
		}
		SecretKey secret;
		try {
			secret = f.generateSecret(new PBEKeySpec(new String(key, StandardCharsets.ISO_8859_1).toCharArray(), salt, iterations, 256)); // key length for AES-256
		} catch (InvalidKeySpecException e) {
			throw new EncryptionException(e);
		}
		secret = new SecretKeySpec(secret.getEncoded(), "AES"); //$NON-NLS-1$
		try {
			cipher = Cipher.getInstance("AES/GCM/NOPADDING", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
		} catch (NoSuchProviderException | SecurityException e) {
			try {
				cipher = Cipher.getInstance("AES/GCM/NOPADDING"); //$NON-NLS-1$
			} catch (NoSuchAlgorithmException | NoSuchPaddingException e1) {
				throw new EncryptionException(e1);
			}
		} catch (NoSuchAlgorithmException | NoSuchPaddingException e) {
			throw new EncryptionException(e);
		}
		try {
			cipher.init(Cipher.ENCRYPT_MODE, secret, new GCMParameterSpec(128, iv), RandomGenerator.rn);
		} catch (InvalidKeyException | InvalidAlgorithmParameterException e) {
			throw new EncryptionException(e);
		}
		stream = new CipherOutputStream(stream, cipher);
		if (compression > Deflater.NO_COMPRESSION) {
			subStream = new DeflaterOutputStream(stream, new Deflater(compression, true), 512, false);
		} else {
			subStream = stream;
		}
	}

	@Override
	public void write(int b) throws IOException {
		subStream.write(b);
	}

	@Override
	public void write(byte[] b) throws IOException {
		subStream.write(b);
	}

	@Override
	public void write(byte[] b, int off, int len) throws IOException {
		subStream.write(b, off, len);
	}

	@Override
	public void flush() throws IOException {
		subStream.flush();
	}

	@Override
	public void close() throws IOException {
		subStream.close();
	}

}
