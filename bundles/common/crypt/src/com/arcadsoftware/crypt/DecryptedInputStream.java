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
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.PublicKey;
import java.security.Security;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.MGF1ParameterSpec;
import java.security.spec.X509EncodedKeySpec;
import java.util.zip.Inflater;
import java.util.zip.InflaterInputStream;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.CipherInputStream;
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
 * This InPutStream allow to read an encrypted and compressed data stream, using 
 * the GZIP compression and an asymetric key for encryption.
 * 
 * @author ARCAD Software
 */
public class DecryptedInputStream extends InputStream {
	
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
	
	private final InputStream subStream;

	/**
	 * Create a new Stream allowing to decrypt a data, using the Master Key.
	 * 
	 * <p>
	 * <strong>Note that this encryption mode can not be shared with other application, and is less strong than the </strong>
	 * 
	 * @param stream The Up stream.
	 * @throws EncryptionException
	 * @throws IOException
	 */
	public DecryptedInputStream(InputStream stream) throws EncryptionException, IOException {
		this(false, stream);
	}

	/**
	 * Create a new Stream allowing to decompress and decrypt a data, using the Master Key.
	 * 
	 * <p>
	 * <strong>Note that this encryption mode can not be shared with other application, and is less strong than the </strong>
	 * 
	 * @param compression True if the upstream must be decompressed.
	 * @param stream The Up stream.
	 * @throws EncryptionException
	 * @throws IOException
	 */
	public DecryptedInputStream(boolean compression, InputStream stream) throws EncryptionException, IOException {
		super();
		SecretKeyFactory f;
		try {
			f = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
		} catch (NoSuchProviderException | SecurityException e1) {
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
			secret = f.generateSecret(new PBEKeySpec(key, salt, 1000, 256)); // key for AES-256
		} catch (InvalidKeySpecException e) {
			throw new EncryptionException(e);
		}
		secret = new SecretKeySpec(secret.getEncoded(), "AES");
		Cipher cipher;
		try {
			cipher = Cipher.getInstance("AES/GCM/NOPADDING", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
		} catch (NoSuchProviderException | SecurityException e1) {
			try {
				cipher = Cipher.getInstance("AES/GCM/NOPADDING"); //$NON-NLS-1$
			} catch (NoSuchAlgorithmException | NoSuchPaddingException e) {
				throw new EncryptionException(e);
			}
		} catch (NoSuchAlgorithmException | NoSuchPaddingException e) {
			throw new EncryptionException(e);
		}
		try {
			cipher.init(Cipher.DECRYPT_MODE, secret, new GCMParameterSpec(128, iv), RandomGenerator.rn);
		} catch (InvalidKeyException | InvalidAlgorithmParameterException e) {
			throw new EncryptionException(e);
		}
		stream = new CipherInputStream(stream, cipher);
		if (compression) {
			subStream = new InflaterInputStream(stream, new Inflater(true), 512);
		} else {
			subStream = stream;
		}
	}
	
	/**
	 * Create a new Stream allowing to decompress and decrypt a data, using an assymetric key.
	 * 
	 * @param decryptionKey The Asymetric key used for encryption, an Encoded RSA Public key.
	 * @param stream The Up stream.
	 * @throws EncryptionException
	 * @throws IOException
	 */
	public DecryptedInputStream(byte[] decryptionKey, InputStream stream) throws EncryptionException, IOException {
		this(decryptionKey, 500, true, stream);
	}

	/**
	 * Create a new Stream allowing to decompress and decrypt a data, using an assymetric key.
	 * 
	 * @param decryptionKey The Asymetric key used for encryption, an Encoded RSA Public key.
	 * @param iterations The number of iteration of the cipher encryption.
	 * @param compression True if the upstream must be decompressed.
	 * @param stream The Up stream.
	 * @throws EncryptionException
	 * @throws IOException
	 */
	public DecryptedInputStream(byte[] decryptionKey, int iterations, boolean compression, InputStream stream) throws EncryptionException, IOException {
		super();
		// Read the encryption key from the stream...
		KeyFactory keyFactory;
		try {
			keyFactory = KeyFactory.getInstance("RSA", BouncyCastleProvider.PROVIDER_NAME);
		} catch (NoSuchProviderException | SecurityException e) {
			try {
				keyFactory = KeyFactory.getInstance("RSA");
			} catch (NoSuchAlgorithmException e1) {
				throw new EncryptionException(e1);
			}
		} catch (NoSuchAlgorithmException e) {
			throw new EncryptionException(e);
		}
		Cipher cipher;
		try {
			cipher = Cipher.getInstance("RSA/NONE/OAEPWithSHA256AndMGF1Padding", BouncyCastleProvider.PROVIDER_NAME);
		} catch (NoSuchProviderException | SecurityException e) {
			try {
				cipher = Cipher.getInstance("RSA/NONE/OAEPWithSHA256AndMGF1Padding");
			} catch (NoSuchAlgorithmException | NoSuchPaddingException e1) {
				throw new EncryptionException(e1);
			}
		} catch (NoSuchAlgorithmException | NoSuchPaddingException e) {
			throw new EncryptionException(e);
		}
		OAEPParameterSpec oaepParameterSpec = new OAEPParameterSpec("SHA-256", "MGF1", MGF1ParameterSpec.SHA256, PSource.PSpecified.DEFAULT);
		try {
			PublicKey pk = keyFactory.generatePublic(new X509EncodedKeySpec(decryptionKey));
			cipher.init(Cipher.DECRYPT_MODE, pk, oaepParameterSpec);
		} catch (InvalidAlgorithmParameterException | InvalidKeySpecException | InvalidKeyException e) {
			throw new EncryptionException("The encryption key is invalid.", e);
		}
		int es = cipher.getBlockSize();
		int bs = cipher.getOutputSize(es);
		if (bs < 80) {
			throw new EncryptionException("The encryption key length is too short.");
		}
		byte[] header = new byte[es];
		stream.read(header);
		try {
			header = cipher.doFinal(header, 0, es);
		} catch (IllegalBlockSizeException | BadPaddingException e) {
			throw new EncryptionException(e);
		}
		byte[] key = new byte[bs - 70];
		byte[] iv = new byte[12];
		byte[] salt = new byte[58];
        System.arraycopy(header, 0, iv, 0, iv.length);
        System.arraycopy(header, iv.length, key, 0, key.length);
        System.arraycopy(header, iv.length + key.length, salt, 0, salt.length);
		SecretKeyFactory f;
		try {
			f = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
		} catch (NoSuchProviderException | SecurityException e1) {
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
			secret = f.generateSecret(new PBEKeySpec(new String(key, StandardCharsets.ISO_8859_1).toCharArray(), salt, iterations, 256)); // key for AES-256
		} catch (InvalidKeySpecException e) {
			throw new EncryptionException(e);
		}
		secret = new SecretKeySpec(secret.getEncoded(), "AES");
		try {
			cipher = Cipher.getInstance("AES/GCM/NOPADDING", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
		} catch (NoSuchProviderException | SecurityException e1) {
			try {
				cipher = Cipher.getInstance("AES/GCM/NOPADDING"); //$NON-NLS-1$
			} catch (NoSuchAlgorithmException | NoSuchPaddingException e) {
				throw new EncryptionException(e);
			}
		} catch (NoSuchAlgorithmException | NoSuchPaddingException e) {
			throw new EncryptionException(e);
		}
		try {
			cipher.init(Cipher.DECRYPT_MODE, secret, new GCMParameterSpec(128, iv), RandomGenerator.rn);
		} catch (InvalidKeyException | InvalidAlgorithmParameterException e) {
			throw new EncryptionException(e);
		}
		stream = new CipherInputStream(stream, cipher);
		if (compression) {
			subStream = new InflaterInputStream(stream, new Inflater(true), 512);
		} else {
			subStream = stream;
		}
	}

	@Override
	public int read() throws IOException {
		return subStream.read();
	}

	@Override
	public int read(byte[] b) throws IOException {
		return subStream.read(b);
	}

	@Override
	public int read(byte[] b, int off, int len) throws IOException {
		return subStream.read(b, off, len);
	}

	@Override
	public byte[] readAllBytes() throws IOException {
		return subStream.readAllBytes();
	}

	@Override
	public byte[] readNBytes(int len) throws IOException {
		return subStream.readNBytes(len);
	}

	@Override
	public int readNBytes(byte[] b, int off, int len) throws IOException {
		return subStream.readNBytes(b, off, len);
	}

	@Override
	public long skip(long n) throws IOException {
		return subStream.skip(n);
	}

	@Override
	public void skipNBytes(long n) throws IOException {
		subStream.skipNBytes(n);
	}

	@Override
	public int available() throws IOException {
		return subStream.available();
	}

	@Override
	public void close() throws IOException {
		subStream.close();
	}

	@Override
	public synchronized void mark(int readlimit) {
		subStream.mark(readlimit);
	}

	@Override
	public synchronized void reset() throws IOException {
		subStream.reset();
	}

	@Override
	public boolean markSupported() {
		return subStream.markSupported();
	}

	@Override
	public long transferTo(OutputStream out) throws IOException {
		return subStream.transferTo(out);
	}

}
