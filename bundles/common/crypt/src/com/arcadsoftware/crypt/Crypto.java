/**
 * Copyright (c) Arcad-Software (2008). All Rights Reserved.
 * 
 * Creation date: 27 juin 2008
 * @author ARCAD Software
 * 
 */
package com.arcadsoftware.crypt;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.KeyFactory;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.Security;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import java.security.spec.MGF1ParameterSpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;
import java.util.Arrays;
import java.util.Base64;
import java.util.Random;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.OAEPParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.PSource;
import javax.crypto.spec.SecretKeySpec;
import org.bouncycastle.crypto.BufferedBlockCipher;
import org.bouncycastle.crypto.digests.WhirlpoolDigest;
import org.bouncycastle.crypto.engines.AESEngine;
import org.bouncycastle.crypto.modes.CBCBlockCipher;
import org.bouncycastle.crypto.paddings.PKCS7Padding;
import org.bouncycastle.crypto.paddings.PaddedBufferedBlockCipher;
import org.bouncycastle.crypto.params.KeyParameter;
import org.bouncycastle.jce.provider.BouncyCastleProvider;

import com.arcadsoftware.crypt.internal.Activator;
import com.arcadsoftware.crypt.internal.Whirlpool;

/**
 * Utility class to perform some cryptography operations.
 * 
 * <p>
 * Encoding and Hash methods :
 * 
 * <ul>
 * <li><b>hash</b> and <b>matches</b> methods are used to generate a secure hash of a string and test the hash representation to a clear text.
 * <li><b>encrypt</b> and <b>decrypt</b> use a secure reversible algorithm. 
 * <li><b>byteArrayToHexString</b> and <b>hexStringToByteArray</b> encode a byte array into its Hexa String representation.
 * </ul>
 * 
 * <p>
 * Some other implementation are accessible:
 * 
 * <ul>
 * <li><b>whirlpool</b>, <b>md5</b> and <b>sha1</b> hash a string with respectives algorithm but, these algorithms are not secure to be able 
 * to hash sensitive data. They can be used to generate CRC value (resp. with size of 512, 128 or 160 bits).
 * <li><b>isHashSecure</b> return false if the given Hash string should be regenerated (with the current implementation of the <b>hash</b> method).
 * <li><b>isCryptSecure</b> return false if the given encoded string should be regenerated (with the current implementation of the <b>hash</b> method).
 * <li><b>isHexString</b> return true if the given string is a potential hexadecimal representation as returned by <b>byteArrayToHexString</b>.  
 * </ul>
 *   
 * <p>
 * This class also give some String manipulation functions, like <b>LD</b> that compute the Levenshtein distance between to Strings.
 * 
 * <p>
 * String manipulation methods, theses methods can be used in Password complexity check:
 * 
 * <ul>
 * <li><b>countCharsInString</b> and <b>countCharsNotInString</b> count the char from a set that are (or not) into a string.
 * <li><b>digitCount</b> return the number of digits into the given String.
 * <li><b>alphaCount</b> return the number of letters into the given string.
 * <li><b>alphaNumCount</b> return the number of letter or digits in the String.
 * <li><b>nonAlphaNumCount</b> return the number of non letter nor digits in the String.
 * <li><b>alphaMajCount</b> return the number upper-case letters in the String.  
 * <li><b>alphaMinCount</b> return the number of lower-case letters is the String.
 * <li><b>diffCharCount</b> return the number of char that do not appears into the two given string.
 * <li><b>countCharsInString</b> (and <b>countCommonsChar</b>) return the number of char that appears into the two strings.
 * <li><b>reverse</b> reverse a String.
 * <li><b>scramble</b> scramble a String.
 * <li><b>scrambleRep</b> scramble a String, always in the same way, i.e. for a given string the scramble representation is always the same...
 * <li><b>randomString</b>, <b>randomStringDigit</b> and <b>randomStringNonAlpha</b> return randomly generated strings that content letters, digit or non alphanumeric symbols. 
 * </ul>
 * 
 * <p>
 * Misc methods:
 * <ul>
 * <li><b>testMemoryUsage</b> test if the JVM have enough free memory to run a LD comparison. 
 * <li><b>minimum</b> return the lower value of the given three values.
 * </ul>
 */
public final class Crypto {

	/**
	 * Get the minimal string length of a Hash generated by this class.
	 */
	public static final int HASH_MINLENGTH = 128;
	
	// ATTENTION This source code must use UTF-8 character set...
	private static final String RANDOMSTRING1 = "@`"; //$NON-NLS-1$
	private static final String RANDOMSTRING2 = ")°'"; //$NON-NLS-1$
	private static final String RANDOMSTRING3 = "=sµ"; //$NON-NLS-1$
	private static final String RANDOMSTRING4 = "é£L"; //$NON-NLS-1$
	private static final String RANDOMSTRING5 = "ȣĪ®"; //$NON-NLS-1$
	private static final String RANDOMSTRING6 = "1üJ"; //$NON-NLS-1$
	private static final String RANDOMSTRING7 = "ĂƇ©%"; //$NON-NLS-1$
	private static final String ALPHALOW = "abcdefghijklmnopqrstuvwxyz"; //$NON-NLS-1$
	private static final String ALPHAHIGH = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"; //$NON-NLS-1$
	private static final String ALPHA = ALPHALOW + ALPHAHIGH;
	private static final String DIGITS = "0123456789"; //$NON-NLS-1$
	private static final String ALPHANUM = ALPHA + DIGITS;
	public static final int SALTMINSIZE;
	private static final int SALTVARIATION;
	public static final int IVMINSIZE;
	private static final int IVVARIATION;
	public static final int HASHMINITERATIONS;
	private static final int HASHITERATIONSVARIATION;
	public static final int CIPHERMINITERATIONS;
	private static final int CIPHERITERATIONSVARIATION;
	private static final int HASH_ALGORITHM = 3;
	private static final int ENCRYPT_ALGORITHM = 2;
	private static final char[] DEFAULTMASTERK;
	private static final SecureRandom SECURERANDOM;
	private static int srcount = 0;
	
	static {
		// Register the BouncyCastle provider.
		if (Security.getProperty(BouncyCastleProvider.PROVIDER_NAME) == null) {
			try {
				Security.addProvider(new BouncyCastleProvider());
			} catch (Exception e) {
				System.err.println("There is a problem with Bouncy Castle (AFS will fall back to JCE implementation): " + e.getLocalizedMessage());
			}
		}
		// Initialize configuration.
		int i = 20;
		try {
			i = Integer.parseInt(System.getProperty("com.arcadsoftware.salt.min.size", Integer.toString(i))); //$NON-NLS-1$
			if (i < 16) {
				i = 16;
			}
		} catch (NumberFormatException e) {}
		SALTMINSIZE = i;
		i = 8;
		try {
			i = Integer.parseInt(System.getProperty("com.arcadsoftware.salt.size.variation", Integer.toString(i))); //$NON-NLS-1$
			if (i < 8) {
				i = 8;
			}
		} catch (NumberFormatException e) {}
		SALTVARIATION = i;
		i = 10;
		try {
			i = Integer.parseInt(System.getProperty("com.arcadsoftware.iv.min.size", Integer.toString(i))); //$NON-NLS-1$
			if (i < 4) {
				i = 4;
			}
		} catch (NumberFormatException e) {}
		// CTR/SIC mode requires IV no greater than: 16 bytes.
		if (i > 16) {
			i = 16;
		}
		IVMINSIZE = i;
		i = 6;
		try {
			i = Integer.parseInt(System.getProperty("com.arcadsoftware.iv.size.variation", Integer.toString(i))); //$NON-NLS-1$
			if (i < 2) {
				i = 2;
			}
		} catch (NumberFormatException e) {}
		// CTR/SIC mode requires IV no greater than: 16 bytes.
		if ((i + IVMINSIZE) > 16) {
			i = 16 - IVMINSIZE;
		}
		IVVARIATION = i;
		i = 11675;
		try {
			i = Integer.parseInt(System.getProperty("com.arcadsoftware.hash.min.iterations", Integer.toString(i))); //$NON-NLS-1$
			if (i < 10000) {
				i = 10000;
			}
		} catch (NumberFormatException e) {}
		HASHMINITERATIONS = i;
		i = 2892;
		try {
			i = Integer.parseInt(System.getProperty("com.arcadsoftware.hash.iterations.variation", Integer.toString(i))); //$NON-NLS-1$
			if (i < 100) {
				i = 100;
			}
		} catch (NumberFormatException e) {}
		HASHITERATIONSVARIATION = i;
		i = 13187;
		try {
			i = Integer.parseInt(System.getProperty("com.arcadsoftware.cypher.min.iterations", Integer.toString(i))); //$NON-NLS-1$
			if (i < 10000) {
				i = 10000;
			}
		} catch (NumberFormatException e) {}
		CIPHERMINITERATIONS = i;
		i = 2637;
		try {
			i = Integer.parseInt(System.getProperty("com.arcadsoftware.cypher.iterations.variation", Integer.toString(i))); //$NON-NLS-1$
			if (i < 100) {
				i = 100;
			}
		} catch (NumberFormatException e) {}
		CIPHERITERATIONSVARIATION = i;
		String dmk = System.getProperty("com.arcadsoftware.masterkey.path"); //$NON-NLS-1$
		char[] dmkc = null;
		if (dmk != null) {
			File mkf = new File(dmk);
			if (mkf.isFile()) {
				try {
					dmkc = getChars(Files.readAllBytes(mkf.toPath()), StandardCharsets.UTF_8, 0);
				} catch (IOException e) {
					dmk = null;
				}
			} else {
				dmk = null;
			}
		} 
		if (dmk == null) {
			dmk = System.getProperty("com.arcadsoftware.masterkey.fog"); //$NON-NLS-1$
			if (dmk != null) {
				dmkc = unFogChar(dmk, StandardCharsets.UTF_8);
			} else {
				dmk = System.getProperty("com.arcadsoftware.masterkey"); //$NON-NLS-1$
				if (dmk != null) {
					dmkc = dmk.toCharArray();
				} else {
					String osn = System.getProperty("os.name"); //$NON-NLS-1$
					String hn = System.getProperty("user.name"); //$NON-NLS-1$
					String jh = System.getProperty("java.home"); //$NON-NLS-1$
					try {
						String h = InetAddress.getLocalHost().getHostName();
						if (!h.equals(InetAddress.getLocalHost().getHostAddress())) {
							hn = h;
						}
					} catch (UnknownHostException e) {}
					if ("true".equalsIgnoreCase(System.getProperty("com.arcadsoftware.masterkey.trace"))) { //$NON-NLS-1$ //$NON-NLS-2$
						System.out.println("osn=" + osn); //$NON-NLS-1$
						System.out.println("hn=" + hn); //$NON-NLS-1$
						System.out.println("jh=" + jh); //$NON-NLS-1$
					}
					dmkc = new char[osn.length() + RANDOMSTRING7.length() + hn.length() + RANDOMSTRING3.length() + jh.length()];
					System.arraycopy(osn.toCharArray(), 0, dmkc, 0, osn.length());
					System.arraycopy(RANDOMSTRING7.toCharArray(), 0, dmkc, osn.length(), RANDOMSTRING7.length());
					System.arraycopy(hn.toCharArray(), 0, dmkc, osn.length() + RANDOMSTRING7.length(), hn.length());
					System.arraycopy(RANDOMSTRING3.toCharArray(), 0, dmkc, osn.length() + RANDOMSTRING7.length() + hn.length(), RANDOMSTRING3.length());
					System.arraycopy(jh.toCharArray(), 0, dmkc, osn.length() + RANDOMSTRING7.length() + hn.length() + RANDOMSTRING3.length(), jh.length());
					if ("true".equalsIgnoreCase(System.getProperty("com.arcadsoftware.masterkey.trace"))) { //$NON-NLS-1$ //$NON-NLS-2$
						System.out.println("Default master key is:\ncom.arcadsoftware.masterkey=" + new String(dmkc));
					}
					// Will record this key in the config.ini file (if the context of execution is an OSGi platform.
					Activator.temp = Arrays.copyOf(dmkc, dmkc.length);
				}
			}
		}
		scrambleRep(dmkc);
		DEFAULTMASTERK = dmkc;
		SecureRandom sr = null;
		try {
			sr = SecureRandom.getInstance("SHA1PRNG"); //$NON-NLS-1$
		} catch (NoSuchAlgorithmException e) {
			try {
				sr = SecureRandom.getInstanceStrong();
			} catch (NoSuchAlgorithmException e1) {
				// Should never occurs as the JVM must have at least one strong implementation of SecureRandom.
				System.out.println(e.getLocalizedMessage());
				// Safe here !
			}
		}
		SECURERANDOM = sr;
	}
	
	private static final String[] TESTRESULTLABELS = { "PWD.Confirmation.Change", "PWD.MinSize", "PWD.MaxSize", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			"PWD.PrevDist", "PWD.LoginDist", "PWD.LoginChar", "PWD.DigitMin", "PWD.AlphaMin", "PWD.InternalError"}; // = N°8 //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
	private static final int RANDON_ENCODED_STRING = 32;
	private static final int MINIMAL_ENCODED_STRING_LENGHT = RANDON_ENCODED_STRING * 2;

	@Deprecated
	private static final String KS = reverse(RANDOMSTRING3) + RANDOMSTRING1 + reverse(RANDOMSTRING2) + reverse(RANDOMSTRING5); // = 128 bytes.
	@Deprecated
	private static final byte[] KSE = getBytes(reverse(RANDOMSTRING3) + RANDOMSTRING6 + reverse(RANDOMSTRING4) + reverse(RANDOMSTRING2) + RANDOMSTRING1 + reverse(RANDOMSTRING5) + reverse(RANDOMSTRING7)); // = 256 bytes

	/**
	 * Get the corresponding label of the Pasword test result score.
	 * 
	 * @return a non null array.
	 */
	public static final String[] getTestResultLabels() {
		return Arrays.copyOf(TESTRESULTLABELS, TESTRESULTLABELS.length);
	}

	/**
	 * Hash with the MD5 algorithm.
	 * 
	 * <p>
	 * The string is converted into UTF-8 byte array.
	 * 
	 * <p>
	 * <b>Note that the MD5 hash algorithm is very weak, do not use it for encryption.</b>
	 * 
	 * @param text
	 * @return Null if there is a problem with the JVM capabilities, an hexadecimal representation of the hash.
	 * @throws EncryptionError
	 */
	public static final String md5(char[] text) {
		byte[] b = getBytes(text);
		try {
			return byteArrayToHexString(MessageDigest.getInstance("MD5").digest(b)); //$NON-NLS-1$
		} catch (Exception e) {
			throw new EncryptionError(e);
		} finally {
			clear(b);
		}
	}

	/**
	 * Hash with the SHA1 algorithm.
	 * 
	 * <p>
	 * The string is converted into UTF-8 byte array.
	 * 
	 * <p>
	 * <b>Note that the SHA1 hash algorithm is very weak, do not use it for encryption.</b>
	 * 
	 * @param text
	 * @return Null if there is a problem with the JVM capabilities, an hexadecimal representation of the hash.
	 * @throws EncryptionError
	 */
	public static final String sha1(char[] text) {
		byte[] b = getBytes(text);
		try {
			return byteArrayToHexString(MessageDigest.getInstance("SHA-1").digest(b)); //$NON-NLS-1$
		} catch (Exception e) {
			throw new EncryptionError(e);
		} finally {
			clear(b);
		}
	}

	/**
	 * Hash with the Whirlpool algorithm.
	 * 
	 * @param text
	 * @return An Base64 representation of the hash.
	 */
	public static final String whirlpool(char[] text) {
		final byte[] bytes = getBytes(text);
		try {
			WhirlpoolDigest whirlpool = new WhirlpoolDigest();
			whirlpool.reset();
			whirlpool.update(bytes, 0, bytes.length);
		    final byte[] hash = new byte[whirlpool.getDigestSize()];
		    whirlpool.doFinal(hash, 0);
			return Base64.getEncoder().encodeToString(hash);
		} finally {
			clear(bytes);
		}
	}
	
	/**
	 * Hash with the latest Hash algorithm, with convenient (security/performance) parameters.
	 * 
	 * <p>
	 * The result hash string contain a stamp that identify the algorithm and the parameters so it is required for hash comparison.
	 * 
	 * @param text
	 * @see https://www.baeldung.com/java-password-hashing
	 * @see #matches(String, String)
	 * @deprecated use {@link #hash(char[])}
	 */
	@Deprecated
	public static final String hash(String text) {
		return hash(text.toCharArray());
	}
	
	/**
	 * Hash with the latest Hash algorithm, with convenient (security/performance) parameters.
	 * 
	 * <p>
	 * The result hash string contain a stamp that identify the algorithm and the parameters so it is required for hash comparison.
	 * 
	 * @param text
	 * @return 
	 * @throws EncryptionError
	 * @see https://www.baeldung.com/java-password-hashing
	 */
	public static final String hash(char[] text) {
		try {
	        byte[] salt = new byte[SALTMINSIZE + SECURERANDOM.nextInt(SALTVARIATION)];
	        SECURERANDOM.nextBytes(salt);
			return hash(text, salt, HASHMINITERATIONS + SECURERANDOM.nextInt(HASHITERATIONSVARIATION), HASH_ALGORITHM);
		} catch (NoSuchAlgorithmException | InvalidKeySpecException e) {
			throw new EncryptionError(e);
		} finally {
	        reseedSecureRandom();
		}
	}

	/**
	 * Hash a data.
	 * 
	 * <p>
	 * The currently supported Hash algorithm are:
	 * 
	 * <ol>
	 * <li> Whrilpool (legacy algorithmà
	 * <li> PBKDF2 with Hmac SHA1 and 1024b key size.
	 * <li> PBKDF2 with Hmac SHA256 and 1024b key size.
	 * </ol>
	 * 
	 * 
	 * @param text The data to hash.
	 * @param salt A random generated data used to make a more difficult a dictionnary based attack.
	 * @param iterations The number of call hashing algorithm.
	 * @param algorithm A code identifying the hashing algorithm.
	 * @return
	 * @throws NoSuchAlgorithmException
	 * @throws InvalidKeySpecException
	 */
	public static final String hash(char[] text, byte[] salt, int iterations, int algorithm) throws NoSuchAlgorithmException, InvalidKeySpecException {
		int size;
		String alg = null;
		byte[] hash = new byte[0];
		switch (algorithm) {
		case 0:
			return whirlpoolEx(text);
		case 1:
			size = 1024; // Ensure an Base64 string longer than 128 char !
			alg = "PBKDF2WithHmacSHA1";
			break;
		case 2:
		case 3:
			size = 1024; // Ensure an Base64 string longer than 128 char !
			alg = "PBKDF2WithHmacSHA256";
			break;
		default:
			throw new NoSuchAlgorithmException(String.format("The given Algorithn index reference (%d) is incorrect.", algorithm));
		}
		if (alg != null) {
			SecretKeyFactory sf;
			try {
				sf = SecretKeyFactory.getInstance(alg, BouncyCastleProvider.PROVIDER_NAME);
			} catch (NoSuchProviderException | SecurityException e) {
				sf = SecretKeyFactory.getInstance(alg);
			}
			hash = sf.generateSecret(new PBEKeySpec(text, salt, iterations, size)).getEncoded();
		}
        byte[] result = new byte[hash.length + salt.length + 12];
        setIntByte(result, 0, algorithm);
        setIntByte(result, 4, iterations - HASHMINITERATIONS);
        setIntByte(result, 8, salt.length - SALTMINSIZE);
        System.arraycopy(hash, 0, result, 12, hash.length);
        System.arraycopy(salt, 0, result, 12 + hash.length, salt.length);
        return Base64.getEncoder().encodeToString(result);
	}
	
	/**
	 * Compare an hashed password to its clear representation, return true if they are compatible.
	 * 
	 * @param hash
	 * @param text
	 * @return
	 * @deprecated use {@link #matches(String, char[])}
	 */
	@Deprecated
	public static final boolean matches(String hash, String text) {
		char[] chars = text.toCharArray();
		try {
			return matches(hash, chars);
		} finally  {
			clear(chars);
		}
	}
	
	/**
	 * Compare an hashed password to its clear representation, return true if they are compatible.
	 * 
	 * @param hash
	 * @param text
	 * @return
	 */
	public static final boolean matches(String hash, char[] text) {
		if ((text == null) || (hash == null)) {
			return false;
		}
		if (Arrays.equals(text, hash.toCharArray())) {
			return true;
		}
		if (hash.length() == HASH_MINLENGTH) {
			return hash.equals(whirlpoolEx(text));
		}
		try {
			byte[] h = Base64.getDecoder().decode(hash);
			int algorithm = getIntByte(h, 0);
			int iterations = getIntByte(h, 4);
			int saltsize = getIntByte(h, 8);
			int size;
			String alg = null;
			byte[] nh = new byte[0];
			switch (algorithm) {
			case 1:
				size = 1024; // SHA1 generate 160bit long hash !
				alg = "PBKDF2WithHmacSHA1";
				break;
			case 3:
				iterations += HASHMINITERATIONS;
				saltsize += SALTMINSIZE;
			case 2:
				size = 1024; // SHA256 generate 256bit long hash !
				alg = "PBKDF2WithHmacSHA256";
				break;
			default:
				return false;
			}
			byte[] salt = Arrays.copyOfRange(h, h.length - saltsize, h.length);
			if (alg != null) {
				SecretKeyFactory sf;
				try {
					sf = SecretKeyFactory.getInstance(alg, BouncyCastleProvider.PROVIDER_NAME);
				} catch (NoSuchAlgorithmException | SecurityException e) {
					sf = SecretKeyFactory.getInstance(alg);
				}
		        nh = sf.generateSecret(new PBEKeySpec(text, salt, iterations, size)).getEncoded();
			}
	        if (h.length != (nh.length + 12 + saltsize)) {
	        	return false;
	        }
	        for (int i = 0; i < nh.length; i++) {
	        	if (nh[i] != h[i+12]) {
	        		return false;
	        	}
	        }
	        return true;
		} catch (Exception e) {
			return false;
		}
	}
	
	/**
	 * Test the given hash material and return true if and only if it has been generated with the current, secure, implementation of the <b>hash</b> method.
	 * 
	 * <p>
	 * If this method return false and you have access to the original data you should consider to regenerate it.
	 * 
	 * @param hash
	 * @return
	 */
	public static final boolean isHashSecure(String hash) {
		if ((hash == null) || (hash.length() <= 128)) {
			// Include "clear passwords" and Whirlpool hash.
			return false;
		}
		try {
			byte[] h = Base64.getDecoder().decode(hash);
			// Test the iteration count, salt size and algo type:
			int a = getIntByte(h, 0);
			int i = getIntByte(h, 4);
			int s = getIntByte(h, 8);
			if ((a < 2) || (i < 1) || (s < 1)) {
				return false;
			}
		} catch (Exception e) {
			// if there is any error then this is because the used algorithm is not currently supported !
			return false;
		}
		return true;
	}
	
	/**
	 * Test the given hash material and return true if and only if it has been generated with the current, secure, implementation of the <b>hash</b> method.
	 * 
	 * <p>
	 * If this method return false and you have access to the original data you should consider to regenerate it.
	 * 
	 * @param hash
	 * @return
	 */
	public static final boolean isHashSecure(char[] hash) {
		if ((hash == null) || (hash.length <= 128)) {
			// Include "clear passwords" and Whirlpool hash.
			return false;
		}
		try {
			ByteBuffer buffer = StandardCharsets.ISO_8859_1.encode(CharBuffer.wrap(hash));
			byte[] h = Base64.getDecoder().decode(Arrays.copyOf(buffer.array(), buffer.limit()));
			// Test the iteration count, salt size and algo type:
			int a = getIntByte(h, 0);
			int i = getIntByte(h, 4);
			int s = getIntByte(h, 8);
			if ((a < 2) || (i < 1) || (s < 1)) {
				return false;
			}
		} catch (Exception e) {
			// if there is any error then this is because the used algorithm is not currently supported !
			return false;
		}
		return true;
	}
	
	/**
	 * Encrypt the given text using the default Master Key.
	 * 
	 * @param text
	 * @return null if there is a problem in the configuration of the Cryptographic properties.
	 * @see #encrypt(String, char[], int)
	 * @deprecated use {@link #encrypt(char[])}
	 */
	@Deprecated
	public static String encrypt(String text) {
		return encrypt(text.toCharArray());
	}
	
	/**
	 * Encrypt the given text using the default Master Key.
	 * 
	 * @param text
	 * @return null if there is a problem in the configuration of the Cryptographic properties.
	 * @see #encrypt(String, char[], int)
	 * @throws EncryptionError
	 */
	public static String encrypt(char[] text) {
		try {
			return encrypt(text, DEFAULTMASTERK, ENCRYPT_ALGORITHM);
		} catch (IllegalStateException | InvalidKeyException | NoSuchAlgorithmException | InvalidAlgorithmParameterException
				| IllegalBlockSizeException | InvalidKeySpecException e) {
			throw new EncryptionError(e);
		}
	}
	
	/**
	 * Encrypt the given text, using the masterKey password. This method use salt and IV when possible.
	 * 
	 * <p>
	 * The different currently implemented algorithms are :
	 * <ol>
	 * <li> AES-256 with CTR mode.
	 * </ol>
	 * @param text the clear text to encrypt, the caller is responsible to delete this information after encryption.
	 * @param masterkey The master key in clear text, the caller is responsible to delete this information after encryption.
	 * @param algorithm The algorithn version number to use, currently suport only version 1.
	 * @return a non null Base64 encrypted buffer.
	 * @throws NoSuchAlgorithmException
	 * @throws InvalidKeyException
	 * @throws InvalidAlgorithmParameterException
	 * @throws IllegalBlockSizeException
	 * @throws InvalidKeySpecException
	 */
	public static String encrypt(char[] text, char[] masterkey, int algorithm) throws NoSuchAlgorithmException, InvalidKeyException, InvalidAlgorithmParameterException, IllegalBlockSizeException, InvalidKeySpecException {
        byte[] salt = new byte[SALTMINSIZE + SECURERANDOM.nextInt(SALTVARIATION)];
        SECURERANDOM.nextBytes(salt);
		byte[] iv = new byte[IVMINSIZE + SECURERANDOM.nextInt(IVVARIATION)];
		SECURERANDOM.nextBytes(iv);
		srcount += 2;
		int itterations = CIPHERMINITERATIONS + SECURERANDOM.nextInt(CIPHERITERATIONSVARIATION);
		SecretKey secret;
		Cipher cipher;
		try {
			switch (algorithm) {
			case 0:
			case 1:
				SecretKeyFactory f;
				try {
					f = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
				} catch (NoSuchProviderException | SecurityException e1) {
					f = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1"); //$NON-NLS-1$
				}
				int kl = 256; // key for AES-256
				try {
					int legal = Cipher.getMaxAllowedKeyLength("AES"); //$NON-NLS-1$
					if (legal < kl) {
						kl = legal;
					}
				} catch (NoSuchAlgorithmException e) {}
				secret = f.generateSecret(new PBEKeySpec(masterkey, salt, itterations, kl));
				secret = new SecretKeySpec(secret.getEncoded(), "AES"); //$NON-NLS-1$
				try {
					cipher = Cipher.getInstance("AES/CTR/NOPADDING", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
				} catch (NoSuchProviderException | SecurityException e) {
					cipher = Cipher.getInstance("AES/CTR/NOPADDING"); //$NON-NLS-1$
				}
				break;
			case 2:
				try {
					f = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
				} catch (NoSuchProviderException | SecurityException e1) {
					// This will not work...
					f = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256"); //$NON-NLS-1$
				}
				kl = 256; // key for AES-256
				try {
					int legal = Cipher.getMaxAllowedKeyLength("AES"); //$NON-NLS-1$
					if (legal < kl) {
						kl = legal;
					}
				} catch (NoSuchAlgorithmException e) {}
				secret = f.generateSecret(new PBEKeySpec(masterkey, salt, itterations, kl));
				secret = new SecretKeySpec(secret.getEncoded(), "AES"); //$NON-NLS-1$
				try {
					cipher = Cipher.getInstance("AES/CTR/NOPADDING", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
				} catch (NoSuchProviderException | SecurityException e) {
					cipher = Cipher.getInstance("AES/CTR/NOPADDING"); //$NON-NLS-1$
				}
				break;
			default:
				throw new NoSuchAlgorithmException(String.format("The given Algorithn index reference (%d) is incorrect.", algorithm));
			}
			cipher.init(Cipher.ENCRYPT_MODE, secret, new IvParameterSpec(iv), SECURERANDOM);
			byte[] b = getBytes(text);
			try {
				byte[] c = cipher.doFinal(b);
				byte[] result = new byte[c.length + 16 + salt.length + iv.length];
		        setIntByte(result, 0, algorithm);
		        if (algorithm >= 2) {
			        setIntByte(result, 4, salt.length - SALTMINSIZE);
			        setIntByte(result, 8, iv.length - IVMINSIZE);
			        setIntByte(result, 12, itterations - CIPHERMINITERATIONS);
		        } else {
			        setIntByte(result, 4, salt.length);
			        setIntByte(result, 8, iv.length);
			        setIntByte(result, 12, itterations);
		        }
		        System.arraycopy(iv, 0, result, 16, iv.length);
		        System.arraycopy(c, 0, result, 16 + iv.length, c.length);
		        System.arraycopy(salt, 0, result, 16 + iv.length + c.length, salt.length);
				return Base64.getEncoder().encodeToString(result);
			} finally {
				clear(b);
				clear(salt);
				clear(iv);
			}
		} catch (NoSuchPaddingException | BadPaddingException e) {
			// Should never occurs as there is no padding !
			throw new NoSuchAlgorithmException(String.format("Internal Error with the given Algorithn index reference (%d) is incorrect.", algorithm), e);
		} finally {
			reseedSecureRandom();
		}
	}

	/**
	 * Decrypt an encrypted text using the default master key.
	 * 
	 * @param text
	 * @return
	 * @see #encrypt(String)
	 */
	public static char[] decrypt(String text) {
		try {
			return decrypt(text, DEFAULTMASTERK);
		} catch (InvalidKeyException | NoSuchAlgorithmException | IllegalBlockSizeException | InvalidKeySpecException
				| InvalidAlgorithmParameterException e) {
			// the message was a clear text message (or is corrupted) !
			return text.toCharArray();
			// TODO Log Error Message...
		}
	}
	
	/**
	 * Decrypt an encrypted text using the given key.
	 * 
	 * @param text
	 * @param masterkey
	 * @return
	 * @throws NoSuchAlgorithmException 
	 * @throws BadPaddingException 
	 * @throws IllegalBlockSizeException 
	 * @throws InvalidAlgorithmParameterException 
	 * @throws InvalidKeyException 
	 * @throws  
	 * @see #encrypt(String, char[], int)
	 */
	public static char[] decrypt(String text, char[] masterkey) throws NoSuchAlgorithmException, IllegalBlockSizeException, InvalidKeySpecException, InvalidKeyException, InvalidAlgorithmParameterException {
		if (text == null) {
			return null;
		}
		if (text.isEmpty()) {
			return new char[0];
		}
		if (isHexString(text)) {
			// New algorithm use BASE64 with starting byte that can not be taken for an HexString... so this passord is a fog one
			return unFog(text).toCharArray();
		}
		byte[] iv = null;
		byte[] salt = null;
		byte[] ct = null;
		int iterations = 0;
		int algorithm = 0;
		try {
			byte[] buffer = Base64.getDecoder().decode(text);
			algorithm = getIntByte(buffer, 0);
			int saltsize = getIntByte(buffer, 4);
			int ivsize = getIntByte(buffer, 8);
			iterations = getIntByte(buffer, 12);
			if (algorithm > 1) {
				saltsize += SALTMINSIZE;
				ivsize += IVMINSIZE;
				iterations += CIPHERMINITERATIONS;
			}
			if ((algorithm > 2) || (saltsize < 4) || (ivsize < 4) || (iterations < 1)) {
				return decodeXE256(text).toCharArray();
			}
			iv = new byte[ivsize];
			salt = new byte[saltsize];
			ct = new byte[buffer.length - 16 - ivsize - saltsize];
			System.arraycopy(buffer, 16, iv, 0, ivsize);
			System.arraycopy(buffer, 16 + ivsize, ct, 0, ct.length);
			System.arraycopy(buffer, buffer.length - saltsize, salt, 0, saltsize);
		} catch (Exception e) {
			// the message was a clear text message (or is corrupted) !
			return text.toCharArray();
		}
		SecretKey secret;
		Cipher cipher;
		try {
			switch (algorithm) {
			case 1:
				SecretKeyFactory f;
				try {
					f = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
				} catch (NoSuchProviderException | SecurityException e1) {
					f = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1"); //$NON-NLS-1$
				}
				int kl = 256; // key for AES-256
				try {
					int legal = Cipher.getMaxAllowedKeyLength("AES"); //$NON-NLS-1$
					if (legal < kl) {
						kl = legal;
					}
				} catch (NoSuchAlgorithmException e) {}
				secret = f.generateSecret(new PBEKeySpec(masterkey, salt, iterations, kl)); // key for AES-256
				secret = new SecretKeySpec(secret.getEncoded(), "AES");
				try {
					cipher = Cipher.getInstance("AES/CTR/NOPADDING", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
				} catch (NoSuchProviderException | SecurityException e) {
					cipher = Cipher.getInstance("AES/CTR/NOPADDING"); //$NON-NLS-1$
				}
				break;
			case 2:
				try {
					f = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
				} catch (NoSuchProviderException | SecurityException e1) {
					f = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256"); //$NON-NLS-1$
				}
				kl = 256; // key for AES-256
				try {
					int legal = Cipher.getMaxAllowedKeyLength("AES"); //$NON-NLS-1$
					if (legal < kl) {
						kl = legal;
					}
				} catch (NoSuchAlgorithmException e) {}
				secret = f.generateSecret(new PBEKeySpec(masterkey, salt, iterations, kl)); // key for AES-256
				secret = new SecretKeySpec(secret.getEncoded(), "AES");
				try {
					cipher = Cipher.getInstance("AES/CTR/NOPADDING", BouncyCastleProvider.PROVIDER_NAME); //$NON-NLS-1$
				} catch (NoSuchProviderException | SecurityException e) {
					cipher = Cipher.getInstance("AES/CTR/NOPADDING"); //$NON-NLS-1$
				}
				break;
			default: // invalid algorithms are detected earlier...
				// the message was a clear text message (or is corrupted) !
				return text.toCharArray();
			}
			cipher.init(Cipher.DECRYPT_MODE, secret, new IvParameterSpec(iv), SECURERANDOM);
			byte[] b = cipher.doFinal(ct);
			try {
				return getChars(b, StandardCharsets.UTF_8, 0);
			} finally {
				clear(b);
			}
		} catch (NoSuchPaddingException | BadPaddingException e) {
			// Do not use padding here and UTF-8 should be supported... !
			// the message was a clear text message (or is corrupted) !
			return text.toCharArray();
		} finally {
			reseedSecureRandom();
		}
	}
	
	public static boolean isCryptSecure(String text) {
		if (text == null) {
			return true;
		}
		if (isHexString(text) || (text.length() < 34)) { // 34 = Base64 of at least 25 bytes.
			return false;
		}
		try {
			byte[] buffer = Base64.getDecoder().decode(text);
			if (buffer.length < 24) {
				return false;
			}
			int algorithm = getIntByte(buffer, 0);
			int saltsize = getIntByte(buffer, 4);
			int ivsize = getIntByte(buffer, 8);
			int iterations = getIntByte(buffer, 12);
			return (algorithm >= 1) && (algorithm < 3) && (saltsize >= 1) && (ivsize >= 1) && (iterations >= 1);  
		} catch (IllegalArgumentException e) {
			return false;
		}
	}
	
	/**
	 * Use an assymmetric algorithm to encrypt a text.
	 * 
	 * <p>
	 * Note that this method use a private key to encrypt and a public key to decrypt.
	 * 
	 * @param text the text to encode.
	 * @param key A PKCS8 encoded private key.
	 * @return null if there is a problem with the key.
	 */
	public static final String encryptA(final char[] text, final byte[] key) {
		return encryptA(getBytes(text), key);
	}
	
	/**
	 * Use an assymmetric algorithm to encrypt a binary set of data.
	 * 
	 * <p>
	 * Note that this method use a private key to encrypt and a public key to decrypt.
	 * 
	 * @param data the binary data to encode.
	 * @param key A PKCS8 encoded private key.
	 * @return null if there is a problem with the key.
	 * @throws EncryptionError
	 */
	public static final String encryptA(byte[] data, final byte[] key) {
		try {
			Cipher cipher;
			try {
				cipher = Cipher.getInstance("RSA/ECB/OAEPWithSHA256AndMGF1Padding", BouncyCastleProvider.PROVIDER_NAME);
			} catch (NoSuchProviderException | SecurityException e) {
				cipher = Cipher.getInstance("RSA/ECB/OAEPWithSHA256AndMGF1Padding");
			}
			KeyFactory keyFactory;
			try {
				keyFactory = KeyFactory.getInstance("RSA", BouncyCastleProvider.PROVIDER_NAME);
			} catch (NoSuchProviderException | SecurityException e) {
				keyFactory = KeyFactory.getInstance("RSA");
			}
			OAEPParameterSpec oaepParameterSpec = new OAEPParameterSpec("SHA-256", "MGF1", MGF1ParameterSpec.SHA256, PSource.PSpecified.DEFAULT);
			PrivateKey pk = keyFactory.generatePrivate(new PKCS8EncodedKeySpec(key));
			cipher.init(Cipher.ENCRYPT_MODE, pk, oaepParameterSpec);
			int bs = cipher.getBlockSize();
			try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
				int i = 0;
				while (i + bs < data.length) {
					bos.write(cipher.doFinal(data, i, bs));
					i += bs;
				}
				bos.write(cipher.doFinal(data, i, data.length - i));
				data = bos.toByteArray();
			}
		} catch (Exception e) {
			throw new EncryptionError(e);
		}
		byte[] sdata = new byte[data.length + 1];
		System.arraycopy(data, 0, sdata, 1, data.length);
		// TAG Value (for versioning)
		sdata[0] = 1;
		return Base64.getEncoder().encodeToString(sdata);
	}

	/**
	 * Use an assymetric algorithm to decrypt a text.
	 * 
	 * <p>
	 * Note that this method use a private key to encrypt and a public key to decrypt.
	 * 
	 * @param text the encrypted data
	 * @param key a X509 encoded public key.
	 * @return null if there is a problem with the key.
	 */
	public static final char[] decryptA(String text, byte[] key) {
		return getChars(decryptAByte(text, key), StandardCharsets.UTF_8, 0);
	}
	
	/**
	 * Use an assymetric algorithm to decrypt a text.
	 * 
	 * <p>
	 * Note that this method use a private key to encrypt and a public key to decrypt.
	 * 
	 * @param text the encrypted data
	 * @param key a X509 encoded public key.
	 * @return null if there is a problem with the key.
	 * @throws EncryptionError
	 */
	public static final byte[] decryptAByte(String text, byte[] key) {
		byte[] data = Base64.getDecoder().decode(text);
		// Remove Tag Value
		byte[] sdata = new byte[data.length - 1];
		System.arraycopy(data, 1, sdata, 0, sdata.length);
		try {
			KeyFactory keyFactory;
			try {
				keyFactory = KeyFactory.getInstance("RSA", BouncyCastleProvider.PROVIDER_NAME);
			} catch (NoSuchProviderException | SecurityException e) {
				keyFactory = KeyFactory.getInstance("RSA");
			}
			Cipher cipher;
			try {
				cipher = Cipher.getInstance("RSA/NONE/OAEPWithSHA256AndMGF1Padding", BouncyCastleProvider.PROVIDER_NAME);
			} catch (NoSuchProviderException | SecurityException e) {
				cipher = Cipher.getInstance("RSA/NONE/OAEPWithSHA256AndMGF1Padding");
			}
			OAEPParameterSpec oaepParameterSpec = new OAEPParameterSpec("SHA-256", "MGF1", MGF1ParameterSpec.SHA256, PSource.PSpecified.DEFAULT);
			PublicKey pk = keyFactory.generatePublic(new X509EncodedKeySpec(key));
			cipher.init(Cipher.DECRYPT_MODE, pk, oaepParameterSpec);
			int i = 0;
			int bs = cipher.getBlockSize();
			try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
				while (i < sdata.length) {
					bos.write(cipher.doFinal(sdata, i, bs));
					i += bs;
				}
				sdata = bos.toByteArray();
			}
		} catch (Exception e) {
			throw new EncryptionError(e);
		}
		return sdata;
	}
	
	/**
	 * Transform a String into an encoded string.
	 * 
	 * <p>
	 * The "cryptographic" algorithm is very simple but ensure that the string is not, at least visually, recognizable. 
	 * 
	 * <p>
	 * The returned string is approximatively x2 more longer than the original (more precisely = 32 + length[in bytes] x 2).
	 * 
	 * <p>
	 * Benchmark test report, depending on the computer and JVM version, to encode+decode a string:
	 * <ul>
	 * <li> fog/unfog = <1ms.
	 * <li> encode/decode = ...ms.
	 * </ul>
	 * 
	 * @param string
	 * @return
	 * @see #decode(String)
	 * @deprecated This method use the Local charset. Use {@link #fogChar(char[], Charset) instead only in new implementations.}
	 */
	public static String fog(String string) {
		if (string == null) {
			return null;
		}
		string = RandomGenerator.randomString(RANDON_ENCODED_STRING) + string;
		return reverse(byteArrayToHexString(string.getBytes()));
	}

	/**
	 * Transform a String into an encoded string.
	 * 
	 * <p>
	 * The "cryptographic" algorithm is very simple but ensure that the string is not, at least visually, recognizable. 
	 * 
	 * <p>
	 * The returned string is approximatively x2 more longer than the original (more precisely = 32 + length[in bytes] x 2).
	 * 
	 * <p>
	 * Benchmark test report, depending on the computer and JVM version, to encode+decode a string:
	 * <ul>
	 * <li> fog/unfog = <1ms.
	 * <li> encode/decode = ...ms.
	 * </ul>
	 * 
	 * @param string
	 * @param charset if null the UTF8 charset is used.
	 * @return
	 */
	public static String fog(String string, Charset charset) {
		if (string == null) {
			return null;
		}
		if (charset == null) {
			charset = StandardCharsets.UTF_8;
		}
		string = RandomGenerator.randomString(RANDON_ENCODED_STRING) + string;
		return reverse(byteArrayToHexString(string.getBytes(charset)));
	}
	
	/**
	 * Transform a Char array into an encoded string using a specific Charset.
	 * 
	 * <p>
	 * The "cryptographic" algorithm is very simple but ensure that the char array is not, at least visually, recognizable. 
	 * 
	 * <p>
	 * The returned string is approximatively x2 more longer than the original (more precisely = 32 + length[in bytes] x 2).
	 * 
	 * <p>
	 * The caller is responsible to clear the char array content after the call of this method.
	 * 
	 * @param string
	 * @param charset
	 * @return
	 */
	public static String fog(char[] string, Charset charset) {
		if (string == null) {
			return null;
		}
		char[] c = new char[string.length + RANDON_ENCODED_STRING];
		try {
			System.arraycopy(RandomGenerator.randomString(RANDON_ENCODED_STRING).toCharArray(), 0, c, 0, RANDON_ENCODED_STRING);
			System.arraycopy(string, 0, c, RANDON_ENCODED_STRING, string.length);
			CharBuffer cb = CharBuffer.wrap(c);
			ByteBuffer bb = charset.encode(cb);
			byte[] b = new byte[bb.remaining()];
			bb.get(b);
			clear(bb.array());
			clear(cb.array());
			return reverse(byteArrayToHexString(b));
		} finally {
			clear(c);
		}
	}
	
	/**
	 * Decode a string encoded with the <code>fog</code> method.
	 * 
	 * @param string
	 * @return
	 * @see #fog(String)
	 * @deprecated This method use the Local charset. consider to use {@link #unFog(String, Charset)} in the new developments.
	 */
	public static String unFog(String string) {
		if ((string == null) || (string.length() == 0) || (string.length() == MINIMAL_ENCODED_STRING_LENGHT)) {
			return ""; //$NON-NLS-1$
		}
		if (string.length() < MINIMAL_ENCODED_STRING_LENGHT) {
			return string;
		}
		string = new String(hexStringToByteArray(reverse(string)));
		return string.substring(RANDON_ENCODED_STRING);
	}
	
	/**
	 * Decode a string encoded with the <code>fog</code> method.
	 * 
	 * @param string
	 * @param charset the same charset that the one used when the key was encrypted.
	 * @return
	 * @see #fog(String)
	 */
	public static String unFog(String string, Charset charset) {
		if ((string == null) || (string.length() == 0) || (string.length() == MINIMAL_ENCODED_STRING_LENGHT)) {
			return ""; //$NON-NLS-1$
		}
		if (string.length() < MINIMAL_ENCODED_STRING_LENGHT) {
			return string;
		}
		string = new String(hexStringToByteArray(reverse(string)), charset);
		return string.substring(RANDON_ENCODED_STRING);
	}
	
	/**
	 * Decode a string encoded with the <code>fog</code> method.
	 * 
	 * @param string hex-string
	 * @param charset if null use UTF8.
	 * @return
	 * @see #fog(String)
	 */
	public static char[] unFogChar(String string, Charset charset) {
		if ((string == null) || (string.length() == 0) || (string.length() == MINIMAL_ENCODED_STRING_LENGHT)) {
			return new char[0];
		}
		if (string.length() < MINIMAL_ENCODED_STRING_LENGHT) {
			return string.toCharArray();
		}
		if (charset == null) {
			charset  = StandardCharsets.UTF_8;
		}
		byte[] b = hexStringToByteArray(reverse(string));
		try {
			return getChars(b, charset, RANDON_ENCODED_STRING);
		} finally {
			clear(b);
		}
	}

	/**
	 * Decode a string encoded with the old version <code>encodeX64(String,String)</code> method.
	 * 
	 * @param passPhrase The secure key, the same as the one used with <code>encode</code> method.
	 * @param value
	 * @return The decoded value or the value itself if the crypting algorithm is not implemented into the current JVM.
	 * @deprecated not Secure implementation.
	 */
	@Deprecated
	public static String decodeX64(String passPhrase, String value) {
		if (value == null) {
			return null;
		}
		try {
			byte[] finalCiphertext = Base64.getDecoder().decode(value);
			byte[] salt = new byte[16];
			byte[] ivBytes = new byte[16];
			byte[] encValue = new byte[finalCiphertext.length - 32];
			System.arraycopy(finalCiphertext, 0, ivBytes, 0, 16);
			System.arraycopy(finalCiphertext, 16, encValue, 0, encValue.length);
			System.arraycopy(finalCiphertext, 16 + encValue.length, salt, 0, 16);
			KeySpec spec = new PBEKeySpec(passPhrase.toCharArray(), salt, 65536, 128); // AES-128
			SecretKeyFactory f = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1"); //$NON-NLS-1$
			SecretKey key = f.generateSecret(spec);
			SecretKey secret = new SecretKeySpec(key.getEncoded(), "AES"); //$NON-NLS-1$
			IvParameterSpec iv = new IvParameterSpec(ivBytes);
			Cipher c = Cipher.getInstance("AES/CBC/PKCS5Padding"); //$NON-NLS-1$ // Safe, only used to decode old string...
			c.init(Cipher.DECRYPT_MODE, secret, iv);
            return new String(c.doFinal(encValue), StandardCharsets.UTF_8); //$NON-NLS-1$
        } catch (Exception e) {
            return value;
        }
	}
	
	/**
	 * Decode a string encoded with the previous <code>encodeXE256(String)</code> method.
	 * 
	 * @param value
	 * @return The crypted value, or the value itself if an error occurs.
	 * @deprecated not Secure implementation.
	 */
	@Deprecated
	public static String decodeXE256(String value) {
		if (value == null) {
			return ""; //$NON-NLS-1$
		}
		try {
		    BufferedBlockCipher cipher = new PaddedBufferedBlockCipher(new CBCBlockCipher(new AESEngine()), new PKCS7Padding());
		    cipher.reset();
		    KeyParameter keyParam = new KeyParameter(KSE);
		    cipher.init(false, keyParam);
		    byte[] inputBytes = Base64.getDecoder().decode(value);
		    byte[] outputBytes = new byte[cipher.getOutputSize(inputBytes.length)];
		    int length = cipher.processBytes(inputBytes, 0, inputBytes.length, outputBytes, 0);
		    length += cipher.doFinal(outputBytes, length); //Do the final block
		    byte[] out = new byte[length];
		    System.arraycopy(outputBytes, 0, out, 0, length);
	        return new String(out, StandardCharsets.UTF_8); //$NON-NLS-1$
		} catch (Exception e) {
			return value;
		}
	}

	/**
	 * Decode a string encoded with the <code>encodeX64(String)</code> method.
	 * 
	 * @param value
	 * @return The decoded value or the value itself if the crypting algorithm is not implemented into the current JVM.
	 * @deprecated not Secure implementation.
	 */
	@Deprecated
	public static String decodeX64(String value) {
		return decodeX64(KS, value);
	}
	
	/**
	 * Get minimum of three values
	 */
	public static final int minimum(int a, int b, int c) {
		if (b < a) {
			a = b;
		}
		if (c < a) {
			return c;
		}
		return a;
	}


	/**
	 * Get minimum of four values
	 */
	public static final int minimum(int a, int b, int c, int d) {
		if (b < a) {
			a = b;
		}
		if (c < a) {
			a = c;
		}
		if (d < a) {
			return d;
		}
		return a;
	}

	/**
	 * Compare two string and return the number of different characters.
	 * 
	 * @param s1 a String, can be null.
	 * @param s2 a String, can be null.
	 * @param maxDiff the maximal difference
	 * @return a number from 0 to maxDiff
	 * @see #diffCharCount(String, String)
	 */
	public static final int compare(String s1, String s2, int maxDiff) {
		return compare(s1.toCharArray(), s2.toCharArray(), maxDiff);
	}
	
	/**
	 * Compare two char[] and return the number of different characters.
	 * 
	 * @param s1 can be null.
	 * @param s2 can be null.
	 * @param maxDiff the maximal difference
	 * @return a number from 0 to maxDiff
	 * @see #diffCharCount(String, String)
	 */
	public static final int compare(char[] s1, char[] s2, int maxDiff) {
		if (s1 == null) {
			if (s2 == null) {
				return 0;
			}
			if (s2.length > maxDiff) {
				return maxDiff;
			}
			return s2.length;
		}
		if (s2 == null) {
			if (s1.length > maxDiff) {
				return maxDiff;
			}
			return s1.length;
		}
		int diff = 0;
		int l = s1.length;
		if (l > s2.length) {
			diff = l - s2.length;
			l = s2.length;
		} else if (l < s2.length) {
			diff = s2.length - l;
		}
		if (diff >= maxDiff) {
			return maxDiff;
		}
		for(int i = 0; i < l; i++) {
			if (s1[i] != s2[i]) {
				if ((++diff) >= maxDiff) {
					return maxDiff;
				}
			}
		}
		return diff;
	}
	
	/**
	 * For large values test is the current JVM have enough memory space to run a Levenshtein distance algorithm.
	 * 
	 * @param s1
	 * @param s2
	 * @return
	 */
	public static boolean testMemoryUsage(String s1, String s2) {
		return testMemoryUsage(s1.toCharArray(), s2.toCharArray());
	}
	
	/**
	 * For large values test is the current JVM have enough memory space to run a Levenshtein distance algorithm.
	 * 
	 * @param s1
	 * @param s2
	 * @return
	 */
	public static boolean testMemoryUsage(char[] s1, char[] s2) {
		if ((s1 == null) || (s2 == null)) {
			return true;
		}
		return Runtime.getRuntime().freeMemory() > ((long) s1.length + 1l) * ((long) s2.length + 1l) * (Integer.SIZE / 8l);
	}

	/**
	 * Compute the Levenshtein distance of two strings.
	 * 
	 * @param s
	 * @param t
	 * @return a positive or null distance.
	 * @see <a href="http://en.wikipedia.org/wiki/Levenshtein_distance">Wikipedia article</a>
	 */
	public static int LD(String s, String t) {
		return LD(s.toCharArray(), t.toCharArray());
	}
	
	/**
	 * Compute the Levenshtein distance of two strings.
	 * 
	 * @param s
	 * @param t
	 * @return a positive or null distance.
	 * @see <a href="http://en.wikipedia.org/wiki/Levenshtein_distance">Wikipedia article</a>
	 */
	public static int LD(char[] s, char[] t) {
		if (s == null) {
			if (t == null) {
				return 0;
			}
			return t.length;
		}
		int n = s.length; // length of s
		if (t == null) {
			return n;
		}
		int m = t.length; // length of t
		if (n == 0) {
			return m;
		}
		if (m == 0) {
			return n;
		}
		int d[][] = new int[n + 1][m + 1]; // matrix
		for (int i = 0; i <= n; i++) {
			d[i][0] = i;
		}
		for (int j = 0; j <= m; j++) {
			d[0][j] = j;
		}
		for (int i = 1; i <= n; i++) {
			char s_i = s[i - 1];
			for (int j = 1; j <= m; j++) {
				char t_j = t[j - 1];
				if (s_i == t_j) {
					d[i][j] = d[i - 1][j - 1];
				} else {
					d[i][j] = minimum(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + 1);
				}
			}
		}
		return d[n][m];
	}

	/**
	 * Compute the Levenshtein distance of two strings, case insentivie.
	 * 
	 * @param s
	 * @param t
	 * @return a positive or null distance.
	 * @see <a href="http://en.wikipedia.org/wiki/Levenshtein_distance">Wikipedia article</a>
	 */
	public static int LDCaseInsensitive(String s, String t) {
		return LDCaseInsensitive(s.toCharArray(), t.toCharArray());
	}

	/**
	 * Compute the Levenshtein distance of two strings, case insentivie.
	 * 
	 * @param s
	 * @param t
	 * @return a positive or null distance.
	 * @see <a href="http://en.wikipedia.org/wiki/Levenshtein_distance">Wikipedia article</a>
	 */
	public static int LDCaseInsensitive(char[] s, char[] t) {
		if (s == null) {
			if (t == null) {
				return 0;
			}
			return t.length;
		}
		int n = s.length;
		if (t == null) {
			return n;
		}
		int m = t.length;
		if (n == 0) {
			return m;
		}
		if (m == 0) {
			return n;
		}
		int d[][] = new int[n + 1][m + 1];
		for (int i = 0; i <= n; i++) {
			d[i][0] = i;
		}
		for (int j = 0; j <= m; j++) {
			d[0][j] = j;
		}
		for (int i = 1; i <= n; i++) {
			char s_i = Character.toLowerCase(s[i - 1]);
			for (int j = 1; j <= m; j++) {
				char t_j = Character.toLowerCase(t[j - 1]);
				if (s_i == t_j) {
					d[i][j] = d[i - 1][j - 1];
				} else {
					d[i][j] = minimum(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + 1);
				}
			}
		}
		return d[n][m];
	}

	/**
	 * Compute the Damerau-Levenshtein distance fo two strings.
	 * 
	 * @param s
	 * @param t
	 * @return a positive or null distance.
	 * @see <a href="http://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance">Wikipedia article</a>
	 */
	public static int DLD(String s, String t) {
		return DLD(s.toCharArray(), t.toCharArray());
	}

	/**
	 * Compute the Damerau-Levenshtein distance fo two strings.
	 * 
	 * @param s
	 * @param t
	 * @return a positive or null distance.
	 * @see <a href="http://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance">Wikipedia article</a>
	 */
	public static int DLD(char[] s, char[] t) {
		if (s == null) {
			if (t == null) {
				return 0;
			}
			return t.length;
		}
		int n = s.length;
		if (t == null) {
			return n;
		}
		int m = t.length;
		if (n == 0) {
			return m;
		}
		if (m == 0) {
			return n;
		}
		int d[][] = new int[n + 1][m + 1];
		for (int i = 0; i <= n; i++) {
			d[i][0] = i;
		}
		for (int j = 0; j <= m; j++) {
			d[0][j] = j;
		}
		char s_i = s[0];
		for (int j = 1; j <= m; j++) {
			char t_j = t[j - 1];
			if (s_i == t_j) {
				d[1][j] = d[0][j - 1];
			} else {
				d[1][j] = minimum(d[0][j] + 1, d[1][j - 1] + 1, d[0][j - 1] + 1);
			}
		}
		char s_pi = s_i;
		for (int i = 2; i <= n; i++) {
			s_i = s[i - 1];
			char t_j = t[0];
			if (s_i == t_j) {
				d[i][1] = d[i - 1][0];
			} else {
				d[i][1] = minimum(d[i - 1][1] + 1, d[i][0] + 1, d[i - 1][0] + 1);
			}
			char t_pj = t_j;
			for (int j = 2; j <= m; j++) {
				t_j = t[j - 1];
				if ((s_i == t_pj) && (s_pi == t_j)) {
					if (s_i == t_j) {
						d[i][j] = minimum(
								d[i-1][j] + 1, // deletion
                                d[i][j-1] + 1, // insertion
                                d[i-1][j-1], // substitution
                                d[i-2][j-2]); // transposition
					} else {
						d[i][j] = minimum(
								d[i-1][j] + 1, // deletion
                                d[i][j-1] + 1, // insertion
                                d[i-1][j-1] + 1, // substitution
                                d[i-2][j-2] + 1); // transposition
					}
				} else if (s_i == t_j) {
					d[i][j] = d[i - 1][j - 1];
				} else {
					d[i][j] = minimum(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + 1);
				}
				t_pj = t_j;
			}
			s_pi = s_i;
		}
		return d[n][m];
	}

	/**
	 * Compute the Damerau-Levenshtein distance fo two strings.
	 * 
	 * @param s
	 * @param t
	 * @return a positive or null distance.
	 * @see <a href="http://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance">Wikipedia article</a>
	 */
	public static int DLDCaseInsensitive(String s, String t) {
		return DLDCaseInsensitive(s.toCharArray(), t.toCharArray());
	}

	/**
	 * Compute the Damerau-Levenshtein distance fo two strings.
	 * 
	 * @param s
	 * @param t
	 * @return a positive or null distance.
	 * @see <a href="http://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance">Wikipedia article</a>
	 */
	public static int DLDCaseInsensitive(char[] s, char[] t) {
		if (s == null) {
			if (t == null) {
				return 0;
			}
			return t.length;
		}
		int n = s.length;
		if (t == null) {
			return n;
		}
		int m = t.length;
		if (n == 0) {
			return m;
		}
		if (m == 0) {
			return n;
		}
		int d[][] = new int[n + 1][m + 1];
		for (int i = 0; i <= n; i++) {
			d[i][0] = i;
		}
		for (int j = 0; j <= m; j++) {
			d[0][j] = j;
		}
		char s_i = Character.toLowerCase(s[0]);
		for (int j = 1; j <= m; j++) {
			char t_j = Character.toLowerCase(t[j - 1]);
			if (s_i == t_j) {
				d[1][j] = d[0][j - 1];
			} else {
				d[1][j] = minimum(d[0][j] + 1, d[1][j - 1] + 1, d[0][j - 1] + 1);
			}
		}
		char s_pi = s_i;
		for (int i = 2; i <= n; i++) {
			s_i = Character.toLowerCase(s[i - 1]);
			char t_j = Character.toLowerCase(t[0]);
			if (s_i == t_j) {
				d[i][1] = d[i - 1][0];
			} else {
				d[i][1] = minimum(d[i - 1][1] + 1, d[i][0] + 1, d[i - 1][0] + 1);
			}
			char t_pj = t_j;
			for (int j = 1; j <= m; j++) {
				t_j = Character.toLowerCase(t[j - 1]);
				if ((s_i == t_pj) && (s_pi == t_j)) {
					if (s_i == t_j) {
						d[i][j] = minimum(
								d[i-1][j] + 1, // deletion
                                d[i][j-1] + 1, // insertion
                                d[i-1][j-1], // substitution
                                d[i-2][j-2]); // transposition
					} else {
						d[i][j] = minimum(
								d[i-1][j] + 1, // deletion
                                d[i][j-1] + 1, // insertion
                                d[i-1][j-1] + 1, // substitution
                                d[i-2][j-2] + 1); // transposition
					}
				} else if (s_i == t_j) {
					d[i][j] = d[i - 1][j - 1];
				} else {
					d[i][j] = minimum(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + 1);
				}
				t_pj = t_j;
			}
			s_pi = s_i;
		}
		return d[n][m];
	}

	/**
	 * Convert a byte[] array to readable string format. This makes the "hex"
	 * readable!
	 * 
	 * @param b byte[] buffer to convert to string format
	 * @return given byte array in String format
	 */
	public static String byteArrayToHexString(byte[] b) {
		StringBuilder sb = new StringBuilder(b.length * 2);
		for (int i = 0; i < b.length; i++) {
			int v = b[i] & 0xff;
			if (v < 16) {
				// Allignement.
				sb.append('0');
			}
			sb.append(Integer.toHexString(v));
		}
		return sb.toString().toUpperCase();
	}

	/**
	 * Convert a byte[] array to readable string format. This makes the "hex"
	 * readable!
	 * 
	 * @param b byte[] buffer to convert to string format
	 * @return given byte array in String format
	 */
	public static String byteArrayToHexString(byte[] b, char sep) {
		StringBuilder sb = new StringBuilder(b.length * 2);
		for (int i = 0; i < b.length; i++) {
			int v = b[i] & 0xff;
			if (sb.length() > 0) {
				sb.append(sep);
			}
			if (v < 16) {
				// Allignement.
				sb.append('0');
			}
			sb.append(Integer.toHexString(v));
		}
		return sb.toString().toUpperCase();
	}

	/**
	 * Test the given text and return true if it is an Hexadecimal representation of a byte array.
	 * 
	 * @param text
	 * @return
	 */
	public static boolean isHexString(String text) {
		if (text == null) {
			return false;
		}
		for (char c: text.toCharArray()) {
			if ((c < 48) || ((c > 57) && (c < 65)) || (c > 70)) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * Convert an hexadecinal (without prefix) representation String to a byte array.
	 * 
	 * @param s The string has represent an hexadecimal number.  
	 * @return The byte representation of the given String, never return null. 
	 */
	public static byte[] hexStringToByteArray(String s) {
		if (s == null) {
			return new byte[0];
		}
		s = s.trim();
		int l = s.length();
		if (l == 0) {
			return new byte[0];
		}
		if ((l % 2) == 1) {
			s = "0" + s;
			l++;
		}
		byte[] b = new byte[l / 2];
		for (int i = 0; i < b.length; i++) {
			int index = i * 2;
			int v = Integer.parseInt(s.substring(index, index + 2), 16);
			b[i] = (byte) v;
		}
		return b;
	}

	/**
	 * Convert a char or an array of chars into an UTF-8 byte array.
	 * @param chars
	 * @return
	 */
	public static byte[] charToBytes(char... chars) {
		Charset charset = Charset.forName("UTF-8"); //$NON-NLS-1$
		ByteBuffer buffer = charset.encode(CharBuffer.wrap(chars));
		return Arrays.copyOf(buffer.array(), buffer.limit());
	}
	
	/**
	 * Return the number charSet member in the string.
	 * 
	 * @param string
	 * @param charSet
	 * @return
	 */
	public static int countCharsInString(String string, String charSet) {
		return countCharsInString(string.toCharArray(), charSet);
	}		

	/**
	 * Return the number charSet member in the string.
	 * 
	 * @param string
	 * @param charSet
	 * @return
	 */
	public static int countCharsInString(char[] string, String charSet) {
		if (string == null) {
			return 0;
		}
		int count = 0;
		for (int i = 0; i < string.length; i++) {
			if (charSet.indexOf(string[i]) > -1) {
				count++;
			}
		}
		return count;
	}

	/**
	 * Return the number charSet member in the string.
	 * 
	 * @param string
	 * @param charSet
	 * @return
	 */
	public static int countCharsInString(char[] string, char[] charSet) {
		if (string == null) {
			return 0;
		}
		int count = 0;
		for (int i = 0; i < string.length; i++) {
			for (int j = 0; j < charSet.length; j++) {
				if (charSet[j] == string[i]) {
					count++;
					break;
				}
			}
		}
		return count;
	}

	/**
	 * Return the number of character from the string that are <b>not</b> in the specified charset.
	 * 
	 * @param string
	 * @param charSet
	 * @return
	 */
	public static int countCharsNotInString(String string, String charSet) {
		return countCharsNotInString(string.toCharArray(), charSet);
	}

	/**
	 * Return the number of character from the string that are <b>not</b> in the specified charset.
	 * 
	 * @param string
	 * @param charSet
	 * @return
	 */
	public static int countCharsNotInString(char[] string, String charSet) {
		int count = 0;
		if (string == null) {
			return 0;
		}
		for (int i = 0; i < string.length; i++) {
			if (charSet.indexOf(string[i]) < 0) {
				count++;
			}
		}
		return count;
	}

	/**
	 * Retourne le nombre de chiffre dans la chaîne.
	 * 
	 * @param string
	 * @return
	 * @deprecated this method does not belong to crytography and more over is not Unicode compliant.
	 */
	public static int digitCount(String string) {
		return countCharsInString(string, DIGITS);
	}

	/**
	 * Retourne le nombre de lettres dans la chaîne.
	 * 
	 * @param string
	 * @return
	 * @deprecated this method does not belong to crytography and more over is not Unicode compliant.
	 */
	public static int alphaCount(String string) {
		return countCharsInString(string, ALPHA); //$NON-NLS-1$
	}

	/**
	 * Retourne le nombre de lettres dans la chaîne.
	 * 
	 * @param string
	 * @return
	 * @deprecated this method does not belong to crytography and more over is not Unicode compliant.
	 */
	public static int alphaNumCount(String string) {
		return countCharsInString(string, ALPHANUM);
	}

	/**
	 * Retourne le nombre de caractères non alpha-numérique dans la chaîne.
	 * 
	 * @param string
	 * @return
	 * @deprecated this method does not belong to crytography and more over is not Unicode compliant.
	 */
	public static int nonAlphaNumCount(String string) {
		return countCharsNotInString(string, ALPHANUM);
	}

	/**
	 * Retourne le nombre de lettres en majuscules dans la chaîne.
	 * 
	 * @param string
	 * @return
	 * @deprecated this method does not belong to crytography and more over is not Unicode compliant.
	 */
	public static int alphaMajCount(String string) {
		return countCharsInString(string, ALPHAHIGH);
	}

	/**
	 * Retourne le nombre de lettres en minuscules dans la chaîne.
	 * 
	 * @param string
	 * @return
	 * @deprecated this method does not belong to crytography and more over is not Unicode compliant.
	 */
	public static int alphaMinCount(String string) {
		return countCharsInString(string, ALPHALOW);
	}

	/**
	 * Retourne le nombre de chiffre dans la chaîne.
	 * 
	 * @param string
	 * @return
	 * @deprecated this method does not belong to crytography and more over is not Unicode compliant.
	 */
	public static int digitCount(char[] string) {
		return countCharsInString(string, DIGITS);
	}

	/**
	 * Retourne le nombre de lettres dans la chaîne.
	 * 
	 * @param string
	 * @return
	 * @deprecated this method does not belong to crytography and more over is not Unicode compliant.
	 */
	public static int alphaCount(char[] string) {
		return countCharsInString(string, ALPHA); //$NON-NLS-1$
	}

	/**
	 * Retourne le nombre de lettres dans la chaîne.
	 * 
	 * @param string
	 * @return
	 * @deprecated this method does not belong to crytography and more over is not Unicode compliant.
	 */
	public static int alphaNumCount(char[] string) {
		return countCharsInString(string, ALPHANUM);
	}

	/**
	 * Retourne le nombre de caractères non alpha-numérique dans la chaîne.
	 * 
	 * @param string
	 * @return
	 * @deprecated this method does not belong to crytography and more over is not Unicode compliant.
	 */
	public static int nonAlphaNumCount(char[] string) {
		return countCharsNotInString(string, ALPHANUM);
	}

	/**
	 * Retourne le nombre de lettres en majuscules dans la chaîne.
	 * 
	 * @param string
	 * @return
	 * @deprecated this method does not belong to crytography and more over is not Unicode compliant.
	 */
	public static int alphaMajCount(char[] string) {
		return countCharsInString(string, ALPHAHIGH);
	}

	/**
	 * Retourne le nombre de lettres en minuscules dans la chaîne.
	 * 
	 * @param string
	 * @return
	 * @deprecated this method does not belong to crytography and more over is not Unicode compliant.
	 */
	public static int alphaMinCount(char[] string) {
		return countCharsInString(string, ALPHALOW);
	}

	/**
	 * Get the number of different characters into the given string.
	 * 
	 * @param string
	 * @return
	 */
	public static int diffCharCount(String string) {
		return diffCharCount(string.toCharArray());
	}

	/**
	 * Get the number of different characters into the given string.
	 * 
	 * @param string may be null
	 * @return
	 */
	public static int diffCharCount(char[] string) {
		if (string == null) {
			return 0;
		}
		int[] diff = new int[string.length]; //$NON-NLS-1$
		int difflen = 0;
		for (int i = 0; i < string.length; i++) {
			char c = string[i];
			int j = 0;
			while (j < difflen) {
				if (string[diff[j]] == c) {
					break;
				}
				j++;
			}
			if (j == difflen) {
				diff[difflen++] = i;
			}
		}
		return difflen;
	}

	/**
	 * Compute the number of different character into the two string.
	 * 
	 * <p>
	 * Support null strings.
	 * 
	 * @param stringA
	 * @param stringB
	 * @return a null or positive number.
	 * @see #compare(String, String, int)
	 */
	public static int diffCharCount(String stringA, String stringB) {
		return diffCharCount(stringA.toCharArray(), stringB.toCharArray());
	}

	/**
	 * Compute the number of different character into the two char[].
	 * 
	 * <p>
	 * Support null array.
	 * 
	 * @param stringA
	 * @param stringB
	 * @return a null or positive number.
	 * @see #compare(String, String, int)
	 */
	public static int diffCharCount(char[] stringA, char[] stringB) {
		if (stringA == null) {
			if (stringB == null) {
				return 0;
			}
			return stringB.length;
		} 
		if (stringB == null) {
			return stringA.length;
		}
		int count = stringA.length - stringB.length;
		if (count < 0) {
			count = -count;
			char[] pivot = stringA;
			stringA = stringB;
			stringB = pivot;
		}
		for (int i = 0; i < stringB.length; i++) {
			if (stringA[i] != stringB[i]) {
				count++;
			}
		}
		return count;
	}

	/**
	 * Reverse a String.
	 * 
	 * @param string
	 * @return
	 */
	public static final String reverse(String string) {
		if (string == null) {
			return null;
		}
		return new StringBuilder(string).reverse().toString();
	}

	/**
	 * Reverse the content of a char[].
	 * 
	 * @param string
	 * @return
	 */
	public static final void reverse(char[] text) {
		if (text != null) {
			int j = text.length - 1;
			int hl = text.length / 2;
			for(int i = 0; i < hl; i++) {
				char p = text[i];
				text[i] = text[j];
				text[j] = p;
				j--;
			}
		}
	}

	/**
	 * Scramble a String !
	 * 
	 * @param string
	 * @return
	 */
	public static final String scramble(String string) {
		if (string == null) {
			return null;
		}
		char[] c = string.toCharArray();
		scramble(c);
		return new String(c);
	}

	/**
	 * Scramble a char[] !
	 * 
	 * @return
	 */
	public static final void scramble(char[] text) {
		if ((text != null) && (text.length > 0)) {
			char x;
			int j, k;
			for(int i = text.length * 2; i >= 0; i--) {
				j = RandomGenerator.randInteger(text.length);
				k = RandomGenerator.randInteger(text.length);
				x = text[j];
				text[j] = text[k];
				text[k] = x;
			}
		}
	}

	/**
	 * Scramble a String in a way that the result is always the same for a given string.
	 * 
	 * @param string
	 * @return
	 */
	public static final char[] scrambleRep(String text) {
		if (text == null) {
			return null;
		}
		final char[] c = text.toCharArray();
		scrambleRep(c);
		return c;
	}

	/**
	 * Scramble a char[] in a way that the result is always the same for a given string.
	 * 
	 * @param a Char array.
	 */
	public static final void scrambleRep(char[] text) {
		if (text != null) {
			int h = 0;
	        for (int i = 0; i < text.length; i++) {
	        	h = 31 * h + text[i];
	        }
			Random rd = new Random(h);
			for(int i = 0; i < text.length; i++) {
				int j = Math.abs(rd.nextInt(text.length));
				char c = text[i];
				text[i] = text[j];
				text[j] = c;
			}
		}
	}

	/**
	 * Utility method that return the UTF8 bytes representation of the given string.
	 * 
	 * @param string
	 * @return the UTF8 representation of the string, or the string in the current Locale if it is not compatible with UTF8 encoding.
	 */
	public static final byte[] getBytes(String string) {
		return string.getBytes(StandardCharsets.UTF_8);
	}
	
	/**
	 * Get the UTF-8 byte representation of the given text...
	 * 
	 * @param text
	 * @return
	 */
	public static final byte[] getBytes(char[] text) {
		if ((text == null) || (text.length == 0)) {
			return new byte[0];
		}
		ByteBuffer bb = StandardCharsets.UTF_8.encode(CharBuffer.wrap(text));
		byte[] b = bb.array();
		try {
			return Arrays.copyOf(b, bb.limit());
		} finally {
			// Remove the data before garbage collection.
			clear(b);
		}
	}

	/**
	 * Get the Character representation of a given byte array, assuming that
	 * theses bytes represent an UTF-8 encoding.
	 * 
	 * @param data
	 * @return
	 */
	public static final char[] getChars(byte[] data, Charset charset, int beginpos) {
		if (data == null) {
			return null;
		}
		CharBuffer bc = charset.decode(ByteBuffer.wrap(data));
		char[] c = bc.array();
		try {
			char[] result = new char[bc.limit() - beginpos];
			System.arraycopy(c, beginpos, result, 0, result.length);
			return result;
		} finally {
			// Remove the data before garbage collection.
			clear(c);
		}
	}

	/**
	 * Utility methods that replace all char from a char array with <i>nul</i> char.
	 *  
	 * @param text
	 */
	public static final void clear(char[] text) {
		if (text != null) {
			for(int i = text.length - 1; i >= 0; i--) {
				text[i] = 0;
			}
		}
	}

	/**
	 * Utility methods that replace all byte from an array with zero.
	 *  
	 * @param text
	 */
	public static final void clear(byte[] text) {
		if (text != null) {
			for(int i = text.length - 1; i >= 0; i--) {
				text[i] = 0;
			}
		}
	}
	
	/**
	 * Return true if the parameter is null, empty or contain only null character.
	 * 
	 * @param text
	 * @return
	 */
	public static final boolean isNull(char[] text) {
		if ((text != null) && (text.length > 0)) {
			for (char c : text) {
				if (c != 0) {
					return false;
				}
			}
		}
		return true;
	}
	
	/**
	 * Hash with the Whirlpool algorithm.
	 * 
	 * <p>
	 * The string is converted into byte array, using the platform locale.
	 * 
	 * <p>
	 * <b>Note that the Whirlpool hash algorithm is secure but too fast and weak against brute force attack, do not use it for encryption of strategic information.</b>
	 * 
	 * <p>
	 * WARN: This method is equal to the previous version, including the bugs !!!
	 * 
	 * @param text
	 * @return An hexadecimal representation of the hash.
	 * @deprecated 
	 */
	private static String whirlpoolEx(char[] text) {
		// This implementation is compatible with the GNU Crypto implementation.
		// Note that the BouncyCastle implementation is not initialized in the same way and produce different hashes !
		return byteArrayToHexString(new Whirlpool(text).digest());
	}
		
	private static void reseedSecureRandom() {
        srcount++;
        if (srcount > 200) {
        	SECURERANDOM.setSeed(SECURERANDOM.generateSeed(100));
        	srcount = 0;
        }
	}
	
	private static void setIntByte(byte[] array, int pos, int value) {
		array[pos] = (byte) ((value >> 24) & 0xFF);
		array[pos+1] = (byte) ((value >> 16) & 0xFF);
		array[pos+2] = (byte) ((value >> 8) & 0xFF);
		array[pos+3] = (byte) (value & 0xFF);
	}

	private static int getIntByte(byte[] array, int pos) {
		return ((0xFF & array[pos]) << 24) | ((0xFF & array[pos + 1]) << 16) | ((0xFF & array[pos + 2]) << 8) | (0xFF & array[pos + 3]);    
	}

	private Crypto() {
		super();
	}
}
