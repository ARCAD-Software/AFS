import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;

import javax.crypto.Cipher;

import org.junit.Assert;
import org.junit.jupiter.api.Test;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.crypt.KeyStoreManager;

class TestCrypto {

	// ATTENTION Cette classe DOIT être enregistrée en UTF8.
	private static final String RANDOMSTRING1 = "@`"; //$NON-NLS-1$
	private static final String RANDOMSTRING2 = ")°'"; //$NON-NLS-1$
	private static final String RANDOMSTRING3 = "=sµ"; //$NON-NLS-1$
	private static final String RANDOMSTRING4 = "é£L"; //$NON-NLS-1$
	private static final String RANDOMSTRING5 = "ȣĪ®"; //$NON-NLS-1$
	private static final String RANDOMSTRING6 = "1üJ"; //$NON-NLS-1$
	private static final String RANDOMSTRING7 = "ĂƇ©%"; //$NON-NLS-1$
	private static final String ALPHALOW = "abcdefghijklmnopqrstuvwxyz"; //$NON-NLS-1$
	private static final String ALPHAHIGH = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"; //$NON-NLS-1$
	protected static final String ALPHA = ALPHALOW + ALPHAHIGH;
	private static final String DIGITS = "0123456789"; //$NON-NLS-1$
	protected static final String ALPHANUM = ALPHA + DIGITS;

	@Test
	void legalLimitation() throws Exception {
		int max = Cipher.getMaxAllowedKeyLength("AES") / 8; //$NON-NLS-1$
		System.out.println("Limite légale AES = " + max + " octets.");
		max = Cipher.getMaxAllowedKeyLength("PBKDF2WithHmacSHA1") / 8; //$NON-NLS-1$
		System.out.println("Limite légale SHA1 = " + max + " octets.");
		max = Cipher.getMaxAllowedKeyLength("SHA256") / 8; //$NON-NLS-1$
		System.out.println("Limite légale SHA256 = " + max + " octets.");
	}
	
	private static void setIntByte(byte[] array, int pos, int value) {
		array[pos] = (byte) ((value >> 24) & 0xFF);
		array[pos + 1] = (byte) ((value >> 16) & 0xFF);
		array[pos + 2] = (byte) ((value >> 8) & 0xFF);
		array[pos + 3] = (byte) (value & 0xFF);
	}

	private static int getIntByte(byte[] array, int pos) {
		return ((0xFF & array[pos]) << 24) | ((0xFF & array[pos + 1]) << 16) | ((0xFF & array[pos + 2]) << 8)
				| (0xFF & array[pos + 3]);
	}

	@Test
	void testByteInt() {
		byte[] array = new byte[12];
		int a = 1234567;
		int b = 2;
		int c = -34455;
		setIntByte(array, 0, a);
		setIntByte(array, 4, b);
		setIntByte(array, 8, c);
		System.out.println(Crypto.byteArrayToHexString(array));
		System.out.println();
		assertEquals(a, getIntByte(array, 0));
		assertEquals(b, getIntByte(array, 4));
		assertEquals(c, getIntByte(array, 8));
	}

	@Test
	void testWrilpool() {
		String mdp = "Voici un texte relativement long, plus c'est long mieux c'est moin long après enfin c'est tout pareil, on s'en fout en fait...";
		long t = System.currentTimeMillis();
		String h = null;
		for (int i = 0; i < 100; i++) {
			h = Crypto.whirlpool(mdp.toCharArray());
		}
		t = (System.currentTimeMillis() - t) / 100;
		System.out.println("Whirlpool = " + h);
		System.out.println("Taille = " + h.length());
		System.out.println("durée = " + t + "ms.");
		System.out.println();
		// Ancien hashage
		assertEquals(true, Crypto.matches(
				"92D34ED36C99A0DEA787766C6C2FC060EF533F4D09853D5E46D757A858E8C5830743B8D74BA205996D53794DE3A473F67255550509E3B43098C98A6D1FBD608E",
				"quadra".toCharArray()));
		System.out.println();
	}

	@Test
	void testScramble() {
		String v = "azerty";
		System.out.println(v);
		System.out.println(new String(Crypto.scrambleRep(v)));
		System.out.println(new String(Crypto.scrambleRep(v)));
		System.out.println(new String(Crypto.scrambleRep(v)));
		System.out.println();
		v = "abcdefghjklmnopqrstuvwxyz";
		System.out.println(v);
		System.out.println(new String(Crypto.scrambleRep(v)));
		System.out.println();
		v = "Bonjour est ce que ce texte est toujours lisible comme çà ?";
		System.out.println(v);
		long t = System.currentTimeMillis();
		System.out.println(new String(Crypto.scrambleRep(v)));
		System.out.println(new String(Crypto.scrambleRep(v)));
		System.out.println(new String(Crypto.scrambleRep(v)));
		t = (System.currentTimeMillis() - t) / 20;
		System.out.println("Timing = " + t + "ms");
		System.out.println();
	}

	@Test
	void testHash() {
		// BC 1.72 introduced a bug... a Yield is required before any usage of the hash method !
		//Thread.yield();
		long t = System.currentTimeMillis();
		String h1 = Crypto.hash("monmotdepasseilestcool".toCharArray());
		String h2 = Crypto.hash("monmotdepasseilestcoul");
		for (int i = 1; i < 10; i++) {
			h2 = Crypto.hash("monmotdepasseilestcoul");
		}
		String h3 = Crypto.hash("quadra");
		t = (System.currentTimeMillis() - t) / 13;
		System.out.println("Hash Timing = " + t + "ms.");
		System.out.println(" h1 = " + h1);
		System.out.println(" h1 len = " + h1.length());
		System.out.println(" h2 = " + h2);
		System.out.println(" h2 len = " + h2.length());
		System.out.println(" quadra = " + h3);
		System.out.println();
		assertEquals(true, Crypto.isHashSecure(h2));
		assertEquals(true, Crypto.matches(h1, "monmotdepasseilestcool"));
		assertEquals(false, Crypto.matches(h2, "monmotdepasseilestcool"));
		// Algorithm = 1
		assertEquals(true, Crypto.matches(
				"AAAAAQAAH0AAAAAQMnw2OWAU003Gm8xHTWfNZVOu15CmLEokZakqxc6PBd8nsq88n9D1eoLZz6YElsPmv1qrDWmQ+O/uKN3jtyQ5fw04IbwVowGoDV7FVVH2HS9QekjVZmFlsaXgFxyXvJcVTC6+4gDdY7cpVQtjqe0anPQbOgw/Jx9oFMGugGKvN/zqx6vdf00ATIrw1foeX3v7",
				"quadra"));
	}

	@Test
	void testHash2() {
		long t = 0;
		for (String p : new String[] { "azerty", "qsdfgh", "wxcvbn", "poiuyt", "mlkjhg", "nbvcxw", "aqwzsx", "edcvfr",
				"yhnbgt", "ikjuyh" }) {
			long tx = System.currentTimeMillis();
			System.out.println(Crypto.hash(p));
			System.out.println(Crypto.hash(p));
			t += (System.currentTimeMillis() - tx);
			boolean test = Crypto.matches(Crypto.hash(p), p);
			assertEquals(true, test);
		}
		t = t / 20;
		System.out.println("Hash Timing = " + t + "ms.");
		System.out.println();
	}

	@Test
	void testCryptOne() {
		Crypto.encrypt("0123456789AB".toCharArray());
	}
	
	@Test
	void testCrypt() {
		String p = "M0t 2 P@sse Möyen";
		String c = null;
		long t = System.currentTimeMillis();
		String c0 = Crypto.encrypt("");
		String c1 = Crypto.encrypt("a");
		String c2 = Crypto.encrypt("0123456789AB");
		String c3 = Crypto.encrypt(
				"tekljthzsldhjkfmslkjghfsdmlgkjhdsp girgeiighjporigjepogijsdmjknbfjb gj melthkj't,n:;,nrt;:,:;,cvbopopoêzẑo('o'p'(o(-_ç_è_àçoè_$_op$n't'nt'rtn");
		String c4 = Crypto.encrypt("quadra");
		for (int i = 0; i < 100; i++) {
			c = Crypto.encrypt(p);
		}
		t = (System.currentTimeMillis() - t) / 105;
		System.out.println("Encrypt Timing = " + t + "ms.");
		char[] pp = null;
		t = System.currentTimeMillis();
		for (int i = 0; i < 100; i++) {
			pp = Crypto.decrypt(c);
		}
		t = (System.currentTimeMillis() - t) / 100;
		System.out.println("Decrypt Timing = " + t + "ms.");
		System.out.println("Encrypt a = " + c1);
		System.out.println(" len = " + c1.length());
		System.out.println("Encrypt 0123456789AB = " + c2);
		System.out.println(" len = " + c2.length());
		System.out.println("Encrypt very long pwd = " + c3);
		System.out.println(" len = " + c3.length());
		System.out.println("Encrypt empty pwd = " + c0);
		System.out.println(" len = " + c0.length());
		System.out.println("Encrypt quadra = " + c4);
		System.out.println(" len = " + c4.length());
		System.out.println();
		assertEquals(true, Crypto.isCryptSecure(c));
		assertArrayEquals(p.toCharArray(), pp);
	}

	@Test
	void testDecrypt() {
		System.out.println("quadra = " + new String(Crypto.decrypt("quadra")));
		System.out.println("quadra = " + new String(Crypto.decrypt("1627461657170596C4352614E6A536A61677779527C67755A725874526665774779754747636")));
		System.out.println("quadra = " + new String(Crypto.decrypt("1627461657175565849667F6A4442546D486478755763494756615A515A717F42784569415A6")));
		System.out.println("quadra = " + new String(Crypto.decrypt("16274616571725967596A554452636072527353766E665548497E4968717A6B6154586A49564")));
	}

	@Test
	void testCryptAvecMasterKey() throws Exception {
		String p = "M0t 2 P@sse Möyen";
		char[] masterkey = new char[] { 'o', 's', 'µ', 'Ƈ', 's', 'g', '0', ' ', 'C', 'i', '1', 'o', '©', '.', '8', 'a',
				'\\', 'G', 'j', 'i', '\\', 'L', 'e', 'P', '\\', '.', '-', 'm', 'k', 'd',
				'e', 'a', 'l', 'r', '0', 'j', 'W', 'Q', '\\', 'O', 'S', '1', '5', 'L', '%', 'D', 'K', 'n', 'E', 'r',
				's', 'J', 'Ă', 'O', 'r', ' ', 'w', 'a', 'E', 'v', ':', '_', 'F', '1', '1', 'T', 'd',
				'=', 'M', 'P' };
		String c1 = Crypto.encrypt(p.toCharArray(), masterkey, 1);
		System.out.println("Encrypt with specific Master key, algorithm 1 = " + c1);
		String c2 = Crypto.encrypt(p.toCharArray(), masterkey, 2);
		System.out.println("Encrypt with specific Master key, algorithm 2 = " + c2);
		
		assertEquals(false, Crypto.isCryptSecure(c1));
		assertEquals(p, new String(Crypto.decrypt(c1, masterkey)));
		assertEquals(true, Crypto.isCryptSecure(c2));
		assertEquals(p, new String(Crypto.decrypt(c2, masterkey)));
	}

	@Test
	void testDiffCharCount() throws Exception {
		assertEquals(0, Crypto.diffCharCount((char[]) null));
		assertEquals(0, Crypto.diffCharCount(new char[0]));
		assertEquals(5, Crypto.diffCharCount(new char[] { 'a', 'Z', 'a', 'z', 'b', 'z', 'c' }));
	}

	@Test
	void testReverse() throws Exception {
		char[] source = new char[] { 'a', 'z', 'e', 'r', 't', 'y' };
		Crypto.reverse(source);
		assertArrayEquals(new char[] { 'y', 't', 'r', 'e', 'z', 'a' }, source);
		source = new char[] { 'a', 'z', 'e', 'r', 't', 'y', 'u' };
		Crypto.reverse(source);
		assertArrayEquals(new char[] { 'u', 'y', 't', 'r', 'e', 'z', 'a' }, source);
	}

	@Test
	void testFog() {
		String p = "quadra";
		System.out.println("FOG de " + p + " = " + Crypto.fog(p.toCharArray()));
	}
	
	@Test
	void testUnFog() {
		String p = "5627A6C5275667275635022756966696275665D24414342514C537E6F6964757C6F635024414342514C53756C6966402D6162776F62705C5A3345B2C37D3542425142414D24425D205C4529A2C786C284C0313023777F646E69675B62755A75516C61434B6247525242586D424751767A41564747747D634673485";
		System.out.println("UnFOG de " + p + "\n = " + Crypto.unFog(p));
	}

	@Test
	void testDecryptVerifier() throws Exception {
		char[] mkey = "Windows 10ĂƇ©%LP-RD-ABARBE=sµC:\\Program Files\\ARCAD Solutions\\ARCAD-Verifier Server\\jre".toCharArray();
		Crypto.scrambleRep(mkey);
		String s = "AAAAAQAAABMAAAAMAAAkBNsW6zb9cCsYYFo6FqYKi6/qp/QW+vjgXa1AiVUxwvmmaklg4X/sSN67ITqDE3wEbw2RGpt2kYFPIhlve6vFm5R9qEcjfXtFWHpA1IRGtGQRiMjkC4kPyqZc3+0jtPtCWdlyCwcx5hA=";
		int i = 1;
		while (Crypto.isCryptSecure(s)) {
			System.out.println("Decryp N°" + i);
			i++;
			s = new String(Crypto.decrypt(s, mkey));
			System.out.println(s);
		}
		System.out.println("Decrypté: " + s);
	}
	
	@Test
	void testEncryptASS() {
		byte[] N1HLAV;
		byte[] VALH1N;

		N1HLAV = new byte[] {48, -126, 2, 119, 2, 1, 0, 48, 13, 6, 9, 42, -122, 72, -122, -9, 13, 1, 1, 1, 
				5, 0, 4, -126, 2, 97, 48, -126, 2, 93, 2, 1, 0, 2, -127, -127, 0, -94, 4, 109, 
				-8, -125, 79, -10, 7, 116, -121, 115, -112, -16, 39, 124, -117, -73, -40, -66, -40, -43, 125, 98, 
				102, 71, 110, 32, -25, -104, 16, 57, 82, -7, -41, 80, 126, -96, 116, -73, 50, 19, -75, -67, 
				-99, 43, -40, -70, -111, -80, -69, -90, 48, 13, 87, -94, 94, -71, 57, -58, -70, -90, -57, -97, 
				-34, -124, 28, -52, 124, -92, -118, -96, 89, 86, -90, 23, -121, 118, 53, -12, -112, -71, -82, 24, 
				34, -68, -105, -26, 67, -77, 114, 55, -72, 84, 58, 5, -109, -98, 87, 63, -72, 105, 16, 14, 
				-15, -86, -71, 69, -28, -23, 98, -51, 4, 118, 44, 127, -111, -38, -126, 120, 101, -75, -100, 120, 
				-20, 103, -97, -110, -23, 2, 3, 1, 0, 1, 2, -127, -128, 26, 79, -93, -40, 78, -30, -26, 
				110, -111, -125, 9, -58, -120, -23, -80, -75, 51, -9, 20, 15, 60, -92, 111, 29, 115, 14, 52, 
				115, -57, -29, 34, -100, 18, 12, -32, 122, -97, -18, 52, 110, 2, 123, 106, 62, -93, 7, 64, 
				77, 54, -87, 82, -76, -51, 5, -27, 77, -126, -63, 26, 63, -25, -93, -43, -105, 88, 49, 6, 
				-58, 69, -81, 75, 90, -117, -8, 84, -18, -68, -94, -42, -56, -14, 126, 22, 93, 3, -80, 96, 
				60, 117, 77, -93, -57, -109, 79, -115, -8, -25, 27, -67, -121, -38, 109, -31, -8, -31, -42, 45, 
				-43, -56, -17, 51, 39, -60, -22, 105, 39, -6, 112, -35, -78, 110, 78, 97, 82, -32, 120, 67, 
				-79, 2, 65, 0, -52, -39, -9, 118, -64, 63, 118, -98, -118, 36, -88, -52, -87, 92, -65, -44, 
				-51, -101, -3, 13, 49, 92, -110, -4, 6, -74, 6, -46, 39, 59, 53, -24, -15, -62, 74, 97, 
				-58, -80, -65, 77, 44, -36, -97, -50, 4, -107, 18, 38, -54, -5, -11, -16, 57, 65, 44, -24, 
				-80, 53, -100, -96, -68, 0, 105, -83, 2, 65, 0, -54, 120, -123, 34, 116, -60, -64, 121, -40, 
				-46, -25, 84, -27, 125, 67, -114, 91, -65, -21, 61, 83, -82, -31, 38, -106, -96, -78, 46, 73, 
				28, 81, 96, 33, 125, 101, 13, 82, 16, -128, -85, 65, 87, -105, -20, 64, -80, 24, 18, -51, 
				7, 66, -93, -7, 7, -112, 63, -13, 19, -30, -93, -75, -17, -19, -83, 2, 64, 106, -108, -43, 
				-114, 49, -124, 83, -127, 53, 58, -47, -96, 122, 85, -55, 113, 60, -16, -11, -2, 81, 58, 107, 
				55, 57, 126, -89, -103, -37, -45, -112, -66, -4, 90, 44, 61, 39, 21, 122, -46, -6, -114, 103, 
				-114, -41, 15, -128, 124, 50, -116, 89, 111, 125, 58, -115, -43, 27, 41, 41, -82, -17, 97, -1, 
				-103, 2, 65, 0, -102, 23, -5, -117, 107, -65, 74, -107, -15, -107, 34, 6, -108, -38, 47, 14, 
				43, 121, -9, 57, 39, 117, 83, 90, 33, 13, -112, 78, 117, -54, 29, -116, -62, 48, 67, 54, 
				78, 28, -51, 48, -119, 124, 118, -35, -111, -91, -13, -52, 107, 74, 121, 71, 126, -126, 105, 32, 
				-94, -119, -95, -43, -30, -28, 5, -55, 2, 65, 0, -113, 47, 116, -101, 42, 70, 93, -79, 92, 
				-123, 78, -72, -56, -86, -108, -42, -110, 118, -13, -23, -19, -55, 28, 66, 33, 114, -89, -104, 55, 
				-110, -30, -89, -35, -119, -82, 31, -7, -43, -96, 16, -86, 82, -102, 97, 50, -18, -68, 16, -96, 
				-63, 125, -11, 73, 12, 22, 13, -115, 72, -89, 55, 115, -24, 67, 107};
		
				VALH1N = new byte[] {48, -127, -97, 48, 13, 6, 9, 42, -122, 72, -122, -9, 13, 1, 1, 1, 5, 0, 3, -127, 
				-115, 0, 48, -127, -119, 2, -127, -127, 0, -94, 4, 109, -8, -125, 79, -10, 7, 116, -121, 115, 
				-112, -16, 39, 124, -117, -73, -40, -66, -40, -43, 125, 98, 102, 71, 110, 32, -25, -104, 16, 57, 
				82, -7, -41, 80, 126, -96, 116, -73, 50, 19, -75, -67, -99, 43, -40, -70, -111, -80, -69, -90, 
				48, 13, 87, -94, 94, -71, 57, -58, -70, -90, -57, -97, -34, -124, 28, -52, 124, -92, -118, -96, 
				89, 86, -90, 23, -121, 118, 53, -12, -112, -71, -82, 24, 34, -68, -105, -26, 67, -77, 114, 55, 
				-72, 84, 58, 5, -109, -98, 87, 63, -72, 105, 16, 14, -15, -86, -71, 69, -28, -23, 98, -51, 
				4, 118, 44, 127, -111, -38, -126, 120, 101, -75, -100, 120, -20, 103, -97, -110, -23, 2, 3, 1, 
				0, 1};
		
		byte[] bArray = new byte[(Integer.SIZE / Byte.SIZE) * 4];
		ByteBuffer bBuffer = ByteBuffer.wrap(bArray);
		bBuffer.asIntBuffer().put(12);
		bBuffer.getInt();
		bBuffer.asIntBuffer().put(666);
		bBuffer.getInt();
		bBuffer.asIntBuffer().put(-852);
		bBuffer.getInt();
		bBuffer.asIntBuffer().put(456);
		String enc = Crypto.encryptA(bArray, N1HLAV);
		System.out.println("Encoded: " + enc);
		System.out.println(" len=" + enc.length());
		Assert.assertArrayEquals(bArray, Crypto.decryptAByte(enc, VALH1N));
	}


	@Test
	void testEncryptASS_KeyManager() throws Exception {
		System.out.println("Generation des Stores...");
		byte[] pri = null;
		byte[] pub = null;
		// ouvre the key stroe qui va stocker la clé privée...
		try (KeyStoreManager km = new KeyStoreManager("./keystore.p12", "quadra".toCharArray())) {
			String alias = "Test_" + System.currentTimeMillis();
			// génère la key pair !
			km.generateNewKeyPair(alias, "arcad".toCharArray());
			// Exporte la clé publique correspondante...
			km.exportPublicKey(alias, "./truststore.p12", "cheval".toCharArray());
			// récupère la clé privée...
			pri = km.getPrivateKey(alias, "arcad".toCharArray());
			// pour le fun ouvre le truststore...
			try (KeyStoreManager km2 = new KeyStoreManager("./truststore.p12", "cheval".toCharArray())) {
				// récupère la clé publique...
				pub = km2.getPublicKey(alias);
			}
		} catch (Exception e) {
			System.out.println(e.getLocalizedMessage());
			throw e;
		}
		System.out.println("Encryption/decryption...");
		byte[] bArray = new byte[(Integer.SIZE / Byte.SIZE) * 4];
		ByteBuffer bBuffer = ByteBuffer.wrap(bArray);
		bBuffer.asIntBuffer().put(12);
		bBuffer.getInt();
		bBuffer.asIntBuffer().put(666);
		bBuffer.getInt();
		bBuffer.asIntBuffer().put(-852);
		bBuffer.getInt();
		bBuffer.asIntBuffer().put(456);
		long t = System.nanoTime();
		String enc = Crypto.encryptA(bArray, pri);
		t = System.nanoTime() - t;
		System.out.println("encoding time: " + t + "ns.");
		System.out.println("Encoded: " + enc);
		System.out.println(" len=" + enc.length());
		t = System.nanoTime();
		byte[] bArray2 = Crypto.decryptAByte(enc, pub);
		t = System.nanoTime() - t;
		System.out.println("decoding time: " + t + "ns.");
		Assert.assertArrayEquals(bArray, bArray2);
	}

	@Test
	void testfog() {
		String a = "toto";
		String b = "";
		String c = RANDOMSTRING1 + RANDOMSTRING2 + RANDOMSTRING3 + RANDOMSTRING4 + RANDOMSTRING5 + RANDOMSTRING6 + RANDOMSTRING7 + RANDOMSTRING1 + "azertyuiopmljhgfdssqqwxcxcvvbbnnMLKHGFDSQAAZZERTYUIOPJFDDCVVGGF";;
		assertEquals(a, new String(Crypto.unFog(Crypto.fog(a.toCharArray()))));
		assertEquals(b, new String(Crypto.unFog(Crypto.fog(b.toCharArray()))));
		assertEquals(c, new String(Crypto.unFog(Crypto.fog(c.toCharArray()))));
	}
	
	@Test
	void testunfog() {
		assertFalse(Crypto.isNull(new char[] {'a'}));
		assertTrue(Crypto.isNull(null));
		assertTrue(Crypto.isNull(new char[] {}));
		assertTrue(Crypto.isNull(new char[] {0,0,0,0,0}));
		System.out.println("MK1 = " + Crypto.unFog("43140354351645273444F29715A64615786CA6475B2CF435D456461614B665F252F616264454E2F2160547D4F4E2F28315E655032345D324F225F2F2372735E4374594F25696141567A40763030343284C9A2C95777774B4D41735F436569685158776A7A596678646D47634461615E6675536"));
		System.out.println("MK2 = " + Crypto.unFog("578347F213C6A6E2362786D6F2F267C652284C275613E6F2D3734696B6C4E25796875B2C786CF2573703A65737269A2CF5A68613A7C63594D66574743727A675C654274687E6E4473516347567A586B4A5361454"));
		System.out.println("MK3 = " + Crypto.unFog("5627A6F2137313F503E283E213B646A6F2D667A6F22696C6F2273757F25B2C37D35786C657864736529A2C786C284C8757E696C475E6859434A6A5A756E654D6456717C675D67615A59445A564766555754437B4"));
		System.out.println("MK4 = " + Crypto.unFog("5627A6C55627A6C5275667275637C52313132386C53707F62746C5E6F696471676F6C6F6D6F686C5A3345B37D33514C4C41444D4D24425D20544529AF3F30313023777F646E6967564648586E407A5D664075444979794B4D684A7F4974635A4C495B4B44737A464"));
		System.out.println("MK4 = " + Crypto.unFog("5627A6F2137313F503E283E213B646A6F2D667A6F22696C6F2273757F25B2C37D35786C657864736529A2C786C284C8757E696C487E694548515378614B6958766C45625249654D6A487D45486777597F47414A4"));
		System.out.println("MK4(iso) = " + Crypto.unFog("5627A6C55627A6C5275667275637C52313132386C53707F62746C5E6F696471676F6C6F6D6F686C5A3345B37D33514C4C41444D4D24425D20544529AF3F30313023777F646E6967564648586E407A5D664075444979794B4D684A7F4974635A4C495B4B44737A464", StandardCharsets.ISO_8859_1));
	}
	
	@Test
	void testHex() {
		assertArrayEquals(Crypto.hexStringToByteArray("ab32"), new byte[] {(byte) 0xAB, 0x32});
		assertArrayEquals(Crypto.hexStringToByteArray("b32"), new byte[] {0xB, 0x32});
	}
	
	@Test
	void testNewFog() {
		String txt = "quadra";
		String encrypted = Crypto.fog(txt.toCharArray());
		System.out.println(encrypted);

		assertEquals(txt, new String(Crypto.unFog(encrypted)));
	}
	
	@Test
	void testOldUnFog() {
		String txt = "1627461657174717A517F6A46587646647B454D626F415A61646163726A474E65496449717F4";
		assertEquals("quadra", new String(Crypto.unFog(txt)));
	}
}
