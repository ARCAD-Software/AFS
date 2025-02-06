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
import java.nio.ByteBuffer;

import org.junit.Assert;
import org.junit.jupiter.api.Test;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.crypt.KeyStoreManager;

class TestKeyStoreManager {

	@Test
	void test() throws Exception {
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
	
}
