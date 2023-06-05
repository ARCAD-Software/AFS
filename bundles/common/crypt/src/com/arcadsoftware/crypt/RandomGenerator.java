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

import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.Random;

/**
 * Utility class to server random numbers and Strings.
 * 
 * @author ARCAD Software
 */
public class RandomGenerator {

	private static final String ALPHALOW = "abcdefghijklmnopqrstuvwxyz"; //$NON-NLS-1$
	private static final String ALPHAHIGH = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"; //$NON-NLS-1$
	private static final String ALPHA = ALPHALOW + ALPHAHIGH;
	private static final char[] VALUES = { 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
			'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd',
			'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
			'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' };
	private static final int PWD_LEN = 12;
	private static final String NONALPHA = " !\"#$%£%&'()*+,-./:;<=>?@[\\]^_{|}~"; //$NON-NLS-1$
	private static final String ALLCHARS = ALPHA + NONALPHA + "0123456789";
	private static final SecureRandom rn;
	
	static {
		SecureRandom r;
		try {
			r = SecureRandom.getInstanceStrong();
		} catch (NoSuchAlgorithmException e) {
			r = null;
		}
		rn = r;
	}

	/**
	 * Generate a very simple password from digits and alphabetic letters.
	 * 
	 * @param rnd A random number generator.
	 * @param length the returned string length.
	 * @return
	 */
	public static char[] simpleRandonPassword(Random rnd, int length) {
		if ((length <= 0) || (rnd == null)) {
			return new char[0];
		}
		final char[] result = new char[length];
		for (int i = 0; i < length; i++) {
			result[i] = VALUES[rnd.nextInt(VALUES.length)];
		}
		return result;
	}

	/**
	 * Generate a complex password with a random size between 12 and 30 and using a set of aplha, digit and non-alpha characters.
	 * @return
	 */
	public static char[] complexRandonPassword() {
		final char[] result = new char[rn.nextInt(18) + 12];
		for (int i = 0; i < result.length; i++) {
			result[i] = ALLCHARS.charAt(rn.nextInt(ALLCHARS.length()));
		}
		return result;
	}

	/**
	 * Generate a very simple password from digits and alphabetic letters.
	 * 
	 * @param length
	 * @return
	 */
	public static char[] simpleRandonPassword(int length) {
		return simpleRandonPassword(rn, length);
	}

	/**
	 * Generate a very simple password from digits and alphabetic letters.
	 * @return
	 */
	public static char[] simpleRandonPassword() {
		return simpleRandonPassword(rn, PWD_LEN);
	}

	/**
	 * Return a random number comprised into the given range.
	 * 
	 * @param lo
	 * @param hi
	 * @return
	 */
	public static int randInteger(int lo, int hi) {
		final int n = (hi - lo) + 1;
		final int i = rn.nextInt() % n;
		if (i < 0) {
			return lo - i;
		}
		return lo + i;
	}

	/**
	 * Generate a random number ranging from zero to n - 1.
	 * 
	 * @param n
	 * @return
	 */
	public static int randInteger(int n) {
		final int i = rn.nextInt() % n;
		if (i < 0) {
			return -i;
		}
		return i;
	}

	/**
	 * Generate a random string using Alphnumeric and non alphanumeric, printable characters.
	 * 
	 * @param length
	 * @return
	 */
	public static String randomStringSecure(int length) {
		SecureRandom sr = null;
		try {
			sr = SecureRandom.getInstance("SHA1PRNG"); //$NON-NLS-1$
		} catch (NoSuchAlgorithmException e) {
			try {
				sr = SecureRandom.getInstanceStrong();
			} catch (NoSuchAlgorithmException e1) {}
		}
		if (sr == null) {
			return randomString(length);
		}
		final StringBuilder sb = new StringBuilder(length);
		for (int i = 0; i < length; i++) {
			sb.append(ALLCHARS.charAt(sr.nextInt(ALLCHARS.length())));
		}
		return sb.toString();
	}

	/**
	 * Generate a random string of letter.
	 * @param length
	 * @return
	 */
	public static String randomString(int length) {
		final StringBuilder sb = new StringBuilder(length);
		for (int i = 0; i < length; i++) {
			sb.append(ALPHA.charAt(randInteger(0, ALPHA.length() - 1)));
		}
		return sb.toString();
	}

	/**
	 * Fill an char array from random alphabetic characters
	 * 
	 * @param buffer
	 * @param offset
	 * @param length
	 */
	public static void randomChars(char[] buffer, int offset, int length) {
		for (int i = offset; (i < offset + length) && (i < buffer.length); i++) {
			buffer[i] = ALPHA.charAt(randInteger(0, ALPHA.length() - 1));
		}
	}

	/**
	 * Fill an char array from random alphabetic upper case characters
	 * 
	 * @param buffer
	 * @param offset
	 * @param length
	 */
	public static void randomUpperCaseChars(char[] buffer, int offset, int length) {
		for (int i = offset; (i < offset + length) && (i < buffer.length); i++) {
			buffer[i] = ALPHAHIGH.charAt(randInteger(0, ALPHAHIGH.length() - 1));
		}
	}

	/**
	 * Fill an char array from random alphabetic lower case characters
	 * 
	 * @param buffer
	 * @param offset
	 * @param length
	 */
	public static void randomLowerCaseChars(char[] buffer, int offset, int length) {
		for (int i = offset; (i < offset + length) && (i < buffer.length); i++) {
			buffer[i] = ALPHALOW.charAt(randInteger(0, ALPHALOW.length() - 1));
		}
	}
	
	/**
	 * Generate a random string of digits.
	 * @param length
	 * @return
	 */
	public static String randomStringDigit(int length) {
		final StringBuilder sb = new StringBuilder(length);
		for (int i = 0; i < length; i++) {
			sb.append((char) randInteger('0', '9'));
		}
		return sb.toString();
	}

	/**
	 * Fill an char array from digit characters. 
	 * 
	 * @param buffer
	 * @param offset
	 * @param length
	 */
	public static void randomCharsDigit(char[] buffer, int offset, int length) {
		for (int i = offset; (i < offset + length) && (i < buffer.length); i++) {
			buffer[i] = (char) randInteger('0', '9');
		}
	}

	/**
	 * Generate a random string composed of non alphabetic characters.
	 * 
	 * @param length
	 * @return
	 */
	public static String randomStringNonAlpha(int length) {
		final StringBuilder sb = new StringBuilder(length);
		for (int i = 0; i < length; i++) {
			sb.append(NONALPHA.charAt(randInteger(0, NONALPHA.length() - 1)));
		}
		return sb.toString();
	}

	/**
	 * Fill an char array from non alphabetic characters.
	 * 
	 * @param buffer
	 * @param offset
	 * @param length
	 */
	public static void randomCharsNonAlpha(char[] buffer, int offset, int length) {
		for (int i = offset; (i < offset + length) && (i < buffer.length); i++) {
			buffer[i] = NONALPHA.charAt(randInteger(0, NONALPHA.length() - 1));
		}
	}

	/**
	 * Generate a array or random integers from zero to size-1.
	 * 
	 * @param size The array size.
	 * @param seed The random generator seed. 
	 * @return
	 */
	public static int[] randomSet(int size, long seed) {
		if (size < 0) {
			return new int[0];
		}
		final Random rnd = new Random(seed);
		final int[] result = new int[size];
		for (int i = 1; i < size; i++) {
			final int j = rnd.nextInt(i);
			result[i] = result[j];
			result[j] = i;
		}
		return result;
	}

}
