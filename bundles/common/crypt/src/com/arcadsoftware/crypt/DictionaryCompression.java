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

import java.util.Arrays;
import java.util.Comparator;

/**
 * Very simple compression utility based on small but specific dictionaries.
 *
 * @author ARCAD Software
 */
public class DictionaryCompression {

	private String[] dictionary;
	private int size;
	private boolean caseSensitive = true;

	/**
	 * Create a new Compression object that must be initialized.
	 */
	public DictionaryCompression() {
		super();
	}

	/**
	 * Create a new Compression object with the given dictionary.
	 * <p>
	 * This dictionary must content non empty strings and have a length between 1 and 160 include.
	 * <p>
	 * The dictionary must be sorted as the first matching token is taken into account. Usually the most longer token
	 * must be in the first places. If the given dictionary is not sorted, set false to the <code>sorted</code>
	 * parameter and this method will sort the dictionary.
	 *
	 * @param dictionary
	 *            A String dictionary, that content statically most current portion of text.
	 * @param sorted
	 *            True if this dictionary is already sorted.
	 */
	public DictionaryCompression(String[] dictionary, boolean sorted) {
		super();
		setDictionary(dictionary, sorted);
	}

	/**
	 * Define the dictionary used to compress the strings.
	 * <p>
	 * This dictionary must content non empty strings and have a length between 1 and 160 include.
	 * <p>
	 * The dictionary must be sorted as the first matching token is taken into account. Usually the most longer token
	 * must be in the first places. If the given dictionary is not sorted, set false to the <code>sorted</code>
	 * parameter and this method will sort the dictionary.
	 *
	 * @param dictionary
	 *            A String dictionary, that content statically most current portion of text.
	 * @param sorted
	 *            True if this dictionary is already sorted.
	 */
	public void setDictionary(String[] dictionary, boolean sorted) {
		size = 0;
		if (dictionary != null) {
			size = dictionary.length;
		}
		if ((size < 1) || (size > 160)) {
			throw new IllegalArgumentException("Dictionary size is incorrect (1 < Size < 160) Size=" + size);
		}
		this.dictionary = dictionary;
		if (!sorted) {
			Arrays.sort(this.dictionary, new Comparator<String>() {
				@Override
				public int compare(String o1, String o2) {
					return o1.length() - o2.length();
				}
			});
		}
	}

	/**
	 * Compress a string according to the given dictionary.
	 * <p>
	 * Token into the dictionary can content any character, but the character that will not correspond to ASCII
	 * character will not be acceptable.
	 *
	 * @param string
	 *            The String to compress.
	 * @return The compressed data.
	 * @throws IllegalArgumentException
	 */
	public byte[] compress(String string) {
		if (dictionary == null) {
			throw new IllegalArgumentException("Dictionary is undefined.");
		}
		if (string == null) {
			return new byte[0];
		}
		int i = 0;
		final int l = string.length();
		int bl = 0;
		final byte[] buffer = new byte[l + 1];
		while (i < l) {
			boolean notfound = true;
			if (caseSensitive) {
				for (int j = 0; j < size; j++) {
					final int tl = dictionary[j].length();
					if (((i + tl) < l) && dictionary[j].equals(string.substring(i, i + tl))) {
						notfound = false;
						buffer[bl++] = (byte) j;
						i += tl;
						break;
					}
				}
			} else {
				for (int j = 0; j < size; j++) {
					final int tl = dictionary[j].length();
					if (((i + tl) < l) && dictionary[j].equalsIgnoreCase(string.substring(i, i + tl))) {
						notfound = false;
						buffer[bl++] = (byte) j;
						i += tl;
						break;
					}
				}
			}
			if (notfound) {
				final char c = string.charAt(i++);
				if ((c < 32) || (c > 127)) {
					throw new IllegalArgumentException("The compressed string content illegal characters: " + c);
				}
				buffer[bl++] = (byte) (128 + c);
			}
		}
		return Arrays.copyOf(buffer, bl);
	}

	public String decompress(byte[] data) {
		final StringBuilder sb = new StringBuilder(data.length);
		for (final byte b : data) {
			int i;
			if (b < 0) {
				i = 256 + b;
			} else {
				i = b;
			}
			if (i < 160) {
				sb.append(dictionary[i]);
			} else {
				sb.append((char) (i - 128));
			}
		}
		return sb.toString();
	}

	/**
	 * Determine if the token stored into the dictionary are case sensitive.
	 *
	 * @param caseSensitive
	 */
	public void setCaseSensitive(boolean caseSensitive) {
		this.caseSensitive = caseSensitive;
	}

	/**
	 * True if the token stored into the dictionary are case sensitive.
	 * @return
	 */
	public boolean isCaseSensitive() {
		return caseSensitive;
	}

	/**
	 * Get the Dictionary of token.
	 * @return
	 */
	public String[] getDictionary() {
		return dictionary;
	}
}
