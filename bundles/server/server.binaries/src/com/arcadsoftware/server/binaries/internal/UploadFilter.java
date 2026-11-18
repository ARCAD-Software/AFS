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
package com.arcadsoftware.server.binaries.internal;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Decides, from its name, whether a file may be uploaded on the server.
 *
 * <p>
 * The filter is built from two optional lists, a <i>white list</i> and a <i>black
 * list</i>:
 * <ul>
 * <li>When the white list is not empty, only the files matching one of its
 * entries are accepted.
 * <li>A file matching one of the black list entries is always refused, even
 * if it is also in the white list.
 * <li>When both lists are empty, every file is accepted.
 * </ul>
 *
 * <p>
 * Each list is a sequence of entries separated by commas or semicolons. An
 * entry is either a file extension (<code>pdf</code> or <code>.pdf</code>) or
 * a file name pattern using <code>*</code> (any characters) and <code>?</code>
 * (one character) as wildcards, like <code>*.tar.gz</code> or
 * <code>report_*.csv</code>. The comparison is not case sensitive.
 *
 * <p>
 * Instances are immutable.
 */
final class UploadFilter {

	/**
	 * A filter that accepts everything.
	 */
	static final UploadFilter ACCEPT_ALL = new UploadFilter(List.of(), List.of());

	private static final String SEPARATORS = "[,;]"; //$NON-NLS-1$

	/**
	 * Build a filter from the raw value of the configuration parameters.
	 *
	 * @param whiteList the allowed files, <code>null</code> or empty to allow any file.
	 * @param blackList the refused files, <code>null</code> or empty to refuse none.
	 * @return the filter.
	 */
	static UploadFilter parse(String whiteList, String blackList) {
		final List<Pattern> white = compile(whiteList);
		final List<Pattern> black = compile(blackList);
		if (white.isEmpty() && black.isEmpty()) {
			return ACCEPT_ALL;
		}
		return new UploadFilter(white, black);
	}

	private static List<Pattern> compile(String list) {
		if (list == null) {
			return List.of();
		}
		final List<Pattern> result = new ArrayList<>();
		for (String entry : list.split(SEPARATORS)) {
			String glob = entry.trim();
			if (glob.indexOf('*') < 0 && glob.indexOf('?') < 0) {
				// A plain entry is a file extension, the leading dot is optional.
				glob = glob.startsWith(".") ? glob.substring(1) : glob; //$NON-NLS-1$
				if (glob.isEmpty()) {
					continue;
				}
				glob = "*." + glob; //$NON-NLS-1$
			}
			result.add(toPattern(glob));
		}
		return List.copyOf(result);
	}

	private static Pattern toPattern(String glob) {
		final StringBuilder regex = new StringBuilder();
		final StringBuilder literal = new StringBuilder();
		for (int i = 0; i < glob.length(); i++) {
			final char c = glob.charAt(i);
			if (c == '*' || c == '?') {
				if (literal.length() > 0) {
					regex.append(Pattern.quote(literal.toString()));
					literal.setLength(0);
				}
				regex.append(c == '*' ? ".*" : "."); //$NON-NLS-1$ //$NON-NLS-2$
			} else {
				literal.append(c);
			}
		}
		if (literal.length() > 0) {
			regex.append(Pattern.quote(literal.toString()));
		}
		return Pattern.compile(regex.toString(), Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE | Pattern.DOTALL);
	}

	private final List<Pattern> whiteList;
	private final List<Pattern> blackList;

	private UploadFilter(List<Pattern> whiteList, List<Pattern> blackList) {
		this.whiteList = whiteList;
		this.blackList = blackList;
	}

	/**
	 * Test if a file can be uploaded.
	 *
	 * @param filename the name of the file, without any path. A <code>null</code> name is
	 *                 handled as an empty one.
	 * @return false if the file is refused.
	 */
	boolean isAllowed(String filename) {
		final String name = normalize(filename);
		if (!whiteList.isEmpty() && !matches(whiteList, name)) {
			return false;
		}
		return !matches(blackList, name);
	}

	private static boolean matches(List<Pattern> patterns, String name) {
		for (Pattern pattern : patterns) {
			if (pattern.matcher(name).matches()) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Remove the trailing dots and spaces: Windows file systems silently drop them,
	 * so "script.exe. " is stored as "script.exe" and must be tested as such.
	 */
	private static String normalize(String filename) {
		if (filename == null) {
			return ""; //$NON-NLS-1$
		}
		int end = filename.length();
		while (end > 0 && (filename.charAt(end - 1) == '.' || filename.charAt(end - 1) == ' ')) {
			end--;
		}
		return filename.substring(0, end);
	}

}
