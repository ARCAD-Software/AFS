/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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
package com.arcadsoftware.afs.client.core.tools;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;

public class ExecUtils {
	public static String executeCommandtoString(ArrayList<String> commandOptions, boolean verbose)
			throws IOException {
		verbose = true;
		final ProcessBuilder processBuilder = new ProcessBuilder(commandOptions);
		processBuilder.redirectErrorStream(true);
		final Process process = processBuilder.start();
		final InputStream is = process.getInputStream();
		final InputStreamReader isr = new InputStreamReader(is);
		final char[] c = new char[8096];
		int count = isr.read(c, 0, 8096);
		final StringBuilder line = new StringBuilder();
		while (count != -1) {
			line.append(String.valueOf(c, 0, count));
			count = isr.read(c, 0, 8096);
		}
		if (verbose) {
			final StringBuilder cmd = new StringBuilder();
			for (final String s : commandOptions) {
				cmd.append(s).append(" "); //$NON-NLS-1$
			}
			cmd.append("\n");//$NON-NLS-1$
			cmd.append(line);
			return cmd.toString();
		} else {
			return line.toString();
		}

	}
}
