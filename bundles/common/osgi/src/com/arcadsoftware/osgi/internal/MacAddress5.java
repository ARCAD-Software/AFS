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
package com.arcadsoftware.osgi.internal;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.text.ParseException;
import java.util.StringTokenizer;

public class MacAddress5 {

	public static final String NOMACADDRESS = "NOMACADDRESS"; //$NON-NLS-1$

	/**
	 * try to determine MAC address of local network card; this is done using a
	 * shell to run ifconfig (linux) or ipconfig (windows). The output of the
	 * processes will be parsed.
	 * 
	 * <p>
	 * To run the whole thing, just type java NetworkInfo
	 * 
	 * <p>
	 * Current restrictions:
	 * 
	 * <ul>
	 * <li>Will probably not run in applets
	 * 
	 * <li>Tested Windows / Linux only
	 * 
	 * <li>Tested J2SDK 1.4 only
	 * 
	 * <li>If a computer has more than one network adapters, only one MAC
	 * address will be returned
	 * 
	 * <li>will not run if user does not have permissions to run
	 * ifconfig/ipconfig (e.g. under linux this is typically only permitted for
	 * root)
	 * </ul>
	 */
	public final String getMacAddress() {
		String os = System.getProperty("os.name").toLowerCase(); //$NON-NLS-1$
		try {
			if (os.startsWith("windows")) { //$NON-NLS-1$
				return windowsParseMacAddress(windowsRunIpConfigCommand());
			} else if (os.startsWith("linux")) { //$NON-NLS-1$
				return linuxParseMacAddress(linuxRunIfConfigCommand());
			} else {
                return NOMACADDRESS;
			}
		} catch (Exception ex) {
            return NOMACADDRESS;
		}
	}

	/*
	 * Linux stuff
	 */
	private final String linuxParseMacAddress(String ipConfigResponse) {
		String localHost = null;
		try {
			localHost = InetAddress.getLocalHost().getHostAddress();
		} catch (UnknownHostException ex) {
            return NOMACADDRESS;
		}
		StringTokenizer tokenizer = new StringTokenizer(ipConfigResponse, "\n"); //$NON-NLS-1$
		String lastMacAddress = null;
		while (tokenizer.hasMoreTokens()) {
			String line = tokenizer.nextToken().trim();
			boolean containsLocalHost = line.indexOf(localHost) >= 0;
			// see if line contains IP address
			if (containsLocalHost && (lastMacAddress != null)) {
				return lastMacAddress;
			}
			// see if line contains MAC address
			int macAddressPosition = line.indexOf("HWaddr"); //$NON-NLS-1$
			if (macAddressPosition <= 0) {
				continue;
			}
			String macAddressCandidate = line.substring(macAddressPosition + 6).trim();
			if (linuxIsMacAddress(macAddressCandidate)) {
				lastMacAddress = macAddressCandidate;
				continue;
			}
		}
		if (lastMacAddress != null) {
			// did not found the localhost IP ethernet address but get one !!!
			return lastMacAddress;
		}
        return NOMACADDRESS;
	}

	private final boolean linuxIsMacAddress(String macAddressCandidate) {
		// TODO: use a smart regular expression
		return macAddressCandidate.length() == 17;
	}

	private final String linuxRunIfConfigCommand() throws IOException {
		Process p = Runtime.getRuntime().exec("ifconfig"); //$NON-NLS-1$
		InputStream stdoutStream = new BufferedInputStream(p.getInputStream());
		StringBuilder buffer = new StringBuilder();
		for(;;) {
			int c = stdoutStream.read();
			if (c == -1) {
				break;
			}
			buffer.append((char) c);
			if (buffer.length() > 100000) {
				break;
			}
		}
		String outputText = buffer.toString();
		stdoutStream.close();
		return outputText;
	}

	/*
	 * Windows stuff
	 */
	private final String windowsParseMacAddress(String ipConfigResponse) throws ParseException {
		String localHost = null;
		try {
			localHost = InetAddress.getLocalHost().getHostAddress();
		} catch (java.net.UnknownHostException ex) {
			throw new ParseException(ex.getMessage(), 0);
		}
		StringTokenizer tokenizer = new StringTokenizer(ipConfigResponse, "\n"); //$NON-NLS-1$
		String lastMacAddress = null;
		while (tokenizer.hasMoreTokens()) {
			String line = tokenizer.nextToken().trim();
			// see if line contains IP address
			if (line.endsWith(localHost) && (lastMacAddress != null)) {
				return lastMacAddress;
			}
			// see if line contains MAC address
			int macAddressPosition = line.indexOf(":"); //$NON-NLS-1$
			if (macAddressPosition <= 0) {
				continue;
			}
			String macAddressCandidate = line.substring(macAddressPosition + 1).trim();
			if (windowsIsMacAddress(macAddressCandidate)) {
				lastMacAddress = macAddressCandidate;
				continue;
			}
		}
		throw new ParseException("cannot read MAC address from [" + ipConfigResponse + "]", 0);  //$NON-NLS-1$//$NON-NLS-2$
	}

	private final boolean windowsIsMacAddress(String macAddressCandidate) {
		// TODO: use a smart regular expression
		if (macAddressCandidate.length() != 17) {
			return false;
		}
		return true;
	}

	private final String windowsRunIpConfigCommand() throws IOException {
		Process p = Runtime.getRuntime().exec("ipconfig /all"); //$NON-NLS-1$
		InputStream stdoutStream = new BufferedInputStream(p.getInputStream());
		StringBuilder buffer = new StringBuilder();
		for (;;) {
			int c = stdoutStream.read();
			if (c == -1) {
				break;
			}
			buffer.append((char) c);
		}
		String outputText = buffer.toString();
		stdoutStream.close();
		return outputText;
	}

}