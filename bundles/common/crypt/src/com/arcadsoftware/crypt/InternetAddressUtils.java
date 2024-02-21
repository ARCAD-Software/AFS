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
package com.arcadsoftware.crypt;

import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 * This class offer some common operation about Internet Addresses.
 * 
 * @author ARCAD Software
 */
public class InternetAddressUtils {

	/**
	 * Get the current host name, or null if unknown.
	 * 
	 * <p>
	 * This implementation still require a DNBS resolution in most cases, but also try to get a OS specific solution if there is no DNS.
	 * 
	 * @return null if the DNS name is not found.
	 */
	public static String getHostName() {
		String result = null;
		try {
			result = InetAddress.getLocalHost().getCanonicalHostName();
			if (result.equals(InetAddress.getLocalHost().getHostAddress()) || "localhost".equalsIgnoreCase(result)) {
				result = null;
			}
			if ((result == null) || result.isEmpty()) {
				result = InetAddress.getLocalHost().getHostName();
				if (result.equals(InetAddress.getLocalHost().getHostAddress()) || "localhost".equalsIgnoreCase(result)) {
					result = null;
				}
			}
		} catch (UnknownHostException e) {}
		if ((result == null) || result.isEmpty()) {
			result = System.getenv("COMPUTERNAME");
		}
		if ((result == null) || result.isEmpty() || "localhost".equalsIgnoreCase(result)) {
			result = System.getenv("HOSTNAME");
		}
		return result;
	}
	
	/**
	 * Get the current Host IP address.
	 * 
	 * <p>
	 * By default return the loopback IP Address.
	 * 
	 * @return never return null.
	 */
	public static String getIPAddress() {
		try {
			String result = InetAddress.getLocalHost().getHostAddress();
			if (result != null) {
				return result;
			}
		} catch (UnknownHostException e) {}
		String result = InetAddress.getLoopbackAddress().getHostAddress();
		if (result != null) {
			return result;
		}
		return "127.0.0.1"; //$NON-NLS-1$
	}
	
	/**
	 * Test if the given IP address is a valid IP address.
	 * 
	 * @param ip
	 * @return
	 */
	public static boolean isValidIPAddress(final String ip) {
        try {
            return isIPAddress(ip) && InetAddress.getByName(ip).getHostAddress().equals(ip);
        } catch (UnknownHostException ex) {
            return false;
        }
    }

	/**
	 * Test if the given string is a correct representation of an IP Address (support IPv4 and IPv6).
	 * 
	 * @param ip
	 * @return
	 */
	public static boolean isIPAddress(final String ip) {
		return isIPv4Address(ip) || isIPv6Address(ip);
	}

	/**
	 * Test if the given string is a correct representation of an IPv4 Address.
	 * 
	 * @param ip
	 * @return
	 */
	public static boolean isIPv4Address(String ip) {
		if (ip == null) {
			return false;
		}
		ip = ip.trim();
		if (ip.isEmpty() || (ip.length() <= 4)) {
			return false;
		}
		int dot1 = ip.indexOf('.');
		if (dot1 < 1) {
			return false;
		}
		int dot2 = ip.indexOf('.', dot1 + 1);
		if (dot2 <= dot1 + 1) {
			return false;
		}
		int dot3 = ip.indexOf('.', dot2 + 1);
		if (dot3 <= dot2 + 1) {
			return false;
		}
		int dot4 = ip.indexOf('.', dot3 + 1);
		if (dot4 >= 0) {
			return false;
		}
		try {
			int i = Integer.parseInt(ip.substring(0, dot1));
			if ((i < 0) || (i > 255)) {
				return false;
			}
			i = Integer.parseInt(ip.substring(dot1 + 1, dot2));
			if ((i < 0) || (i > 255)) {
				return false;
			}
			i = Integer.parseInt(ip.substring(dot2 + 1, dot3));
			if ((i < 0) || (i > 255)) {
				return false;
			}
			i = Integer.parseInt(ip.substring(dot3 + 1));
			if ((i < 0) || (i > 255)) {
				return false;
			}
		} catch (NumberFormatException e) {
			return false;
		}
		return true;
	}

	/**
	 * Test if the given string is a correct representation of an IPv6 Address.
	 * 
	 * @param ip
	 * @return
	 */
	public static boolean isIPv6Address(final String ip) {
		int ddn = 0;
		int p = 0;
		for (int i = ip.indexOf(':'); i >= 0; i = ip.indexOf(':', p)) {
			if (i > p) {
				String h = ip.substring(p, i);
				if (!Crypto.isHexString(h)) {
					return false;
				}
				if (Crypto.hexStringToByteArray(h).length > 2) {
					return false;
				}
			}
			p = i + 1;
			if (p == ip.length()) {
				return false;
			}
		}
		if ((ddn == 0) || (ddn > 7)) {
			return false;
		}
		if (p < ip.length()) {
			String h = ip.substring(p);
			if (!Crypto.isHexString(h)) {
				return isIPv4Address(h);
			}
			if (Crypto.hexStringToByteArray(h).length > 2) {
				return false;
			}
		}
		return true;
	}
}
