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
package com.arcadsoftware.osgi.internal;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.util.Enumeration;

public class MacAddress6 {

	public String getMacAddress() {
		try {
			InetAddress[] addresses = InetAddress.getAllByName(InetAddress.getLocalHost().getHostName());
			if (addresses == null) {
				return null;
			}
			for (InetAddress address : addresses) {
				// Get NetworkInterface for the current host and then read the hardware address.
				NetworkInterface ni = NetworkInterface.getByInetAddress(address);
				if (ni != null) {
					byte[] mac = ni.getHardwareAddress();
					if (isValidMac(mac)) {
						return macToString(mac);
					}
				}
			}
			InetAddress ip = InetAddress.getLocalHost();
			if (ip != null) {
				NetworkInterface network = NetworkInterface.getByInetAddress(ip);
				if (network != null) {
					byte[] mac = network.getHardwareAddress();
					if (isValidMac(mac)) {
						return macToString(mac);
					}
				}
			}
			Enumeration<NetworkInterface> networks = NetworkInterface.getNetworkInterfaces();
			if (networks != null) {
				while (networks.hasMoreElements()) {
					NetworkInterface network = networks.nextElement();
					byte[] mac = network.getHardwareAddress();
					if (isValidMac(mac)) {
						return macToString(mac);
					}
				}
			}
		} catch (Exception e) {
			if (Activator.getInstance() != null) {
				Activator.getInstance().error(e);
			}
		}
		return null;
	}

	private boolean isValidMac(byte[] mac) {
		return (mac != null) && (mac.length > 4) && //
				((mac[0] != 0) || (mac[1] != 0) || (mac[2] != 0) || (mac[3] != 0));
	}

	private String macToString(byte[] mac) {
		// Extract each array of mac address and convert it to hexa
		// with the following format 080027DC4A9E.
		StringBuilder sb = new StringBuilder();
		for (byte element : mac) {
			sb.append(String.format("%02X", element)); //$NON-NLS-1$
		}
		return sb.toString();
	}
}
