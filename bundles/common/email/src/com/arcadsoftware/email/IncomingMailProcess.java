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
package com.arcadsoftware.email;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Manage a incoming emails check. This process can be run periodically.
 * 
 * @see IncomingEmailProcessTask
 */
public abstract class IncomingMailProcess extends IncomingEmailProcessTask {

	public static final String EXCHANGELISTED = "-"; //$NON-NLS-1$

	private HashMap<String,String> aliasAddresses = new HashMap<String,String>();
	private final ArrayList<String> addresses = new ArrayList<String>();
	
	public IncomingMailProcess(int timing, String server, int port, String type, String folderName, String login, String password) {
		super(timing, server, port, type, folderName, login, password);
	}
	
	public void process() {
		super.run();
	}
	
	/**
	 * Check if none of the email addresses listed are member of the <i>Check addresses list</i>.
	 * 
	 * @param listAddresses an addresses list to check
	 * @return true if none of the email addresses is member of the <i>Check addresses list</i>
	 * @see #setCheckAddresses(Object)
	 */
	protected boolean isNotMemberOf(String listAddresses) {
		if ((listAddresses == null) || (listAddresses.length() == 0)) {
			return true;
		}
		String[] la = listAddresses.split(","); //$NON-NLS-1$
		if ((la == null) || (la.length == 0)) {
			return true;
		}
		ArrayList<String> list = new ArrayList<String>(la.length);
		for(String s: la) {
			list.add(s.trim().toLowerCase());
		}
		for(String add: addresses) {
			// fullmatch
			int i = list.indexOf(add);
			if (i == -1) {
				//partial result
				String addsup = '<' + add;
				for(int j = list.size()-1; j >= 0; j--) {
					String el = list.get(j);
					if (el.startsWith(add) || (el.indexOf(addsup) > -1)) {
						i = j;
						break;
					}
				}
			}
			if (i > -1) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Check if none of the email addresses listed are member of the <i>Check addresses list</i>.
	 * 
	 * @param listAddresses an addresses list to check
	 * @return true if at least one email addresses is not member of the <i>Check addresses list</i>
	 * @see #setCheckAddresses(Object)
	 */
	protected boolean isNotAllMemberOf(String listAddresses) {
		if ((listAddresses == null) || (listAddresses.length() == 0)) {
			return true;
		}
		String[] la = listAddresses.split(","); //$NON-NLS-1$
		if ((la == null) || (la.length == 0)) {
			return true;
		}
		ArrayList<String> list = new ArrayList<String>(la.length);
		for(String s:la) {
			list.add(s.trim().toLowerCase());
		}
		// (Pas optimisé) A refaire dans l'autre sens !!!
		for(String add: addresses) {
			// fullmatch
			int i = list.indexOf(add);
			if (i == -1) {
				//partial result
				String addsup = '<' + add;
				for(int j = list.size()-1; j >= 0; j--) {
					String el = list.get(j);
					if (el.startsWith(add) || (el.indexOf(addsup) > -1)) {
						i = j;
						break;
					}
				}
			}
			if (i > -1) {
				list.remove(i);
			}
		}
		return list.size() > 0;
	}

	protected String alias(String address, String to) {
		String[] ads = address.split(","); //$NON-NLS-1$
		if ((ads != null) && (ads.length > 0)) {
			address = ads[0];
		}
		String result = aliasAddresses.get(address.trim().toLowerCase());
		if (result == null) {
			return address;
		}
		if (result.length() == 0) {
			return null;
		}
		if (result.equals(EXCHANGELISTED)) {
			try {
				return to.split(",")[0].trim().toLowerCase(); //$NON-NLS-1$
			} catch (Throwable e) {}
		}
		return result;
	}

	/**
	 * Define the Aliases addresses map. 
	 * 
	 * <p>
	 * This can be used to map given addresses with known addresses (nickname or white lists...)
	 * 
	 * @param aliasAddresses
	 * @see #alias(String, String)
	 */
	public void setAliasAddresses(HashMap<String, String> aliasAddresses) {
		this.aliasAddresses = aliasAddresses;
	}

	/**
	 * Theses addresses can be checked as the address corresponding to the mail box address.
	 * 
	 * 
	 * @param object the addresses list
	 * @see #isNotMemberOf(String, boolean)
	 */
	public void setCheckAddresses(Object add) {
		addresses.clear();
		if (add != null) {
			String[] addr = add.toString().split(","); //$NON-NLS-1$
			for (int i = 0; i < addr.length; i++) {
				if ((addr[i] != null) && (addr[i].length() > 0)) {
					addresses.add(addr[i].trim().toLowerCase());
				}
			}
		}
	}

	public HashMap<String, String> getAliasAddresses() {
		return aliasAddresses;
	}

	protected ArrayList<String> getCheckAddresses() {
		return addresses;
	}
	
}