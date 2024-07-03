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
package com.arcadsoftware.afs.client.users;

public enum AuthType {

	CONFIG(0, "configauth", "configauth", false, false),
	IBMI(3, "ibmiauth", "ibmiauth", true, true),
	LDAP(2, "ldapauth", "ldap", true, true),
	LOCAL(1, "localauth", "localauth", true, false);

	public static int compare(final AuthType authType1, final AuthType authType2) {
		return Integer.compare(authType1.position, authType2.position);
	}

	public static AuthType fromCode(final String code) {
		for (final AuthType value : AuthType.values()) {
			if (value.code().equals(code)) {
				return value;
			}
		}
		return null;
	}

	private final String code;
	private boolean createAllowed;
	private boolean importAllowed;
	private final String loginResourceSuffix;
	private final int position;
	private final String resourceAddress;
	private final String resourceSuffix;

	private AuthType(final int position, final String code, final String resourceSuffix, final boolean createAllowed,
			final boolean importAllowed) {
		this.position = position;
		this.code = code;
		this.resourceSuffix = resourceSuffix;
		this.createAllowed = createAllowed;
		this.importAllowed = importAllowed;
		loginResourceSuffix = resourceSuffix + ".login"; //$NON-NLS-1$
		resourceAddress = "/data/" + code; //$NON-NLS-1$
	}

	public String code() {
		return code;
	}

	public boolean createAllowed() {
		return createAllowed;
	}

	public boolean importAllowed() {
		return importAllowed;
	}

	public String loginResourceSuffix() {
		return loginResourceSuffix;
	}

	public String resourceAddress() {
		return resourceAddress;
	}

	public String resourceSuffix() {
		return resourceSuffix;
	}
}