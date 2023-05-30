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
package com.arcadsoftware.afs.client.users;

public interface IUsersConsts {

	/**
	 * The Entity type of the User objects.
	 */
	public static final String ENTITY_USER = "user"; //$NON-NLS-1$
	public static final String ENTITY_CLIENT = "client"; //$NON-NLS-1$
	public static final String ENTITY_PROFILE = "profile"; //$NON-NLS-1$
	public static final String ENTITY_LDAP = "ldapauth"; //$NON-NLS-1$
	
	public static final String PROP_USER_FIRSTNAME = "firstname"; //$NON-NLS-1$
	public static final String PROP_USER_LASTNAME = "lastname"; //$NON-NLS-1$
	public static final String PROP_USER_EMAIL = "email"; //$NON-NLS-1$
	
	public static final String LDAPIMPORT_LOGIN = "ldap.login"; //$NON-NLS-1$
	
	public static final String LDAP_IMPORT_SERVICE_GET = "/admin/ldap/import"; //$NON-NLS-1$
	
	public static final String LDAP_IMPORT_SERVICE_POST = "/admin/ldap/import/"; //$NON-NLS-1$

	public static final String IBMI_IMPORT_LOGIN = "ibmiauth.login";
	public static final String USER_LINK_PROFILES = "profiles";

	
}
