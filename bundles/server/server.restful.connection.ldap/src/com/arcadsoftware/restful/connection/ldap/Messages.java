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
package com.arcadsoftware.restful.connection.ldap;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = Messages.class.getPackage().getName() + ".messages"; //$NON-NLS-1$
	public static String Activator_StartingService_Error;
	public static String LdapAuthentificationResource_Empty_Login;
	public static String LdapAuthentificationService_Authentification_Denied;
	public static String LdapAuthentificationService_Binding_Error;
	public static String LdapAuthentificationService_ChangePWD_Error;
	public static String LdapAuthentificationService_Compare_Error;
	public static String LdapAuthentificationService_Connection_Error;
	public static String LdapAuthentificationService_Dateparsing_Error;
	public static String LdapAuthentificationService_Disconnection_Error;
	public static String LdapAuthentificationService_LoginListing_Error;
	public static String LdapAuthentificationService_NoConnection;
	public static String LdapAuthentificationService_Read_Error;
	public static String LdapAuthentificationService_TimeMap_Error;
	public static String LdapConnectionCredential_LDAPLogin;
	public static String LDAPExportResource_ExportError;
	public static String LDAPExportResource_NotAValidID;
	public static String LDAPExportResource_NotConfigured;
	public static String LDAPExportResource_UserIdNotInteger;
	public static String LDAPImportResource_ERROR_UserIdError;
	public static String LDAPImportResource_ERROR_UserLoginError;
	public static String LDAPExportSectionNode_LDAPError;
	public static String LDAPExportSectionNode_UserNotFound;
	public static String LDAPExportSectionNode_UnknownLoginRow;
	public static String LDAPImportResource_ERROR_UserSelectionEmpty;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
