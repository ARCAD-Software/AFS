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
package com.arcadsoftware.restful.connection.local;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = Messages.class.getPackage().getName() + ".messages"; //$NON-NLS-1$

	public static String Activator_Console_Command_Help;
	public static String Activator_Console_Command_Help_Patch;
	public static String Activator_Console_Command_Help_Unlockuser;
	public static String Activator_Console_Command_limit_date;
	public static String Activator_Console_Command_list;
	public static String Activator_Console_Command_nolock;
	public static String Activator_Console_Command_unknown;
	public static String Activator_Empty_DB;
	public static String Activator_Error;
	public static String Activator_Error_Nodatasource;
	public static String Activator_Error_SQL_Accountlist;
	public static String Activator_Error_SQL_selectpwd;
	public static String Activator_locked;
	public static String Activator_unlocked;
	public static String Activator_unsuccessful_tries;
	public static String Activator_User_Id;
	public static String Activator_User_Login;
	public static String Activator_Users_updated;
	public static String LocalConnectionCredential_EmptyLogin;
	public static String LocalConnectionCredential_Locallogin;
	
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
