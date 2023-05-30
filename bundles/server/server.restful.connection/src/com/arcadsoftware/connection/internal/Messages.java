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
package com.arcadsoftware.connection.internal;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "com.arcadsoftware.connection.internal.messages"; //$NON-NLS-1$

	public static String Activator_Console_Command_Cache_cleared;
	public static String Activator_Console_Command_Cache_Containt1;
	public static String Activator_Console_Command_Cache_Containt2;
	public static String Activator_Console_Command_Cache_user;
	public static String Activator_Console_Command_Error_Option;
	public static String Activator_Console_Command_Help;
	public static String Activator_Console_Command_Help_Cache;
	public static String SecureGuard_NoAuthentificationService;
	public static String CurrentUserResource_Debug_Empty_old_password;
	public static String CurrentUserResource_Error_badparamters;
	
	static {
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {}
}
