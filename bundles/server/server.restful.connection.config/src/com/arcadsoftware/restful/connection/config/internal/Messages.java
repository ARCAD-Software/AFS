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
package com.arcadsoftware.restful.connection.config.internal;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "com.arcadsoftware.restful.connection.config.internal.messages"; //$NON-NLS-1$
	public static String Activator_Debug_ConnectionChanged;
	public static String Activator_Error_EmptyPassword;
	public static String Activator_Error_IdInteger;
	public static String Activator_HashPassword;
	public static String Activator_HashUsage;
	public static String Activator_Help;
	public static String Activator_LoginDetailsSpecial;
	public static String Activator_LoginDetailsUser;
	public static String Activator_LoginList;
	public static String Activator_NoLogin;
	public static String Activator_NoLoginDetails;
	public static String Activator_Usage;
	public static String ConnectionCredential_Error_InvalidPassword;
	public static String ConnectionCredential_Text;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {}
}
