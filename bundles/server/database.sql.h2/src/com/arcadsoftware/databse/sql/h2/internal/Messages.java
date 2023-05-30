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
package com.arcadsoftware.databse.sql.h2.internal;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = Messages.class.getPackage().getName() + ".messages"; //$NON-NLS-1$
	public static String Activator_DBServerError;
	public static String Activator_DBStart;
	public static String Activator_DBStop;
	public static String Activator_ErrorMsg;
	public static String Activator_HelpH2;
	public static String Activator_HelpH2Manual;
	public static String Activator_HelpH2Server;
	public static String Activator_NoEvent;
	public static String Activator_ShellError;
	public static String Activator_ShellNoRemote;
	public static String Activator_ShellStarting;
	public static String Activator_ShellWarning;
	public static String Activator_UserNameNeeded;
	public static String Activator_UserPwdNeeded;
	public static String Activator_WebServerError;
	public static String Activator_WebServerStart1;
	public static String Activator_WebServerStart2;
	public static String Activator_WebServerStop;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {}
}
