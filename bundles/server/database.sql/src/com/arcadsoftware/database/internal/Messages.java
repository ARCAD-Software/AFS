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
package com.arcadsoftware.database.internal;

import org.eclipse.osgi.util.NLS;

/**
 *
 */
public class Messages extends NLS {
	private static final String BUNDLE_NAME = "com.arcadsoftware.database.internal.messages"; //$NON-NLS-1$
	public static String Activator_ConnectionWith;
	public static String Activator_DataSource_Declared;
	public static String Activator_Error;
	public static String Activator_NoEventServiceAvailable;
	public static String Activator_NotImplemented;
	public static String Activator_NotValidJNDI;
	public static String DatabaseCommandProvider_Declared;
	public static String DatabaseCommandProvider_Error;
	public static String DatabaseCommandProvider_Help;
	public static String DatabaseCommandProvider_Help_db;
	public static String DatabaseCommandProvider_Help_DBhelp;
	public static String DatabaseCommandProvider_Help_dblist;
	public static String DatabaseCommandProvider_Help_dbtest;
	public static String DatabaseCommandProvider_id;
	public static String DatabaseCommandProvider_IDNumber;
	public static String DatabaseCommandProvider_list;
	public static String DatabaseCommandProvider_login;
	public static String DatabaseCommandProvider_MissingSGDB;
	public static String DatabaseCommandProvider_MissingURL;
	public static String DatabaseCommandProvider_NoConfigAdmin;
	public static String DatabaseCommandProvider_NoDB;
	public static String DatabaseCommandProvider_NoResult;
	public static String DatabaseCommandProvider_NumberOfRow;
	public static String DatabaseCommandProvider_password;
	public static String DatabaseCommandProvider_passwordxxxx;
	public static String DatabaseCommandProvider_PoolMax;
	public static String DatabaseCommandProvider_PoolMin;
	public static String DatabaseCommandProvider_Row;
	public static String DatabaseCommandProvider_sgdb;
	public static String DatabaseCommandProvider_SpecifyDataSource;
	public static String DatabaseCommandProvider_SpecifySQL;
	public static String DatabaseCommandProvider_Timeout;
	public static String DatabaseCommandProvider_Timing;
	public static String DatabaseCommandProvider_Undeclared;
	public static String DatabaseCommandProvider_url;
	public static String SQLDatabaseAccess_ComplexInsert;
	public static String SQLDatabaseAccess_DSClosed;
	public static String SQLDatabaseAccess_ListQuery;
	public static String SQLDatabaseAccess_MapListQuery;
	public static String SQLDatabaseAccess_MapQuery;
	public static String SQLDatabaseAccess_NoDSReady;
	public static String SQLDatabaseAccess_SimgleQuery;
	public static String SQLDatabaseAccess_Update;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
