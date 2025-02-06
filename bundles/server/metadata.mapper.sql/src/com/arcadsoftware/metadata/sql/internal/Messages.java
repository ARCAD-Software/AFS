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
package com.arcadsoftware.metadata.sql.internal;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "com.arcadsoftware.metadata.sql.internal.messages"; //$NON-NLS-1$
	public static String Activator_MapperRegistered;
	public static String Fragments_InternalError;
	public static String Fragments_Invalid;
	public static String Fragments_NotFound;
	public static String Fragments_Unknown;
	public static String MapperSQLService_Empty;
	public static String MapperSQLService_Error_Count;
	public static String MapperSQLService_Error_Insert;
	public static String MapperSQLService_Error_ItemSelection;
	public static String MapperSQLService_Error_Selection;
	public static String MapperSQLService_Error_Update;
	public static String MapperSQLService_Info_MultiUpdateWithComplexCriteria;
	public static String MapperSQLService_InternalError_CriteriaNotReduced;
	public static String MapperSQLService_ItemSelection;
	public static String MapperSQLService_SQLError;
	public static String MapperSQLService_UnknownCriteria;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {}
}
