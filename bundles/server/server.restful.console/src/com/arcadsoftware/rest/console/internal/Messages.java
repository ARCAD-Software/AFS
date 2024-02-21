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
package com.arcadsoftware.rest.console.internal;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "com.arcadsoftware.rest.console.internal.messages"; //$NON-NLS-1$

	public static String Activator_Error_LoadingPage;

	public static String DataSourceSection_NoDSReturnedFromClient;

	public static String DataSourceSection_SQLError;
	public static String Section_FileSystemNotAvailable;
	public static String Section_InvalidFileName;
	public static String Section_NewSectionInstalled;
	public static String SectionsListResource_Description;
	public static String SectionsListResource_Name;

	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {}
}
