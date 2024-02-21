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
package com.arcadsoftware.server.properties.internal;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = Messages.class.getPackage().getName() + ".messages";
	public static String Activator_Command_Purge;
	public static String Activator_Command_PurgeAll;
	public static String Activator_Command_PurgeNone;
	public static String Activator_Help;
	public static String AdminPropertiesResource_EmptyFileError;
	public static String AdminPropertiesResource_NoDeletionError;
	public static String AdminPropertiesResource_UploadError;
	public static String PropertiesResource_File_Not_Readable;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
