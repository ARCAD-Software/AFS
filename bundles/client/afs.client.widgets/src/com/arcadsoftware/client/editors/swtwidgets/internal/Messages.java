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
package com.arcadsoftware.client.editors.swtwidgets.internal;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "com.arcadsoftware.client.editors.swtwidgets.messages"; //$NON-NLS-1$
	public static String DateInputSWTProvider_okButton;
	public static String DownloadSWTProvider_FileDialogText;
	public static String DownloadSWTProvider_OpenFileButton;
	public static String WebLinkSWTProvider_ViewButton;
	public static String ArcadBrowserViewer_ValidateItemLabel;
	public static String yes;
	public static String no;
	public static String patternMessage;
	public static String linkTable_remove;
	public static String linkTable_label;
	public static String linkTable_edit;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
		// Do nothing
	}
}
