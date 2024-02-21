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
package com.arcadsoftware.metadata.registry.internal;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = Messages.class.getName();
	public static String RegistryBundleListener_FileSystemNotAvailable;
	public static String RegistryBundleListener_InvalidFileName;
	public static String XMLRegistry_ErrorDuringLoadingProcess;
	public static String XmlRegistry_DEBUG_Generated;
	public static String XmlRegistry_EntityBelongToAnotherRegistry;
	public static String XmlRegistry_NoEntitiesDeclared;
	public static String XmlRegistry_NotAFile;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {}
}
