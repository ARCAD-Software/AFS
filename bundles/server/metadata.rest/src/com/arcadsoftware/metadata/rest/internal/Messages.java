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
package com.arcadsoftware.metadata.rest.internal;

import org.eclipse.osgi.util.NLS;

/**
 *
 */
public class Messages extends NLS {
	
	private static final String BUNDLE_NAME = "com.arcadsoftware.metadata.rest.internal.messages"; //$NON-NLS-1$

	public static String Activator_Debug_EventNotFired;

	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {}
}
