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
package com.arcadsoftware.afs.client.core.internal;

import java.util.ResourceBundle;

public class Resources {
	public static final String PLUGIN_ID = "com.arcadsoftware.afs.client.core.base"; //$NON-NLS-1$
	public static final String BUNDLE_ID = PLUGIN_ID + ".resources"; //$NON-NLS-1$

	private final ResourceBundle resourceBundle;

	private static Resources resources = new Resources();

	private Resources() {
		resourceBundle = ResourceBundle.getBundle(BUNDLE_ID);
	}

	public static String resString(final String key, Object... params) {
		return String.format(resources.resourceBundle.getString(key), params);
	}
}
