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
package com.arcadsoftware.afs.client.server;

import com.arcadsoftware.afs.client.server.internals.Activator;

public interface ISRVIconConsts {

	public static final String LOCALID = Activator.getDefault().getBundle().getSymbolicName();
	public static final String LOCALPATH = LOCALID + ":icons/";//$NON-NLS-1$
	public static final String SPLASH = LOCALPATH + "splash/arcad.png"; //$NON-NLS-1$
}
