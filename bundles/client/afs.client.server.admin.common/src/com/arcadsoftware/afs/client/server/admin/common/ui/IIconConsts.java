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
package com.arcadsoftware.afs.client.server.admin.common.ui;

import com.arcadsoftware.afs.client.server.admin.common.Activator;



public interface IIconConsts {
	public static final String FRAMEWORKUIID = "com.arcadsoftware.afs.framework.ui";//$NON-NLS-1$
	public static final String LOCALID = Activator.getInstance().getBundle().getSymbolicName();
	public static final String LOCALPATH = LOCALID+":/icons/";//$NON-NLS-1$
	
	public static final String CATEGORY = LOCALPATH+"category.png"; //$NON-NLS-1$
	public static final String SECTION = LOCALPATH+"section.png"; //$NON-NLS-1$

	public static final String ACTION = LOCALPATH+"action.png"; //$NON-NLS-1$
	public static final String SENDMAIL = LOCALPATH+"sendmail.png"; //$NON-NLS-1$

}
