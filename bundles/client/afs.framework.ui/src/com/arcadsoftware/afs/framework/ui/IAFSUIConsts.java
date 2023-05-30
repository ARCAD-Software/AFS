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
package com.arcadsoftware.afs.framework.ui;

@Deprecated
public interface IAFSUIConsts {
	
	/**
	 * Seriously, you really have to use a 5 char long constant (23 long if you do not extends this shit) to replace a 2 char long and explicit constant ?
	 * Really ?
	 */
	@Deprecated
	public static final String EMPTY = ""; //$NON-NLS-1$
	
	/**
	 * This is the US default date format !!!
	 * Do not use as a default, use ISO one !
	 */
	@Deprecated
	public static final String DEFAULT_DATEFORMAT = "MM/dd/yyyy"; //$NON-NLS-1$
}
