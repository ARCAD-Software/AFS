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
package com.arcadsoftware.afs.client.core.ui.actions;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

public interface IBeanMapActionListener {
	public static int ACTION_NONE = 0;
	public static int ACTION_ADD = 1;
	public static int ACTION_UPDATE = 2;
	public static int ACTION_DELETE = 3;
	public static int ACTION_RELOAD = 4;
	public static int ACTION_REFRESH = 5;

	public void actionDone(int type, BeanMap bean);

	public void actionDone(int type, BeanMapList list);
}
