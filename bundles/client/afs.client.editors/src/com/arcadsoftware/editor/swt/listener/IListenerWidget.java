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
package com.arcadsoftware.editor.swt.listener;

import com.arcadsoftware.beanmap.BeanMap;

/**
 * Widgets implements this interface to listen other widget.
 */
public interface IListenerWidget {

	/**
	 * Refresh the widget.
	 *
	 * @param beanMap
	 *            the new beanMap value.
	 */
	public void refreshWidget(BeanMap beanMap);

}
