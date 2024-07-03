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
package com.arcadsoftware.editor.swt.renderer;

import com.arcadsoftware.editor.swt.ISWTRenderer;

/**
 * This listener is informed when a list of BeanMap associated with the current edited data is ready to be used.
 *
 * @author ARCAD Software
 * @see ISWTRenderer#addLoadedList(ILoadedListListener)
 */
public interface ILoadedListListener {

	/**
	 * Called when the list of type listened are read to be used.
	 *
	 * @param renderer
	 */
	public void loadedListComplete(ISWTRenderer renderer);

	/**
	 * Used to get the type of the list values.
	 *
	 * @return
	 */
	public String getListType();

}
