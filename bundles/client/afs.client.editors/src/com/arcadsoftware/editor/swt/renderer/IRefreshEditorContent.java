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
package com.arcadsoftware.editor.swt.renderer;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.swt.ISWTRenderer;

/**
 * This interface permits to refresh editor content.
 */
public interface IRefreshEditorContent {

	/**
	 * Refresh editor content with the given beanMap.
	 * 
	 * @param beanMap
	 *            the beanMap to refresh
	 * @param renderer
	 *            the renderer which have modified the given beanMap
	 */
	public void refreshEditorContent(BeanMap beanMap, ISWTRenderer renderer);

	/**
	 * @param renderer
	 *            the compared renderer.
	 * @return true if internal renderer equals given renderer.
	 */
	public boolean isSameRenderer(ISWTRenderer renderer);

}
