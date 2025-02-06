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
package com.arcadsoftware.editor.swt.actions;

import org.eclipse.jface.action.IAction;

import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedTableViewer;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedTreeViewer;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.swt.IBeanMapSelector;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;

/**
 * This interface permits to define an action for dynamic editor.
 */
public interface IEditorAction extends IAction {

	/**
	 * @param selector
	 *            the selector to set
	 */
	public void setBeanMapSelector(IBeanMapSelector selector);

	/**
	 * @param renderer
	 *            the renderer to set
	 */
	public void setRenderer(ISWTRenderer renderer);

	/**
	 * @param beanMap
	 */
	public void refreshSelector(BeanMap beanMap);

	/**
	 * @param element
	 *            the element to set
	 */
	public void setElement(Element element);

	/**
	 * @param internalEditorId
	 *            the internalEditorId to set
	 */
	public void setInternalEditorId(String internalEditorId);

	/**
	 * @param viewer
	 *            the attached BeanMapTableViewer
	 */
	public void setTableViewer(AbstractColumnedTableViewer viewer);

	/**
	 * @param viewer
	 *            the attached BeanMapTreeViewer
	 */
	public void setTreeViewer(AbstractColumnedTreeViewer viewer);

}
