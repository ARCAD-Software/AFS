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
package com.arcadsoftware.editor.swt.actions;

import com.arcadsoftware.aev.core.ui.actions.ArcadAction;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedTableViewer;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedTreeViewer;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.swt.IBeanMapSelector;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;

/**
 * Extends this class to create an action for dynamic editor.
 */
public abstract class AbstractEditorAction extends ArcadAction implements IEditorAction {

	protected IBeanMapSelector selector;
	protected ISWTRenderer renderer;
	protected Element element;
	protected String internalEditorId;
	protected AbstractColumnedTableViewer tableViewer;
	protected AbstractColumnedTreeViewer treeViewer;

	public AbstractColumnedTableViewer getTableViewer() {
		return tableViewer;
	}

	@Override
	public void setTableViewer(AbstractColumnedTableViewer tableViewer) {
		this.tableViewer = tableViewer;
	}

	public AbstractColumnedTreeViewer getTreeViewer() {
		return treeViewer;
	}

	@Override
	public void setTreeViewer(AbstractColumnedTreeViewer treeViewer) {
		this.treeViewer = treeViewer;
	}

	protected BeanMap getCurrentBeanMap() {
		if (selector != null) {
			return selector.getSelectedBeanMap();
		}
		return null;
	}

	@Override
	public void setBeanMapSelector(IBeanMapSelector selector) {
		this.selector = selector;
	}

	@Override
	public void setRenderer(ISWTRenderer renderer) {
		this.renderer = renderer;
		if (renderer.isReadOnly()) {
			setEnabled(false);
		}
	}

	@Override
	public void refreshSelector(BeanMap beanMap) {
		selector.refreshSelector(beanMap);
	}

	@Override
	public void setElement(Element element) {
		this.element = element;
	}

	@Override
	public void setInternalEditorId(String internalEditorId) {
		this.internalEditorId = internalEditorId;
	}

	public void selectBeanMap(BeanMap beanMap) {
		selector.selectBeanMap(beanMap);
	}

	public IBeanMapSelector getSelector() {
		return selector;
	}
}