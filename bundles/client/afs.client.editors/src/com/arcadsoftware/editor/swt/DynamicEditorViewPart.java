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
package com.arcadsoftware.editor.swt;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import com.arcadsoftware.editor.implementation.swt.renderer.SWTRenderer;

public abstract class DynamicEditorViewPart extends ViewPart {

	private static final String COM_ARCADSOFTWARE_EDITORS_TEST_VIEWER = "com.arcadsoftware.editors.test.viewer"; //$NON-NLS-1$

	private DynamicEditorComposite edyn;

	public DynamicEditorViewPart() {
		super();
	}

	@Override
	public void createPartControl(Composite parent) {
		edyn = new DynamicEditorComposite(parent, null, getType(), getLayoutName());
		((SWTRenderer) edyn.getRenderer()).addTitleChangeListener(new IEditorTitleChangeListener() {

			@Override
			@SuppressWarnings("synthetic-access")
			public void changed(ISWTRenderer renderer, String title) {
				setPartName(title);
			}
		});
		// loading
		// Define the selection provider.
		getSite().setSelectionProvider(edyn.getSelectionProvider());
		// Create the help context id for the viewer's control
		PlatformUI.getWorkbench().getHelpSystem().setHelp(edyn, COM_ARCADSOFTWARE_EDITORS_TEST_VIEWER);
		final IActionBars bars = getViewSite().getActionBars();
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());
	}

	protected void fillLocalPullDown(IMenuManager manager) {
		for (final IAction action : edyn.getBasicActions()) {
			manager.add(action);
		}
		manager.add(new Separator());
		for (final IAction action : edyn.getGlobalActions()) {
			manager.add(action);
		}
	}

	protected void fillLocalToolBar(IToolBarManager manager) {
		for (final IAction action : edyn.getBasicActions()) {
			manager.add(action);
		}
	}

	@Override
	public void setFocus() {
		edyn.setFocus();
	}

	protected abstract String getType();

	protected abstract String getLayoutName();

	public void load(int id) {
		edyn.load(id);
	}
}
