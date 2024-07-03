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
package com.arcadsoftware.afs.framework.ui.views;

import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;

public class AbstractAFSView extends ViewPart {

	protected String initialPartName = ""; //$NON-NLS-1$

	protected void defineActions() {
		// Do nothing
	}

	protected void defineLocalToolbar(IToolBarManager manager) {
		// Do nothing
	}

	protected void defineLocalPullDown(IMenuManager manager) {
		// Do nothing
	}

	protected void defineLocalContextMenu(IMenuManager manager) {
		// Do nothing
	}

	protected void doOnDoubleClick() {
		// Do nothing
	}

	public void doOnSelect() {
		// Do nothing
	}

	protected void hookContextMenu() {
		final MenuManager menuMgr = new MenuManager("#PopupMenu"); //$NON-NLS-1$
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			@Override
			public void menuAboutToShow(IMenuManager manager) {
				AbstractAFSView.this.fillContextMenu(manager);
			}
		});
	}

	protected void contributeToActionBars() {
		final IActionBars bars = getViewSite().getActionBars();
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());
		bars.updateActionBars();
		bars.getToolBarManager().update(true);
	}

	protected void fillLocalPullDown(IMenuManager manager) {
		defineLocalPullDown(manager);
		manager.update(true);
		getViewSite().getActionBars().updateActionBars();
	}

	protected void fillContextMenu(IMenuManager manager) {
		defineLocalContextMenu(manager);
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}

	protected void fillLocalToolBar(IToolBarManager manager) {
		defineLocalToolbar(manager);
	}

	@Override
	public void init(IViewSite site) throws PartInitException {
		super.init(site);
		initialPartName = getPartName();
	}

	@Override
	public void createPartControl(Composite parent) {
		setInterface();
		hookContextMenu();
	}

	public void setInterface() {
		contributeToActionBars();
	}

	@Override
	public void setFocus() {
		// Do nothing
	}

	public void activate() {
		// Do nothing
	}

}
