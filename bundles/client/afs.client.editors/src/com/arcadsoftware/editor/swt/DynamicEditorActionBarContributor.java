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
package com.arcadsoftware.editor.swt;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.ICoolBarManager;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.window.ApplicationWindow;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.EditorActionBarContributor;

/**
 * This class implements the action bar contributor for dynamic editor.
 */
public class DynamicEditorActionBarContributor extends EditorActionBarContributor {

	private IToolBarManager toolBarManager;
	private IMenuManager menuManager;

	@Override
	public void contributeToCoolBar(ICoolBarManager coolBarManager) {
		// Do nothing
	}

	@Override
	public void contributeToMenu(IMenuManager newMenuManager) {
		this.menuManager = newMenuManager;
	}

	@Override
	public void contributeToStatusLine(IStatusLineManager statusLineManager) {
		// Do nothing
	}

	@Override
	public void contributeToToolBar(IToolBarManager newToolBarManager) {
		this.toolBarManager = newToolBarManager;
	}

	@Override
	public void setActiveEditor(IEditorPart targetEditor) {
		if (targetEditor instanceof DynamicEditorPart) {
			DynamicEditorPart editor = (DynamicEditorPart) targetEditor;
			refreshToolBar(editor);
			refreshMenu(editor);
		}
	}

	private void refreshToolBar(DynamicEditorPart editor) {
		toolBarManager.removeAll();
		List<IAction> actions = editor.getToolBarActions();
		if (actions != null) {
			for (IAction action: actions) {
				toolBarManager.add(action);
			}
		}
		toolBarManager.update(true);
	}

	private void refreshMenu(DynamicEditorPart editor) {
		Map<String, List<IAction>> actions = editor.getMenuActions();
		if (actions != null) {
			List<IMenuManager> menus = new ArrayList<IMenuManager>();
			for (Map.Entry<String, List<IAction>> entry : actions.entrySet()) {
				IMenuManager menu = null;
				IWorkbenchWindow activeWorkbenchWindow = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
				if (activeWorkbenchWindow instanceof ApplicationWindow) {
					MenuManager menuBarManager = ((ApplicationWindow) activeWorkbenchWindow).getMenuBarManager();
					if (menuBarManager != null) {
						for (IContributionItem item : menuBarManager.getItems()) {
							if (item instanceof MenuManager
									&& ((MenuManager) item).getMenuText().equalsIgnoreCase(entry.getKey())) {
								menu = (IMenuManager) item;
								break;
							}
						}
					}
				}
				if (menu == null) {
					menu = menuManager.findMenuUsingPath(entry.getKey());
				}
				if (menu == null) {
					menu = new MenuManager(entry.getKey());
					menuManager.add(menu);
				}
				if (!menus.contains(menu)) {
					menu.removeAll();
					menus.add(menu);
				}
				for (IAction action: entry.getValue()) {
					menu.add(action);
				}
			}
			for (IMenuManager menu: menus) {
				menu.update(true);
			}
		}
	}

}
