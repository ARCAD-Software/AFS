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
package com.arcadsoftware.editor.implementation.swt.renderer;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.IAction;

import com.arcadsoftware.editor.swt.actions.IEditorAction;
import com.arcadsoftware.editor.swt.renderer.IRendererActions;

/**
 * This class permits renderer defines actions on toolBar and menuBar.
 */
public class RendererActions implements IRendererActions{

	private List<IAction> toolbarActions;
	private Map<String, List<IAction>> menuActions;
	
	public List<IAction> getToolBarActions() {
		return toolbarActions;
	}

	public void addToolBarAction(IEditorAction action) {
		if (toolbarActions == null)
			toolbarActions = new ArrayList<IAction>();
		toolbarActions.add(action);
	}

	public Map<String, List<IAction>> getMenuActions() {
		return menuActions;
	}

	public void addMenuAction(String menuLabel, IEditorAction action) {
		if (menuActions == null)
			menuActions = new HashMap<String, List<IAction>>();
		List<IAction> listActions = menuActions.get(menuLabel);
		if (listActions == null) {
			listActions = new ArrayList<IAction>();
			menuActions.put(menuLabel, listActions);
		}
		listActions.add(action);
	}
	
}
