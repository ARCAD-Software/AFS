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

import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.IAction;

import com.arcadsoftware.editor.swt.actions.IEditorAction;

/**
 * This interface permits renderer defines actions on toolBar and menuBar.
 */
public interface IRendererActions {

	/**
	 * @return the toolBar list actions.
	 */
	public List<IAction> getToolBarActions();

	/**
	 * @return the menuBar list actions.
	 */
	public Map<String, List<IAction>> getMenuActions();

	/**
	 * Add the given action in toolbar.
	 *
	 * @param action
	 *            the action to be added.
	 */
	public void addToolBarAction(IEditorAction action);

	/**
	 * Add the given action in menu.
	 *
	 * @param menuLabel
	 *            the menu label.
	 * @param action
	 *            the action to be added.
	 */
	public void addMenuAction(String menuLabel, IEditorAction action);

}
