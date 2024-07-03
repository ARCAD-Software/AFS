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
package com.arcadsoftware.afs.framework.application;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;
import org.eclipse.ui.actions.ContributionItemFactory;

import com.arcadsoftware.afs.framework.application.internal.Activator;

public abstract class AbstractBasicMenuFactory implements IBasicMenuFactory {

	public boolean isMenuVisible(String menuPath) {
		final IConfigurationManager manager = Activator.getInstance().getConfigurationManager();
		if (manager != null) {
			return manager.isMenuVisible(menuPath);
		}
		return false;
	}

	public String getMenuLabel(String menuPath) {
		final IConfigurationManager manager = Activator.getInstance().getConfigurationManager();
		if (manager != null) {
			return manager.getMenuLabel(menuPath);
		}
		return ""; //$NON-NLS-1$
	}

	protected boolean addMenu(String menuId, IActionRegister actionBarAdvisor, MenuManager menu,
			IWorkbenchWindow windows, ActionFactory action) {
		if (isMenuVisible(menuId)) {
			final IWorkbenchAction a = action.create(windows);
			actionBarAdvisor.register(a);
			menu.add(a);
			return true;
		}
		return false;
	}

	protected boolean addMenu(String menuId, IActionRegister actionBarAdvisor, MenuManager menu,
			IWorkbenchAction action) {
		if (isMenuVisible(menuId)) {
			actionBarAdvisor.register(action);
			menu.add(action);
			return true;
		}
		return false;
	}

	protected boolean addMenu(String menuId, IActionRegister actionBarAdvisor, MenuManager menu,
			IWorkbenchWindow windows, ContributionItemFactory action) {
		if (isMenuVisible(menuId)) {
			final IContributionItem a = action.create(windows);
			menu.add(a);
			return true;
		}
		return false;
	}

	protected boolean addMenu(String menuId, IActionRegister actionBarAdvisor, MenuManager menu, IAction action) {
		if (isMenuVisible(menuId)) {
			actionBarAdvisor.register(action);
			menu.add(action);
			return true;
		}
		return false;
	}

}
