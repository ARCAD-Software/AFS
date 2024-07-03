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
package com.arcadsoftware.afs.framework.application.internal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.internal.WorkbenchPlugin;
import org.eclipse.ui.internal.registry.ActionSetRegistry;
import org.eclipse.ui.internal.registry.IActionSetDescriptor;

import com.arcadsoftware.afs.framework.application.IActionRegister;
import com.arcadsoftware.afs.framework.application.IBasicMenuFactory;
import com.arcadsoftware.afs.framework.application.IConfigurationManager;
import com.arcadsoftware.afs.framework.application.IMENUConst;

@SuppressWarnings("restriction")
public class BasicActionBarAdvisor extends ActionBarAdvisor implements IActionRegister {

	private IWorkbenchWindow workbenchWindow;
	private IBasicMenuFactory menuFactory;
	private IConfigurationManager manager;

	private IConfigurationManager getManager() {
		if (manager == null) {
			manager = Activator.getInstance().getConfigurationManager();
		}
		return manager;
	}

	public BasicActionBarAdvisor(IActionBarConfigurer configurer) {
		super(configurer);

		removeUnWantedActions();
	}

	/**
	 * This operation allows cleaning action Sets coming form eclipse plugins, that can not be cleaned another way. Not
	 * really clean... But Efficient!
	 */
	private void removeUnWantedActions() {
		final ActionSetRegistry asr = WorkbenchPlugin.getDefault().getActionSetRegistry();
		final IActionSetDescriptor[] actionSets = asr.getActionSets();

		IExtension ext = null;
		if (getManager() != null) {
			for (final IActionSetDescriptor actionSet : actionSets) {
				// System.out.println(actionSet.getId());
				if (!getManager().isActionSetVisible(actionSet.getId())) {
					// System.out.println(" ===> REMOVED");
					ext = actionSet.getConfigurationElement().getDeclaringExtension();
					asr.removeExtension(ext, new Object[] { actionSet });
				}
			}
		}
	}

	@Override
	public IWorkbenchWindow getWorkbenchWindow() {
		return workbenchWindow;
	}

	@Override
	public void register(IAction action) {
		super.register(action);
	}

	@Override
	public void disposeAction(IAction action) {
		super.disposeAction(action);
	}

	@Override
	protected void makeActions(IWorkbenchWindow window) {
		workbenchWindow = window;
	}

	private boolean isMenuVisible(String menuId) {
		if (getManager() != null) {
			return getManager().isMenuVisible(menuId);
		}
		return false;
	}

	private String getMenuLabel(String menuId) {
		if (getManager() != null) {
			return getManager().getMenuLabel(menuId);
		}
		return menuId;
	}

	private void createFileMenu(IMenuManager menuBar) {
		if (isMenuVisible(IMENUConst.FILE)) {
			final MenuManager fileMenu = new MenuManager(getMenuLabel(IMENUConst.FILE),
					IWorkbenchActionConstants.M_FILE);
			menuBar.add(fileMenu);
			menuFactory.createFileSubMenu(this, fileMenu);
		}
	}

	private void createEditMenu(IMenuManager menuBar) {
		if (isMenuVisible(IMENUConst.EDIT)) {
			final MenuManager editMenu = new MenuManager(getMenuLabel(IMENUConst.EDIT),
					IWorkbenchActionConstants.M_EDIT);
			menuBar.add(editMenu);
			menuFactory.createEditSubMenu(this, editMenu);
		}
	}

	private void createHelpMenu(IMenuManager menuBar) {
		if (isMenuVisible(IMENUConst.HELP)) {
			final MenuManager helpMenu = new MenuManager(getMenuLabel(IMENUConst.HELP),
					IWorkbenchActionConstants.M_HELP);
			menuBar.add(helpMenu);
			menuFactory.createHelpSubMenu(this, helpMenu);
		}
	}

	private void createWindowMenu(IMenuManager menuBar) {
		if (isMenuVisible(IMENUConst.WINDOW)) {
			final MenuManager helpMenu = new MenuManager(getMenuLabel(IMENUConst.WINDOW),
					IWorkbenchActionConstants.M_WINDOW);
			menuBar.add(helpMenu);
			menuFactory.createWindowSubMenu(this, helpMenu);
		}
	}

	private boolean loadMenuFactory() {
		for (final IConfigurationElement element : Platform.getExtensionRegistry()
				.getConfigurationElementsFor(IBasicMenuFactory.MENUFACTORY_EXTENSION)) {
			try {
				menuFactory = (IBasicMenuFactory) element.createExecutableExtension("class");
				return true;
			} catch (final CoreException e) {
				Activator.getInstance().getLog().log(new Status(IStatus.ERROR, //
						Activator.getInstance().getBundle().getSymbolicName(), //
						e.getLocalizedMessage(), e));
			}
		}
		return false;
	}

	private void createStandardMenus(IMenuManager menuBar) {
		if (loadMenuFactory()) {
			createFileMenu(menuBar);
			createEditMenu(menuBar);
			createWindowMenu(menuBar);
			createHelpMenu(menuBar);
		} else {
			Activator.getInstance().getLog().log(new Status(IStatus.WARNING, //
					Activator.getInstance().getBundle().getSymbolicName(), //
					Messages.resString("configuration.warning"))); //$NON-NLS-1$
		}
	}

	@Override
	protected void fillMenuBar(IMenuManager menuBar) {
		createStandardMenus(menuBar);
	}
}
