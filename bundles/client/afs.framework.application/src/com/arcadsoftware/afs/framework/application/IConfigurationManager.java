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

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.application.IWorkbenchConfigurer;

/**
 * This interface is used to define a <code>Configuration Manager</code>. It is used by the extension point
 * com.arcadsoftware.afs.rcp.base.configurationManager.
 * <p>
 * The main goals of such a class is to:
 * <ul>
 * <li>Manage the visual definition of the RCP (status line, coolBar, etc)</li>
 * <li>Define the actions to be executed during the application lifecyle</li>
 * <li>Define the default settings</li>
 * <li>Manage the content of the menu bar</li>
 * </ul>
 *
 * @author ARCAD Software
 */
public interface IConfigurationManager {

	/**
	 * Extension point that use this interface.
	 */
	public static final String CONFIGURATION_MANAGER_EXTENSION = "com.arcadsoftware.afs.framework.application.configurationManager"; //$NON-NLS-1$

	/**
	 * Indicates is the menu identified by the parameter <code>menuId</code> is visible
	 *
	 * @param menuId
	 *            the menu identifier to test
	 */
	public boolean isMenuVisible(String menuId);

	public String getMenuLabel(String menuId);

	/**
	 * Check if ActionSet is visible; Used to remove Action Sets coming from platform plugins.
	 *
	 * @param actionSetId
	 * @return
	 */
	public boolean isActionSetVisible(String actionSetId);

	public String getDefaultPerspectiveId();

	public void doAfterInitialization(IWorkbenchConfigurer configurer);

	public void preStartup();

	public void postStartup();

	public void postShutdown();

	public boolean preShutdown();

	public Point getInitialSize();

	/**
	 * Returns the visibility indicator of the cool bar
	 *
	 * @return true if the cool bar must be displayed.
	 */
	public boolean isCoolBarVisible();

	public boolean isProgressBarVisible();

	/**
	 * Returns the visibility indicator of the Progress Bar
	 *
	 * @return true if the Progress Bar must be displayed.
	 */
	public boolean isFastViewBarVisible();

	/**
	 * Returns the visibility indicator of the status Line
	 *
	 * @return true if the status Line must be displayed.
	 */
	public boolean isStatusLineVisible();

	/**
	 * Returns the visibility indicator of the perspective bar
	 *
	 * @return true if the perspective bar must be displayed.
	 */
	public boolean isPerspectiveBarVisible();

	public void setPerspectiveBarLocation();

	public void setPerspectiveBarStyle();

	/**
	 * Define the main workbench window style.
	 *
	 * @return zero if the window style must be the default one.
	 * @see Shell
	 */
	public int getShellStyle();

	/**
	 * Modifie the shellof the main window.
	 *
	 * @param shell
	 */
	public void adaptMainShell(Shell shell);
}
