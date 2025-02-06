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
package com.arcadsoftware.afs.framework.application;

import org.eclipse.jface.action.MenuManager;

public interface IBasicMenuFactory {

	/**
	 * Extension point that use this interface.
	 */
	public static final String MENUFACTORY_EXTENSION = "com.arcadsoftware.afs.framework.application.menuFactory"; //$NON-NLS-1$

	public void createFileSubMenu(IActionRegister actionBarAdvisor, MenuManager fileMenu);

	public void createEditSubMenu(IActionRegister actionRegister, MenuManager editMenu);

	public void createHelpSubMenu(IActionRegister actionBarAdvisor, MenuManager helpMenu);

	public void createWindowSubMenu(IActionRegister actionBarAdvisor, MenuManager windowMenu);
}
