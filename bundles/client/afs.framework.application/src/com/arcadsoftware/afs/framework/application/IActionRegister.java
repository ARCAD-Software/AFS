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

import org.eclipse.jface.action.IAction;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.application.IActionBarConfigurer;

public interface IActionRegister {

	public IWorkbenchWindow getWorkbenchWindow();

	/**
	 * Registers the given action with the key binding service (by calling
	 * {@link IActionBarConfigurer#registerGlobalAction(IAction)}), and adds it to the list of actions to be disposed
	 * when the window is closed.
	 * <p>
	 * In order to participate in key bindings, the action must have an action definition id (aka command id), and a
	 * corresponding command extension. See the <code>org.eclipse.ui.commands</code> extension point documentation for
	 * more details.
	 * </p>
	 * 
	 * @param action
	 *            the action to register, this cannot be <code>null</code>
	 * @see IAction#setActionDefinitionId(String)
	 * @see #disposeAction(IAction)
	 */
	public void register(IAction action);

	/**
	 * Disposes the given action.
	 * <p>
	 * The default implementation checks whether the action is an instance of
	 * <code>ActionFactory.IWorkbenchAction</code> and calls its <code>dispose()</code> method if so. Subclasses may
	 * extend.
	 * </p>
	 * 
	 * @param action
	 *            the action to dispose
	 */
	public void disposeAction(IAction action);

}
