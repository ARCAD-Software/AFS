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
package com.arcadsoftware.editor.swt.actions;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.arcadsoftware.editor.implementation.Activator;

/**
 * This class permits to create instance of action defines by com.arcadsoftware.client.editor.action extension point.
 */
public class EditorActionFactory {

	private static final String EXTENSION_POINT_ID = "com.arcadsoftware.client.editor.action"; //$NON-NLS-1$
	private static final String ACTION = "action"; //$NON-NLS-1$
	private static final String CLASS = "class"; //$NON-NLS-1$
	private static final String ID = "id"; //$NON-NLS-1$

	private EditorActionFactory() {
		// Do nothing
	}

	/**
	 * Creates a new instance of action.
	 *
	 * @param actionId
	 *            The action id.
	 * @return The action instance if actionId is correct; null otherwise.
	 */
	public static IEditorAction getEditorAction(String actionId) {
		IEditorAction action = null;
		if (actionId == null) {
			throw new NullPointerException();
		}
		final IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
		final IConfigurationElement[] configurationElements = extensionRegistry
				.getConfigurationElementsFor(EXTENSION_POINT_ID);
		for (final IConfigurationElement element : configurationElements) {
			if (ACTION.equals(element.getName()) && actionId.equals(element.getAttribute(ID))) {
				try {
					action = (IEditorAction) element.createExecutableExtension(CLASS);
					break;
				} catch (final CoreException e) {
					Activator.getInstance().log(e);
				}
			}
		}
		return action;
	}

}
