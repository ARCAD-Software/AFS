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
package com.arcadsoftware.afs.client.core;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.arcadsoftware.afs.client.core.internal.BaseActivator;
import com.arcadsoftware.afs.client.core.singletons.SingletonManager;

public class AFSRightManager {

	public static final String EXTENSION_ID = "com.arcadsoftware.afs.client.core.rightmanager"; //$NON-NLS-1$
	public static final String CLASS_ATTRIBUTE = "class"; //$NON-NLS-1$

	private AFSRightManager() {

	}

	public static IRightManagerExtension getRightManager() {
		final IExtensionRegistry registry = Platform.getExtensionRegistry();
		final IConfigurationElement[] elements = registry.getConfigurationElementsFor(EXTENSION_ID);
		for (final IConfigurationElement element : elements) {
			final String className = element.getAttribute(CLASS_ATTRIBUTE);
			try {
				final IRightManagerExtension extensionInstance = (IRightManagerExtension) element
						.createExecutableExtension(CLASS_ATTRIBUTE);
				return SingletonManager.get(extensionInstance.getClass());
			} catch (final CoreException e) {
				BaseActivator.getLogger().warn(String.format("Error looking for %s instance.", className), e);
			}
		}
		return null;
	}
}
