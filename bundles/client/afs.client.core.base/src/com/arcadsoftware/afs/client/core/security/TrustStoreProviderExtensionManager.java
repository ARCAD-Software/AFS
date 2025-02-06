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
package com.arcadsoftware.afs.client.core.security;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.arcadsoftware.afs.client.core.connection.ITrustStoreProvider;
import com.arcadsoftware.afs.client.core.internal.BaseActivator;

public class TrustStoreProviderExtensionManager {
	
	public static final String TRUSTSTORE_EXTENSION_ID = "com.arcadsoftware.afs.client.truststore.provider"; //$NON-NLS-1$
	public static final String CLASS_ATTRIBUTE = "class"; //$NON-NLS-1$

	private TrustStoreProviderExtensionManager() {}

	public static ITrustStoreProvider getTrustStoreProvider() {
		final IExtensionRegistry registry = Platform.getExtensionRegistry();
		final IConfigurationElement[] elements = registry.getConfigurationElementsFor(TRUSTSTORE_EXTENSION_ID);
		for (final IConfigurationElement element : elements) {
			try {
				return (ITrustStoreProvider) element.createExecutableExtension(CLASS_ATTRIBUTE);
			} catch (final CoreException e) {
				BaseActivator.getDefault().warn("TrustStoreProviderExtensionManager::getTrustStoreProvider", e);
			}
		}
		return null;
	}
}
