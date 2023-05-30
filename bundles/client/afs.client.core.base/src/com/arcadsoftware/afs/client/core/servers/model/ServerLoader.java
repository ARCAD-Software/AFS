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
package com.arcadsoftware.afs.client.core.servers.model;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;

import com.arcadsoftware.afs.client.core.internal.BaseActivator;
import com.arcadsoftware.afs.client.core.internal.server.model.FileServerLoader;

public class ServerLoader {
	
	public static final String EXTENSION_POINT = "com.arcadsoftware.afs.client.core.serverLoader"; //$NON-NLS-1$
	private static IServerLoader serverLoaderInstance;
	
	public static IServerLoader getInstance() {
		if (serverLoaderInstance == null) {
			for (IConfigurationElement e: Platform.getExtensionRegistry().getConfigurationElementsFor(EXTENSION_POINT)) {
				try {
					Object o = e.createExecutableExtension("class"); //$NON-NLS-1$
					if (o instanceof IServerLoader) {
						serverLoaderInstance = (IServerLoader) o;
					}
				} catch (CoreException e1) {
					BaseActivator.getDefault().error(e1.getLocalizedMessage(),e1);
				}
			}
			// Get default implementation (use an XML file on the local file system).
			if (serverLoaderInstance == null) {
				serverLoaderInstance = new FileServerLoader();
			}
		}
		return serverLoaderInstance;
	}
	
	public static void setInstance(IServerLoader instance) {
		serverLoaderInstance = instance;
	}
	
	private ServerLoader() {
		super();
	}

}
