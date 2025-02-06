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
package com.arcadsoftware.afs.client.server.ui.actions;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.server.IServerGetLogFileProvider;
import com.arcadsoftware.afs.client.server.connection.ConnectionManager;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.framework.ui.plugins.LogUITools;

public abstract class ServerGetLogfileAction extends AbstractServerAction {

	public static final String EXTENSION_ID = "com.arcadsoftware.afs.client.server.getlogfile.provider"; //$NON-NLS-1$
	public static final String CLASS_ATTRIBUTE = "class"; //$NON-NLS-1$

	@Override
	protected void setInterface() {
		setText(Activator.resString("server.action.getlog.text")); //$NON-NLS-1$
		setToolTipText(Activator.resString("server.action.getlog.tooltip"));//$NON-NLS-1$
		setImageDescriptor(AFSIcon.SERVER_LOG.imageDescriptor());
	}

	private ServerConnection retrieveConnection() {
		ServerConnection connection = getServerConnection();
		if (connection == null) {
			connection = ConnectionManager.getInstance().connect(getServerToManage(), true);
		}
		return connection;
	}

	private IServerGetLogFileProvider getProvider() {
		final IExtensionRegistry registry = Platform.getExtensionRegistry();
		final IConfigurationElement[] elements = registry.getConfigurationElementsFor(EXTENSION_ID);
		for (final IConfigurationElement element : elements) {
			try {
				final IServerGetLogFileProvider provider = (IServerGetLogFileProvider) element
						.createExecutableExtension(CLASS_ATTRIBUTE);
				return provider;
			} catch (final CoreException e) {
				LogUITools.logWarning(Activator.getDefault().getBundle(), e);
			}
		}
		return null;
	}

	@Override
	protected boolean execute() {
		final ServerConnection connection = retrieveConnection();
		final IServerGetLogFileProvider provider = getProvider();
		if (provider != null) {
			provider.setServerConnection(connection);
			if (provider.getLogFile()) {
				provider.openFile();
			}
		}
		return true;
	}

	protected abstract ServerConnection getServerConnection();

}
