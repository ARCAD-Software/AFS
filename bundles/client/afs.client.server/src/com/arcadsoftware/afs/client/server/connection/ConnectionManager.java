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
package com.arcadsoftware.afs.client.server.connection;

import java.util.Optional;

import com.arcadsoftware.afs.client.core.connection.IConnectionListener;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.security.TrustStoreProviderExtensionManager;
import com.arcadsoftware.afs.client.core.servers.model.IServerLoader;
import com.arcadsoftware.afs.client.core.servers.model.Server;
import com.arcadsoftware.afs.client.core.servers.model.ServerLoader;
import com.arcadsoftware.afs.client.core.singletons.SingletonManager;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.client.server.internals.ui.dialogs.ConnectionDialog;
import com.arcadsoftware.afs.framework.messages.UserMessageManager;
import com.arcadsoftware.afs.framework.ui.AbstractAFSListenerList;

public class ConnectionManager {
	
	public static ConnectionManager getInstance() {
		return SingletonManager.get(ConnectionManager.class);
	}
	
	private final AbstractAFSListenerList connectionListeners = new AbstractAFSListenerList();	
	private Optional<ServerConnection> lastServerConnection = Optional.empty();
	
	private ConnectionManager() {
		super();
	}
	
	public void addConnectionListener(IConnectionListener listener) {
		connectionListeners.add(listener);
	}
	
	public void removeConnectionListener(IConnectionListener listener) {
		connectionListeners.remove(listener);
	}
	
	private void fireConnection(ServerConnection connection) {
		for (Object o: getInstance().connectionListeners.getListeners()) {
			IConnectionListener listener = (IConnectionListener) o;
			try {
				listener.OnConnection(connection);
			} catch (Exception e) {
				Activator.getInstance().error(e.getLocalizedMessage(),e);
				removeConnectionListener(listener);
			}
		}
		getInstance().lastServerConnection = Optional.ofNullable(connection);
	}	

	public Optional<ServerConnection> getLastServerConnection() {
		return lastServerConnection;
	}
	
	public ServerConnection connect(Server selectedServer,boolean manageUser) {
		return connect(selectedServer, manageUser, ServerLoader.getInstance());
	}
	
	public ServerConnection connect(Server selectedServer, boolean manageUser, IServerLoader serverLoader) {
		if ((selectedServer != null) && ConnectionDialog.connect(selectedServer, manageUser)) {
			ServerConnection result = new ServerConnection(selectedServer);
			result.setTrustStoreprovider(TrustStoreProviderExtensionManager.getTrustStoreProvider());
			result.setMessageManager(UserMessageManager.getInstance());
			if (result.connect(selectedServer.getLastLogin(), selectedServer.getLastPassword(),manageUser)) {
				if (!selectedServer.isRememberPassword()) {
					selectedServer.setLastPassword(""); //$NON-NLS-1$
				}
				if (serverLoader != null) {
					serverLoader.update(selectedServer);
				}
				fireConnection(result);
			}
			return result;
		}
		return null;
	}		
}
