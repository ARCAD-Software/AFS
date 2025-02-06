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

import com.arcadsoftware.aev.core.ui.actions.ArcadAction;
import com.arcadsoftware.afs.client.core.AFSRightManager;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.servers.model.Server;
import com.arcadsoftware.afs.client.core.ui.actions.ISecuredAction;
import com.arcadsoftware.afs.client.server.connection.ConnectionManager;
import com.arcadsoftware.afs.client.server.ui.containers.ServerContainer;

public abstract class AbstractExtendedServerAction extends ArcadAction implements ISecuredAction {

	private ServerContainer serverContainer;
	private Server selectedServer;
	private ServerConnection serverConnection;

	@Override
	protected boolean canExecute() {
		return (selectedServer != null);
	}

	public ServerContainer getServerContainer() {
		return serverContainer;
	}

	public void setServerContainer(ServerContainer serverContainer) {
		this.serverContainer = serverContainer;
	}

	public Server getSelectedServer() {
		return selectedServer;
	}

	public void setSelectedServer(Server selectedServer) {
		this.selectedServer = selectedServer;
	}

	public ServerConnection getServerConnection() {
		return serverConnection;
	}

	public void setServerConnection(ServerConnection serverConnection) {
		this.serverConnection = serverConnection;
	}

	private ServerConnection retrieveConnection() {
		if (serverConnection == null) {
			serverConnection = ConnectionManager.getInstance().connect(selectedServer, true);
			setServerConnection(serverConnection);
		}
		return serverConnection;
	}

	@Override
	protected boolean execute() {
		retrieveConnection();
		if (isAllowed()) {
			return doOnServer();
		} else {
			AFSRightManager.getRightManager().missingRight(getExpectedRigths());
			return false;
		}
	}

	@Override
	public boolean isAllowed() {
		return getServerConnection().isAllowed(getExpectedRigths());
	}

	public abstract boolean doOnServer();

}
