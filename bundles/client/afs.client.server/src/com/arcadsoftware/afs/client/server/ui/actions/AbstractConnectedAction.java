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
import com.arcadsoftware.afs.client.core.connection.IConnectionListener;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.server.connection.ConnectionManager;

public abstract class AbstractConnectedAction extends ArcadAction implements IConnectionListener {

	private ServerConnection serverConnection;

	public AbstractConnectedAction() {
		setEnabled(false);
		ConnectionManager.getInstance().addConnectionListener(this);
	}

	@Override
	public void OnConnection(final ServerConnection connection) {
		setEnabled(true);
		serverConnection = connection;
	}

	protected ServerConnection getServerConnection() {
		return serverConnection;
	}
}
