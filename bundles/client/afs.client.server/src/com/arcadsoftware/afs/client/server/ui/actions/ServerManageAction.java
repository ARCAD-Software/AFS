/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.server.connection.ConnectionManager;
import com.arcadsoftware.afs.client.server.internals.Activator;

public class ServerManageAction extends AbstractServerAction {

	@Override
	protected void setInterface() {
		setText(Activator.resString("server.action.manage.text")); //$NON-NLS-1$
		setToolTipText(Activator.resString("server.action.manage.tooltip"));//$NON-NLS-1$
		setImageDescriptor(AFSIcon.SERVER_EDIT.imageDescriptor());
	}

	@Override
	protected boolean execute() {
		final ServerConnection connection = ConnectionManager.getInstance().connect(selectedServer, manageUser());
		return (connection != null) && connection.isConnected();
	}

	public boolean manageUser() {
		return true;
	}

}
