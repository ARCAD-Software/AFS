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
package com.arcadsoftware.afs.client.server.ui.actions;

import org.eclipse.ui.IViewPart;

import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.AFSRightManager;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.actions.ISecuredAction;
import com.arcadsoftware.afs.client.server.connection.ConnectionManager;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.client.server.ui.views.SectionView;

public abstract class ServerConfigureAction extends AbstractServerAction implements ISecuredAction {
	
	@Override
	protected void setInterface() {
		setText(Activator.resString("server.action.configure.text")); //$NON-NLS-1$
		setToolTipText(Activator.resString("server.action.configure.tooltip"));//$NON-NLS-1$
		setImageDescriptor(AFSIcon.SERVER_CONFIG.imageDescriptor());
	}
	
	
	protected ServerConnection retrieveConnection(){
		ServerConnection connection  = getServerConnection();
		if (connection==null) {
			connection = ConnectionManager.getInstance().connect(getServerToManage(),true);		
			setServerConnection(connection);
		}
		return connection;
	}
	
	
	@Override
	protected boolean execute() {
		selectedServer = getServerToManage();
		ServerConnection connection = retrieveConnection();
		if (isAllowed()) {
			IViewPart view = Activator.getDefault().showView(SectionView.ID);
			if (view instanceof SectionView) {
				((SectionView)view).OnConnection(connection);
			}
			return true;
		} else {
			AFSRightManager.getRightManager().missingRight(getExpectedRigths());
			return false;
		}
	}
	
	@Override
	public boolean isAllowed() {
		return getServerConnection().isAllowed(getExpectedRigths());
	}	
	
	protected abstract ServerConnection getServerConnection();
	protected abstract void setServerConnection(ServerConnection connection);
}
