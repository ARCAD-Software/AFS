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
package com.arcadsoftware.afs.client.core.ui.actions;

import com.arcadsoftware.aev.core.ui.actions.ArcadAction;
import com.arcadsoftware.afs.client.core.AFSRightManager;
import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;

public abstract class AbstractConnectedBasicAction extends ArcadAction 
implements ISecuredAction, IActionActivationManager{

	protected DataAccessHelper helper;
	protected ServerConnection connection;
	
	public AbstractConnectedBasicAction(ServerConnection connection){
		super();
		helper = new DataAccessHelper(connection);
		this.connection = connection; 
	}

	@Override
	protected boolean canExecute() {
		boolean result =  super.canExecute();
		if (result) {
			result = isAllowed();
		}
		return result;
	}
	
	public boolean isAllowed() {
		boolean result = connection.isAllowed(getExpectedRigths());
		if(!result) {
			AFSRightManager.getRightManager().missingRight(getExpectedRigths());
		}
		return result;
	}

	public boolean allowMultiSelection() {
		return true;
	}
	
	public boolean isAvailable() {
		return canExecute();
	}
		
}
