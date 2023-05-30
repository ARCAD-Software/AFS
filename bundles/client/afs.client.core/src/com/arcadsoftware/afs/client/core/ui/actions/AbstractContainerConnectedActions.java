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

import java.util.List;

import com.arcadsoftware.aev.core.ui.actions.ArcadAction;
import com.arcadsoftware.aev.core.ui.actions.ArcadActions;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;

public abstract class AbstractContainerConnectedActions extends ArcadActions {

	protected ServerConnection connection;
	public AbstractContainerConnectedActions(ServerConnection connection){
		super();
		this.connection = connection;
		makeAction();
	}
	
	
	public void setExtentedActions(List<ArcadAction> extActions) {
		if (extActions!=null) {
			for (ArcadAction action:extActions) {
				if (action==null) {
					addSeparator();
				} else {
					addAction(action);
				}
			}		
		}
	}
	
}
