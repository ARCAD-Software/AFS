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

import com.arcadsoftware.afs.client.core.servers.model.ServerLoader;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.client.server.ui.dialogs.ServerDialog;


public class ServerPropertiesAction extends AbstractServerAction {
	@Override
	protected void setInterface() {
		setText(Activator.resString("server.action.properties.text")); //$NON-NLS-1$
		setToolTipText(Activator.resString("server.action.properties.tooltip"));//$NON-NLS-1$
		setImageDescriptor(Activator.getInstance().getImageDescriptor("PROPERTIES"));//$NON-NLS-1$
	}
	
	
	@Override
	protected boolean execute() {
		if (ServerDialog.updateServer(selectedServer)){
			return ServerLoader.getInstance().update(selectedServer);
		}
		return false;
	}
}
