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
import com.arcadsoftware.afs.client.core.servers.model.ServerLoader;
import com.arcadsoftware.afs.client.server.internals.Activator;

public class ServerDeleteAction extends AbstractServerAction {

	@Override
	protected void setInterface() {
		setText(Activator.resString("server.action.delete.text")); //$NON-NLS-1$
		setToolTipText(Activator.resString("server.action.delete.tooltip"));//$NON-NLS-1$
		setImageDescriptor(AFSIcon.SERVER_DELETE.imageDescriptor());
	}

	@Override
	protected boolean execute() {
		return ServerLoader.getInstance().delete(selectedServer);
	}
}
