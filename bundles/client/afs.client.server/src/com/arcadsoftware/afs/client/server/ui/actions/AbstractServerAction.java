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

import com.arcadsoftware.aev.core.ui.actions.ArcadAction;
import com.arcadsoftware.afs.client.core.servers.model.Server;

public abstract class AbstractServerAction extends ArcadAction {

	protected Server selectedServer = null;

	@Override
	protected boolean canExecute() {
		selectedServer = getServerToManage();
		return (selectedServer != null);
	}

	protected Server getServerToManage() {
		return null;
	}
}
