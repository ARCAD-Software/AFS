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
package com.arcadsoftware.afs.client.core.ui.widgets;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;

public class ConnectedBasicEditorAction extends AbstractConnectedEditorAction {

	@Override
	public boolean isAllowed() {
		final ServerConnection connection = getConnection();
		if (connection != null) {
			return connection.isAllowed(getExpectedRigths());
		} else {
			return false;
		}
	}

	@Override
	protected boolean canExecute() {
		boolean result = super.canExecute();
		if (result) {
			result = isAllowed();
		}
		return result;
	}
}
