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
package com.arcadsoftware.afs.client.core.ui.widgets;

import java.util.List;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.actions.ISecuredAction;
import com.arcadsoftware.afs.client.core.ui.loaders.CoreContentLoader;
import com.arcadsoftware.editor.swt.ISWTDataLoader;
import com.arcadsoftware.editor.swt.actions.AbstractEditorAction;

public class AbstractConnectedEditorAction extends AbstractEditorAction
		implements ISecuredAction {
	private DataAccessHelper helper = null;

	public DataAccessHelper getHelper() {
		if (helper == null) {
			if (getConnection() != null) {
				helper = new DataAccessHelper(getConnection());
			}
		}
		return helper;
	}

	public ServerConnection getConnection() {
		final ISWTDataLoader dataLoader = renderer.getDataLoader();
		if (dataLoader instanceof CoreContentLoader) {
			final CoreContentLoader loader = (CoreContentLoader) dataLoader;
			final ServerConnection connection = loader.getConnection();
			return connection;
		}
		return null;
	}

	@Override
	public List<Integer> getExpectedRigths() {
		return null;
	}

	@Override
	public boolean isAllowed() {
		final ServerConnection connection = getConnection();
		if (connection != null) {
			final boolean result = connection.isAllowed(getExpectedRigths());
			if (!result) {
				Activator.getDefault().missingRight(getExpectedRigths());
			}
			return result;
		} else {
			return false;
		}
	}

}
