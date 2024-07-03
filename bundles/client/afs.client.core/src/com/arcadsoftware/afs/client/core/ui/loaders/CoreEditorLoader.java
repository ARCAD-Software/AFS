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
package com.arcadsoftware.afs.client.core.ui.loaders;

import java.util.Hashtable;
import java.util.Properties;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.editor.IEditorLoader;
import com.arcadsoftware.metadata.MetaDataEntity;

public class CoreEditorLoader implements IEditorLoader {

	Hashtable<ServerConnection, DataAccessHelper> helpers;
	ServerConnection connection = null;

	public CoreEditorLoader() {
		helpers = new Hashtable<>();
	}

	public void setServerConnection(ServerConnection connexion) {
		connection = connexion;

	}

	private DataAccessHelper getHelper(ServerConnection connexion) {
		DataAccessHelper helper = helpers.get(connexion);
		if (helper == null) {
			helper = new DataAccessHelper(connection);
			helpers.put(connection, helper);
		}
		return helper;
	}

	@Override
	public String loadXMLLayoutDocument(String name, String type, int kind) {
		if (connection != null) {
			final DataAccessHelper helper = getHelper(connection);
			return helper.getLayoutFile(name, type);
		}
		return null;
	}

	@Override
	public Properties loadProperties(String name) {
		if (connection != null) {
			final DataAccessHelper helper = getHelper(connection);
			return helper.getProperties(name);
		}
		return null;
	}

	@Override
	public MetaDataEntity loadMetaDataEntity(String type) {
		if (connection != null) {
			final DataAccessHelper helper = getHelper(connection);
			return helper.getEntity(type);
		}
		return null;
	}

}
