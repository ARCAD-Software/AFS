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
package com.arcadsoftware.afs.client.server.ui.propertysource;

import com.arcadsoftware.afs.client.core.servers.model.Server;
import com.arcadsoftware.afs.client.core.servers.model.ServerLoader;
import com.arcadsoftware.afs.client.core.ui.propertysource.AbstractAFSProperties;

public class ServerPropertySource extends AbstractAFSProperties {

	public static final String PROPERTY_NAME = "server.name"; //$NON-NLS-1$
	public static final String PROPERTY_URL = "server.url"; //$NON-NLS-1$

	private final Server server;

	public ServerPropertySource(Server server) {
		this.server = server;
	}

	@Override
	public Object getPropertyValue(Object id) {
		if (id.equals(PROPERTY_URL)) {
			return server.getUrl();
		}
		if (id.equals(PROPERTY_NAME)) {
			return server.getName();
		}
		return null;
	}

	@Override
	public void setPropertyValue(Object id, Object value) {
		if (id.equals(PROPERTY_URL)) {
			server.setUrl((String) value);
			ServerLoader.getInstance().update(server);
		}
	}

	public Server getServer() {
		return server;
	}

}
