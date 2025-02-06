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
package com.arcadsoftware.afs.client.core.ui.res;

import java.util.Properties;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;

public abstract class AbstractServerProperties {

	private Properties serverProperties;

	protected AbstractServerProperties() {
		super();
	}

	public String resString(final ServerConnection connection, final String key) {
		if ((connection == null) || (key == null)) {
			return "";
		}
		if (serverProperties == null) {
			serverProperties = new DataAccessHelper(connection).getProperties(getResourceFilename());
			if (serverProperties == null) {
				return key;
			}
		}
		final String value = serverProperties.getProperty(key, key);
		if ((value != null) && (value.startsWith("!"))) {
			return resString(connection, value.substring(1, value.length()));
		}
		return value;
	}

	public abstract String getResourceFilename();

}
