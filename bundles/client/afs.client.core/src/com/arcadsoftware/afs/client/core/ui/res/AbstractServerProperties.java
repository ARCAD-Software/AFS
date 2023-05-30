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
package com.arcadsoftware.afs.client.core.ui.res;

import java.util.Properties;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;

public abstract class AbstractServerProperties {
	
	private Properties serverProperties;

	
	protected AbstractServerProperties() {

	}
	
	public String resString(ServerConnection connection,String key) {
		if (key == null)
			return "";
		if (serverProperties == null) {
			DataAccessHelper helper = new DataAccessHelper(connection);
			serverProperties = helper.getProperties(getResourceFilename());
			if (serverProperties == null) {
				return key;
			}
		}
		String value = serverProperties.getProperty(key, key);
		return (value.startsWith("!")) ? resString(connection,value.substring(1, value.length())) : value;	
	}		
	
	public abstract String getResourceFilename(); 
	
}
