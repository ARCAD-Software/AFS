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
package com.arcadsoftware.afs.client.core.connection;

import com.arcadsoftware.metadata.client.DataAccess;
import com.arcadsoftware.rest.ServerErrorException;

public abstract class AbstractConnector {
	ServerConnection server;
	
	public AbstractConnector(ServerConnection server){
		this.server = server;
	}
		
	public ServerConnection getServerConnection(){
		return server;
	}
	
	public void manageErrorException(ServerErrorException e) {
		server.manageErrorException(e);
	}

	public DataAccess getDataAccess(){
		return server.getDataAccess();
	}
	
}
