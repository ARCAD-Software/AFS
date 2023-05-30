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
package com.arcadsoftware.afs.client.server;

import org.eclipse.swt.graphics.Image;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;

/**
 * Use to get Application Server Information
 * @author ARCAD Software
 *
 */
public interface IServerVersionProvider {
	public String getServerVersion();
	public String getServerDescription();
	public void setServerConnection(ServerConnection connection);
	public Image getDialogImage();
	public String getDialogTitle();
}
