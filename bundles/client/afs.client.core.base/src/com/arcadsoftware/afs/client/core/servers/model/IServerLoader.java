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
package com.arcadsoftware.afs.client.core.servers.model;

public interface IServerLoader {

	/**
	 * Load the server connection list already registered to this Client.
	 *
	 * @return
	 */
	public Servers load();

	/**
	 * Register a new server connection.
	 *
	 * @param server
	 * @return
	 */
	public boolean add(Server server);

	/**
	 * Unregister a server connection.
	 *
	 * @param server
	 * @return
	 */
	public boolean delete(Server server);

	/**
	 * Update the registration of a Server connection.
	 *
	 * @param server
	 * @return
	 */
	public boolean update(Server server);
}
