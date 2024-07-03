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
package com.arcadsoftware.afs.client.core.ui.views;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.framework.ui.help.DynamicHelp;
import com.arcadsoftware.afs.framework.ui.views.AbstractAFSView;

public class AbstractConnectedView extends AbstractAFSView {

	private ServerConnection connection;
	private DataAccessHelper helper;

	/**
	 * @return may return null.
	 */
	public ServerConnection getConnection() {
		return connection;
	}

	/**
	 * @param connection
	 */
	public void setConnection(ServerConnection connection) {
		this.connection = connection;
		if (connection != null) {
			helper = new DataAccessHelper(connection);
		} else {
			helper = null;
		}
		connectionChanged(connection);
	}

	/**
	 * This method is called each time {@link #setConnection(ServerConnection)} is called.
	 *
	 * @param connection
	 *            may be null if the connection is undone.
	 */
	protected void connectionChanged(ServerConnection connection) {
	}

	/**
	 * Help Context Id
	 *
	 * @return
	 */
	protected String getDynamicHelpId() {
		return null;
	}

	@Override
	public void setFocus() {
		super.setFocus();
		DynamicHelp.showContextHelpId(getDynamicHelpId());
	}

	/**
	 * Get access to the connection facade.
	 *
	 * @return may be null if no connection is set.
	 */
	public DataAccessHelper getHelper() {
		return helper;
	}
}
