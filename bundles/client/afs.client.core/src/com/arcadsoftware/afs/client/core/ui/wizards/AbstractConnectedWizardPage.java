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
package com.arcadsoftware.afs.client.core.ui.wizards;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.beanmap.BeanMap;

public abstract class AbstractConnectedWizardPage extends
		AbstractAFSSimpleItemWizardPage {

	protected ServerConnection connection;
	protected BeanMap initalBeanmap;
	protected DataAccessHelper helper;

	public AbstractConnectedWizardPage(ServerConnection connection, String pageName, String title,
			String description) {
		super(pageName, title, description);
		this.connection = connection;
		helper = new DataAccessHelper(connection);
	}

	public ServerConnection getConnection() {
		return connection;
	}

	public void setConnection(ServerConnection connection) {
		this.connection = connection;
	}

	public void initBeanMap(BeanMap beanmap) {
		initalBeanmap = beanmap;
	}

	/**
	 * Get Initial beanMap
	 *
	 * @return
	 */
	protected BeanMap getInitialBeanMap() {
		return initalBeanmap;
	}

	public void screenToBeanMap(BeanMap beanmap) {

	}

	public boolean isLinkedToCurrentBeanMap() {
		return true;
	}

}
