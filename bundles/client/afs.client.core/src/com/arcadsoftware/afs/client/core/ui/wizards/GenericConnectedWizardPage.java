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
package com.arcadsoftware.afs.client.core.ui.wizards;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;

public class GenericConnectedWizardPage extends AbstractConnectedLayoutWizardPage {

	private String type =null;
	private String layoutName =null;
	
	public GenericConnectedWizardPage(ServerConnection connection,
			String pageName, String title, String description,
			String type, String layoutName) {
		super(connection, pageName, title, description);
		this.type = type;
		this.layoutName = layoutName;
	}

	@Override
	public String getType() {
		return type;
	}

	@Override
	public String getLayoutName() {
		return layoutName;
	}

}
