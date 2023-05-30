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
package com.arcadsoftware.afs.client.core.ui.composites;

import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.aev.core.ui.composite.AbstractBasicTableManagerComposite;
import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;

public abstract class AbstractConnectedSimpleTableManagerComposite extends AbstractBasicTableManagerComposite{

	protected ServerConnection connection;
	protected DataAccessHelper helper;

	
	public AbstractConnectedSimpleTableManagerComposite(Composite parent,
			int style,ServerConnection serverConnection) {
		super(parent, style);
		this.connection = serverConnection;
		this.helper = new DataAccessHelper(serverConnection);			
	}

	 

}
