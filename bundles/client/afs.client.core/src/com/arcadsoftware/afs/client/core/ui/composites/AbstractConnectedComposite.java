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
package com.arcadsoftware.afs.client.core.ui.composites;

import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.framework.ui.composites.AbstractAFSComposite;

/**
 * This class provides a composite in a connected context.<br>
 * First, Create the composite giving a serverConnection<br>
 *
 * @author ARCAD Software
 */
public abstract class AbstractConnectedComposite extends AbstractAFSComposite {

	protected ServerConnection connection;
	protected DataAccessHelper helper;

	public AbstractConnectedComposite(Composite parent, int style,
			ServerConnection connection) {
		this(parent, style, connection, true);
	}

	public AbstractConnectedComposite(Composite parent, int style,
			ServerConnection connection, boolean init) {
		super(parent, style);
		this.connection = connection;
		helper = new DataAccessHelper(connection);
		format();
		if (init) {
			createContent(this);
		}
	}

	protected void format() {
		final GridLayout gridLayout = new GridLayout(3, false);
		setLayout(gridLayout);
	}

	public ServerConnection getConnection() {
		return connection;
	}

	public DataAccessHelper getHelper() {
		return helper;
	}

	/**
	 * Creates the content of the composite
	 *
	 * @param parent
	 *            the composite itself
	 */
	public abstract void createContent(Composite parent);

}
