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
import com.arcadsoftware.beanmap.BeanMap;

/**
 * This class provides a composite to display/populate a BeanMap in a connected context.<br>
 * First, Create the composite giving a serverConnection<br>
 * then, call the {@link #init(BeanMap)} method to define the related BeanMap. This method automatically populates the
 * content of the composite by calling the {@link toComposite()} method.
 *
 * @author ARCAD Software
 */
public abstract class AbstractConnectBeanMapComposite extends AbstractAFSComposite {

	private final ServerConnection connection;
	private final DataAccessHelper helper;

	public AbstractConnectBeanMapComposite(Composite parent, int style,
			ServerConnection connection) {
		super(parent, style);
		this.connection = connection;
		helper = new DataAccessHelper(connection);
		format();
		createContent(this);
	}

	protected void format() {
		final GridLayout gridLayout = new GridLayout(3, false);
		setLayout(gridLayout);
	}

	public void init(BeanMap beanmap) {
		if (beanmap == null) {
			toComposite(beanmap);
		}
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

	/**
	 * Defines how to transfer the content of the beanmap to the composite childs.
	 *
	 * @param beanmap
	 */
	public abstract void toComposite(BeanMap beanmap);

	/**
	 * Defines how to transfer the content of the composite items th the beanMap
	 *
	 * @param beanmap
	 */
	public abstract void fromComposite(BeanMap beanmap);

}
