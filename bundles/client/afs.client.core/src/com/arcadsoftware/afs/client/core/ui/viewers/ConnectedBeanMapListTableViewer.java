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
package com.arcadsoftware.afs.client.core.ui.viewers;

import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.tools.MetadataUtils;
import com.arcadsoftware.afs.framework.ui.viewers.BeanMapListTableViewer;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;

public class ConnectedBeanMapListTableViewer extends BeanMapListTableViewer {
	private final ServerConnection connection;
	private final DataAccessHelper helper;

	public ConnectedBeanMapListTableViewer(ServerConnection connection, Composite parent, int style,
			MetaDataEntity entity, String attributeList) {
		super(parent, style);
		this.connection = connection;
		helper = new DataAccessHelper(connection);
		initialize(entity, attributeList);
	}

	@Override
	protected int getColumnSize(String attribute) {
		final MetaDataAttribute metaDataAttribute = MetadataUtils.getInstance().resolveMetaDataAttribute(helper, entity,
				attribute);
		if (metaDataAttribute != null) {
			return metaDataAttribute.getColSize();
		} else {
			return super.getColumnSize(attribute);
		}
	}

	@Override
	protected String getColumnHeader(String attribute) {
		final MetaDataAttribute metaDataAttribute = MetadataUtils.getInstance().resolveMetaDataAttribute(helper, entity,
				attribute);
		if (metaDataAttribute != null) {
			return metaDataAttribute.getName();
		} else {
			return super.getColumnHeader(attribute);
		}

	}

	public ServerConnection getConnection() {
		return connection;
	}

}
