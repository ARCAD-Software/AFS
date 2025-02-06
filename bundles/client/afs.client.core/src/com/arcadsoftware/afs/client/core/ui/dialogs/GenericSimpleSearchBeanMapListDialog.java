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
package com.arcadsoftware.afs.client.core.ui.dialogs;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

public class GenericSimpleSearchBeanMapListDialog extends
		AbstractSimpleSearchBeanMapListDialog {

	private final String entityType;
	private final String dialogTitle;
	private final String attributteList;
	private final String searchClause;
	private final Point size;
	private IColumnHeaderProvider columnHeaderProvider = null;

	public GenericSimpleSearchBeanMapListDialog(Shell parentShell,
			ServerConnection connection, boolean showAll,
			String entityType,
			String attributteList,
			String title,
			Point size,
			IColumnHeaderProvider columnHeaderProvider) {
		super(parentShell, connection, showAll);
		this.entityType = entityType;
		dialogTitle = title;
		this.attributteList = attributteList;
		this.size = size;
		this.columnHeaderProvider = columnHeaderProvider;
		searchClause = null;
		initialize(connection);
	}

	public GenericSimpleSearchBeanMapListDialog(Shell parentShell,
			ServerConnection connection, boolean showAll,
			String entityType,
			String attributteList,
			String searchClause,
			String title,
			Point size,
			IColumnHeaderProvider columnHeaderProvider) {
		super(parentShell, connection, showAll);
		this.entityType = entityType;
		dialogTitle = title;
		this.attributteList = attributteList;
		this.size = size;
		this.columnHeaderProvider = columnHeaderProvider;
		this.searchClause = searchClause;
		initialize(connection);
	}

	@Override
	public String getSearchClause() {
		return searchClause;
	}

	@Override
	public String getSelectClause() {
		return attributteList;
	}

	@Override
	public String getType() {
		return entityType;
	}

	@Override
	public Point getSize() {
		return size;
	}

	@Override
	public String getTitle() {
		return dialogTitle;
	}

	@Override
	protected String getUserDefineColumnHeader(String attribute) {
		if (columnHeaderProvider != null) {
			return columnHeaderProvider.getUserDefineColumnHeader(attribute);
		}
		return null;
	}

	public static BeanMapList selectList(ServerConnection connection,
			String entityType,
			String attributteList,
			String title,
			Point size) {
		return selectList(connection, entityType, attributteList, title, size, null, null);
	}

	public static BeanMapList selectList(ServerConnection connection,
			String entityType,
			String attributteList,
			String searchClause,
			String title,
			Point size) {
		return selectList(connection, entityType, attributteList, title, size, null, searchClause);
	}

	public static BeanMap select(ServerConnection connection,
			String entityType,
			String attributteList,
			String title,
			Point size) {
		return select(connection, entityType, attributteList, title, size, null);
	}

	public static BeanMapList selectList(ServerConnection connection,
			String entityType,
			String attributteList,
			String title,
			Point size,
			IColumnHeaderProvider colProvider,
			String searchClause) {
		final GenericSimpleSearchBeanMapListDialog dialog = new GenericSimpleSearchBeanMapListDialog(
				Activator.getDefault().getPluginShell(),
				connection, true,
				entityType,
				attributteList,
				searchClause,
				title,
				size,
				colProvider);
		return AbstractSearchBeanMapListDialog.selectList(dialog);
	}

	public static BeanMap select(ServerConnection connection,
			String entityType,
			String attributteList,
			String title,
			Point size,
			IColumnHeaderProvider colProvider) {
		final GenericSimpleSearchBeanMapListDialog dialog = new GenericSimpleSearchBeanMapListDialog(
				Activator.getDefault().getPluginShell(),
				connection, true,
				entityType,
				attributteList,
				title,
				size, colProvider);
		return AbstractSearchBeanMapListDialog.select(dialog);
	}

}
