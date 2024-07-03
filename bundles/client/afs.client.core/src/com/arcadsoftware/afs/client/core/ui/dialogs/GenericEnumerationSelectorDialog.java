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
package com.arcadsoftware.afs.client.core.ui.dialogs;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.framework.ui.images.ImageManager;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

public class GenericEnumerationSelectorDialog extends AbstractBeanMapListSelectorDialog {

	private final String entityType;
	private final String dialogTitle;
	private final String attributeList;
	private final String iconKey;

	public GenericEnumerationSelectorDialog(Shell parentShell,
			ServerConnection connection, boolean multiselection,
			String type,
			String attributList,
			String title,
			String iconKey) {
		super(parentShell, multiselection);
		entityType = type;
		attributeList = attributList;
		dialogTitle = title;
		this.iconKey = iconKey;
		initialize(connection);
	}

	@Override
	public BeanMapList getInput() {
		final BeanMapList list = helper.getList(getType(), getAttributeList());
		return list;
	}

	@Override
	public Point getSize() {
		return new Point(600, 500);
	}

	@Override
	public String getType() {
		return entityType;
	}

	@Override
	public String getAttributeList() {
		return attributeList;
	}

	@Override
	public String getTitle() {
		return dialogTitle;
	}

	@Override
	public Image getElementIcon() {
		if (iconKey != null) {
			return ImageManager.getInstance().getImage(iconKey);
		}
		return super.getElementIcon();
	}

	public static BeanMap select(ServerConnection connection,
			String type,
			String attributList,
			String title,
			String iconKey) {
		final GenericEnumerationSelectorDialog dialog = new GenericEnumerationSelectorDialog(
				Activator.getDefault().getPluginShell(),
				connection, false,
				type, attributList,
				title,
				iconKey);
		return AbstractBeanMapListSelectorDialog.select(dialog);
	}

	public static BeanMap select(ServerConnection connection,
			String type,
			String title,
			String iconKey) {
		final GenericEnumerationSelectorDialog dialog = new GenericEnumerationSelectorDialog(
				Activator.getDefault().getPluginShell(),
				connection, false,
				type, "code name", title, iconKey);
		return AbstractBeanMapListSelectorDialog.select(dialog);
	}

}
