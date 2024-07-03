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
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

public class GenericBeanMapListSelectorDialog extends AbstractBeanMapListSelectorDialog {

	private final String type;
	private final String attributeList;
	private final String title;
	private final Image icon;
	final BeanMapList contentList = null;

	public GenericBeanMapListSelectorDialog(Shell parentShell,
			ServerConnection connection, boolean multiselection,
			String type, String attributeList, String title, Image icon) {
		super(parentShell, connection, multiselection, false);
		this.type = type;
		this.attributeList = attributeList;
		this.title = title;
		this.icon = icon;
		initialize(connection);
	}

	@Override
	public String getType() {
		return type;
	}

	@Override
	public String getAttributeList() {
		return attributeList;
	}

	@Override
	public BeanMapList getInput() {
		return contentList;
	}

	@Override
	public Point getSize() {
		return new Point(600, 500);
	}

	@Override
	public String getTitle() {
		return title;
	}

	@Override
	public Image getElementIcon() {
		return icon;
	}

	public static BeanMap select(ServerConnection connection, final BeanMapList content, boolean multiselection,
			String type, String attributeList, String title, Image icon) {
		final GenericBeanMapListSelectorDialog dialog = new GenericBeanMapListSelectorDialog(
				Activator.getDefault().getPluginShell(), connection, false, type, attributeList, title, icon) {
			@Override
			public BeanMapList getInput() {
				return content;
			}
		};
		return AbstractBeanMapListSelectorDialog.select(dialog);
	}

}
