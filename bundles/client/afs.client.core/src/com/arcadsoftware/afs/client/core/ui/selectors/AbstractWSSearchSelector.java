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
package com.arcadsoftware.afs.client.core.ui.selectors;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Hashtable;
import java.util.Set;

import org.eclipse.swt.graphics.Image;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.dialogs.GenericBeanMapListSelectorDialog;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.client.editors.swtwidgets.widgets.ISearchBeanMap;
import com.arcadsoftware.metadata.MetaDataEntity;

public abstract class AbstractWSSearchSelector implements ISearchBeanMap {

	private final ServerConnection connection;

	private final String wspath;
	private final Hashtable<String, String> parameters;

	public AbstractWSSearchSelector(ServerConnection connection, String wspath, Hashtable<String, String> parameters) {
		this.connection = connection;
		this.wspath = wspath;
		this.parameters = parameters;
	}

	@Override
	public BeanMap search(MetaDataEntity structure) {
		final DataAccessHelper helper = new DataAccessHelper(connection);
		final StringBuilder path = new StringBuilder(wspath);
		if (parameters.size() > 0) {
			path.append("?");
			final StringBuilder parameterString = new StringBuilder("");
			final Set<String> keySet = parameters.keySet();
			for (final String key : keySet) {
				final String value = parameters.get(key);
				if (value != null) {
					if (parameterString.length() > 0) {
						parameterString.append("&");
					}
					parameterString.append(key).append("=").append(value);
				}
			}
			path.append(parameterString);
		}

		final BeanMapList result = helper.getListFromPath(path.toString(), getType());
		if (((result == null) || result.isEmpty()) && (helper.getLastMessage() != null)) {
			String message = helper.getLastMessage().toString();
			if (!message.trim().isEmpty()) {
				if (helper.getLastCause() != null) {
					final StringWriter sw = new StringWriter();
					helper.getLastCause().printStackTrace(new PrintWriter(sw));
					message += '\n' + sw.toString();
				}
				Activator.getDefault().openError(message);
				return null;
			}
		}
		return select(connection, result);
	}

	public BeanMap select(ServerConnection connection, BeanMapList content) {
		return GenericBeanMapListSelectorDialog.select(connection, content, false, getType(), getAttributeList(),
				getTitle(), getElementIcon());
	}

	public Image getElementIcon() {
		return null;
	}

	public abstract String getType();

	public abstract String getAttributeList();

	public abstract String getTitle();

}
