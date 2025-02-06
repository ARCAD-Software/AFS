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
package com.arcadsoftware.afs.client.core.ui.editors;

import java.util.Hashtable;
import java.util.Map;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.listeners.IBeanMapModifier;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.swt.BeanMapEditorInput;

public class ConnectedBeanMapInput extends BeanMapEditorInput {

	private ServerConnection connection;
	private final Map<String, Object> virtualValues;

	private IBeanMapModifier modifier;

	public ConnectedBeanMapInput(ServerConnection connection, BeanMap beanMap) {
		this(connection, beanMap, null);
	}

	public ConnectedBeanMapInput(ServerConnection connection, BeanMap beanMap, String layoutName) {
		super(beanMap, layoutName);
		this.connection = connection;
		virtualValues = new Hashtable<>();
	}

	public ServerConnection getConnection() {
		return connection;
	}

	public void setConnection(ServerConnection connection) {
		this.connection = connection;
	}

	public Map<String, Object> getVirtualValues() {
		return virtualValues;
	}

	public void setVirtualValues(Map<String, Object> virtualValues) {
		if (virtualValues != null) {
			this.virtualValues.putAll(virtualValues);
		}
	}

	public void addVirtualValues(String key, String value) {
		virtualValues.put(key, value);
	}

	public IBeanMapModifier getModifier() {
		return modifier;
	}

	public void setModifier(IBeanMapModifier modifier) {
		this.modifier = modifier;
	}

	@Override
	public boolean equals(Object obj) {
		if ((obj instanceof BeanMapEditorInput) && (((BeanMapEditorInput) obj).getBeanMap() != null)) {
			if (obj instanceof ConnectedBeanMapInput) {
				final ConnectedBeanMapInput newInput = (ConnectedBeanMapInput) obj;
				final BeanMap newBeanmap = ((ConnectedBeanMapInput) obj).getBeanMap();

				if ((newBeanmap != null) && (getBeanMap() != null) && (connection != null)
						&& (newInput.getConnection() != null)) {
					if (connection.equals(newInput.getConnection())) {
						return getBeanMap().equals(newBeanmap);
					} else {
						return false;
					}
				}
			} else {
				return ((BeanMapEditorInput) obj).getBeanMap().equals(getBeanMap());
			}
		}
		return super.equals(obj);
	}
}
