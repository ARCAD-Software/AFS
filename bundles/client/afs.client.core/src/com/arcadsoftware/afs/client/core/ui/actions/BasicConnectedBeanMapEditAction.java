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
package com.arcadsoftware.afs.client.core.ui.actions;

import java.util.List;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

public class BasicConnectedBeanMapEditAction extends AbstractConnectedBeanMapEditAction  {

	private BeanMap edited;
	
	public BasicConnectedBeanMapEditAction(ServerConnection connection, BeanMap edited) {
		super(connection);
		this.edited = edited;
	}

	@Override
	protected BeanMapList getBeanMapListToManage() {
		BeanMapList list = new BeanMapList();
		list.add(edited);
		return list;
	}

	public List<Integer> getExpectedRigths() {
		return null;
	}
}
