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
package com.arcadsoftware.afs.client.core.ui.widgets;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.MetaDataLink;

public abstract class AbstractConnectedTableWithButtonBarSelectionSWTProvider
		extends AbstractConnectedTableWithButtonBarSWTProvider {
	@Override
	protected void createBeanMap(final MetaDataLink link, final boolean withOpenEditor) {
		if (creationAllowed()) {
			final BeanMap groupBeanMap = renderer.getCurrentBean();
			final ServerConnection connection = getConnection();
			if (connection != null) {
				BeanMap selected = selectBeanMap(connection);
				if (selected != null) {
					final boolean checkExistence = getLayoutParameters().getParameterBoolean("unique");
					if (checkExistence) {
						// test if not already exists
						final BeanMapList users = getHelper().getLinkList(groupBeanMap, link.getName(), link.getType());
						for (final BeanMap user : users) {
							if (user.getId() == selected.getId()) {
								String message = getLayoutParameters().getParameter("alreadyExistMessage");
								if ((message == null) || (message.length() == 0)) {
									message = Activator.resString("msg.error.selected.alreadyExist");
								} else {
									message = renderer.getLocalizedMessage(message);
								}
								Activator.getDefault().openError(message);
								return;
							}
						}
					}
					selected = renderer.loadBeanMap(link.getType(), selected.getId());
					renderer.addLinkitem(link, selected);
					getList().setBeanMapValue(selected);
				}
			}
		} else {
			missingRight(getExpectedAddRight());
		}
	}

	public abstract BeanMap selectBeanMap(ServerConnection connection);

}
