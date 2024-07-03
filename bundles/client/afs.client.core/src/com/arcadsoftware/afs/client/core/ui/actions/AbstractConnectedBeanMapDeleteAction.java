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
package com.arcadsoftware.afs.client.core.ui.actions;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

public abstract class AbstractConnectedBeanMapDeleteAction extends
		AbstractConnectedBeanMapAction {

	public AbstractConnectedBeanMapDeleteAction(ServerConnection connection) {
		super(connection);
	}

	public boolean beanDelete(BeanMap bean) {
		return helper.delete(bean);
	}

	@Override
	protected boolean execute() {
		final BeanMap b = getBeanMapToManage();
		setCancelled(true);
		if (b == null) {
			final BeanMapList list = getBeanMapListToManage();
			if (list != null) {
				if (Activator.getDefault().openConfirm(getTitleDeleteConfirmationMessage(),
						getDeleteConfirmationMessage())) {
					setCancelled(false);
					for (final BeanMap bean : list) {
						final boolean result = beanDelete(bean);
						if (!result) {
							return false;
						}
					}
				} else {
					return false;
				}
			}
		} else {
			if (Activator.getDefault().openConfirm(getTitleDeleteConfirmationMessage(),
					getDeleteConfirmationMessage())) {
				setCancelled(false);
				return beanDelete(b);
			} else {
				return false;
			}
		}
		return true;
	}

	protected String getDeleteConfirmationMessage() {
		return Activator.resString("action.delete.confirmation");
	}

	protected String getTitleDeleteConfirmationMessage() {
		return Activator.resString("action.delete.confirmation.title");
	}

	@Override
	protected int getActionType() {
		return IBeanMapActionListener.ACTION_DELETE;
	}
}
