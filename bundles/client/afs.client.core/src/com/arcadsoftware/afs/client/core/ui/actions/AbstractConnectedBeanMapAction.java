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

import java.util.ArrayList;
import java.util.List;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.framework.services.IDynamicHelpIdProvider;
import com.arcadsoftware.afs.framework.ui.beanmap.actions.AbstractBeanMapAction;
import com.arcadsoftware.afs.framework.ui.help.DynamicHelp;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

public abstract class AbstractConnectedBeanMapAction extends AbstractBeanMapAction
		implements ISecuredAction, IActionActivationManager, IDynamicHelpIdProvider {

	protected DataAccessHelper helper;
	protected ServerConnection connection;
	protected List<IBeanMapActionListener> listeners;

	public AbstractConnectedBeanMapAction() {
		super();
	}

	public AbstractConnectedBeanMapAction(ServerConnection connection) {
		super();
		setConnection(connection);
		DynamicHelp.registerContextHelpId(getDynamicHelpId(), this);
	}

	public void setConnection(ServerConnection connection) {
		if (connection != null) {
			helper = new DataAccessHelper(connection);
		} else {
			helper = null;
		}
		this.connection = connection;
	}

	@Override
	protected boolean canExecute() {
		boolean result = super.canExecute();
		if (result) {
			result = isAllowed();
			if (!result) {
				Activator.getDefault().missingRight(getExpectedRigths());
			}
		}
		return result;
	}

	@Override
	public boolean isAllowed() {
		return connection.isAllowed(getExpectedRigths());
	}

	@Override
	protected void doBeforeRun() {
		super.doAfterRun();
		DynamicHelp.showContextHelpId(getDynamicHelpId());
	}

	@Override
	protected void doAfterRun() {
		super.doAfterRun();
		if (isRunOk() && !isCancelled()) {
			BeanMapList beanMaps = getBeanMapListToManage();
			if (beanMaps == null) {
				final BeanMap bm = getBeanMapToManage();
				if (bm != null) {
					beanMaps = new BeanMapList(bm);
				}
			}
			if ((listeners != null) && (beanMaps != null) && (getActionType() != IBeanMapActionListener.ACTION_NONE)) {
				for (final IBeanMapActionListener listener : listeners) {
					listener.actionDone(getActionType(), beanMaps);
				}
			}
		}
	}

	protected int getActionType() {
		return IBeanMapActionListener.ACTION_NONE;
	}

	public void addActionListener(IBeanMapActionListener listener) {
		if (listeners == null) {
			listeners = new ArrayList<>();
		}
		if (!listeners.contains(listener)) {
			listeners.add(listener);
		}
	}

	protected ServerConnection getConnection() {
		return connection;
	}

	@Override
	public boolean allowMultiSelection() {
		return true;
	}

	@Override
	public boolean isAvailable() {
		return canExecute();
	}

	/**
	 * Get Dynamic Help Id
	 *
	 * @return
	 */
	@Override
	public String getDynamicHelpId() {
		return null;
	}

}
