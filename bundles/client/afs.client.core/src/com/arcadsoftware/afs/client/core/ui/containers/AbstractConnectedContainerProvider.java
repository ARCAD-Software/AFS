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
package com.arcadsoftware.afs.client.core.ui.containers;

import java.util.ArrayList;

import com.arcadsoftware.aev.core.ui.container.Container;
import com.arcadsoftware.aev.core.ui.container.ContainerProvider;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.framework.ui.views.AFSRootContainerInput;

public abstract class AbstractConnectedContainerProvider extends ContainerProvider
		implements ISelfSelectionManager {

	private String containerId = null;
	private ServerConnection connection = null;
	private ArrayList<AbstractConnectedContainer> childContainer;
	private int order = 0;
	private boolean fixedChildrenFirst = false;

	public AbstractConnectedContainerProvider(Container parent) {
		this(parent, false);
	}

	public AbstractConnectedContainerProvider(AbstractConnectedContainer parent) {
		this(parent, false);
	}

	public void setParent(AbstractConnectedContainer parent) {
		setParent(parent, false);
	}

	public AbstractConnectedContainerProvider(Container parent, boolean fixedChildrenFirst) {
		super(parent);
		loadContainers(fixedChildrenFirst);
	}

	public AbstractConnectedContainerProvider(AbstractConnectedContainer parent, boolean fixedChildrenFirst) {
		super(parent);
		if (parent != null) {
			setServerConnection(parent.getServerConnection());
			loadContainers(fixedChildrenFirst);
		}
	}

	public void setParent(AbstractConnectedContainer parent, boolean fixedChildrenFirst) {
		super.setParent(parent);
		setServerConnection(parent.getServerConnection());
		loadContainers(fixedChildrenFirst);
	}

	public void setServerConnection(ServerConnection connection) {
		this.connection = connection;
	}

	public ServerConnection getServerConnection() {
		return connection;
	}

	public boolean isConnected() {
		return (connection != null) && (connection.isConnected());
	}

	@Override
	public void selected() {

	}

	public void setContainerId(String containerId) {
		this.containerId = containerId;
	}

	public String getContainerId() {
		return containerId;
	}

	private void loadContainers(boolean fixedChildrenFirst) {
		this.fixedChildrenFirst = fixedChildrenFirst;
		String parentViewId = null;
		if (getRootContainerInput() instanceof AFSRootContainerInput) {
			parentViewId = ((AFSRootContainerInput) getRootContainerInput()).getParentViewId();
		}
		childContainer = new ArrayList<>();
		ConnectedContainerExtensionManager.getInstance().createChildContainers(this, childContainer, parentViewId);
	}

	private Object[] mergeChildren(Object[] kids) {
		if (kids == null) {
			kids = new Object[0];
		}

		Object[] firsts;
		Object[] seconds;

		if (fixedChildrenFirst) {
			firsts = kids;
			seconds = childContainer.toArray();
		} else {
			firsts = childContainer.toArray();
			seconds = kids;
		}

		if ((firsts.length == 0) && (seconds.length == 0)) {
			return new Object[0];
		} else if ((firsts.length != 0) && (seconds.length == 0)) {
			return firsts;
		} else if ((firsts.length == 0) && (seconds.length != 0)) {
			return seconds;
		} else {
			final Object[] result = new Object[firsts.length + seconds.length];
			System.arraycopy(firsts, 0, result, 0, firsts.length);
			System.arraycopy(seconds, 0, result, firsts.length, seconds.length);
			return result;
		}
	}

	@Override
	public Object[] getContainers() {
		return mergeChildren(getFixedChildren());
	}

	public abstract Object[] getFixedChildren();

	public int getOrder() {
		return order;
	}

	public void setOrder(int order) {
		this.order = order;
	}
}
