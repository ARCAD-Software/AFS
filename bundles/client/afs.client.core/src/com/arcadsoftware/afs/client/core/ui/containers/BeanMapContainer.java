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
package com.arcadsoftware.afs.client.core.ui.containers;

import java.util.ArrayList;
import java.util.List;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.ui.actions.ISecuredAction;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;

public abstract class BeanMapContainer extends AbstractConnectedContainer implements ISecuredAction {

	protected BeanMapList children;
	protected DataAccessHelper helper;

	public BeanMapContainer() {
		super(null);
	}

	public void init() {
		helper = new DataAccessHelper(getServerConnection());
		loadData();
	}

	public BeanMapContainer(AbstractConnectedContainerProvider parent) {
		super(parent);
		init();
	}

	@Override
	public void setParent(AbstractConnectedContainerProvider parent) {
		super.setParent(parent);
		init();
	}

	public BeanMapList getList() {
		return children;
	}

	@Override
	public boolean isAllowed() {
		return getServerConnection().isAllowed(getExpectedRigths());
	}

	protected void loadData() {
		if (isAllowed()) {
			final String orders = getSorOrders();
			final String attributes = getSelectClause();
			final ISearchCriteria searchClause = getSearchClause();
			if (searchClause == null) {
				if (attributes == null) {
					children = helper.getList(getType());
				} else {
					if (orders == null) {
						children = helper.getList(getType(), attributes);
					} else {
						children = helper.getList(getType(), attributes, orders);
					}
				}
			} else {
				if (attributes == null) {
					children = helper.getList(getType(), searchClause);
				} else {
					if (orders == null) {
						children = helper.getList(getType(), attributes, searchClause);
					} else {
						children = helper.getList(getType(), attributes, searchClause, orders);
					}
				}
			}
		}
	}

	protected ISearchCriteria getSearchClause() {
		return null;
	}

	protected String getSorOrders() {
		return null;
	}

	protected String getSelectClause() {
		return null;
	}

	@Override
	public Object[] getFixedChildren() {
		if (children != null) {
			final ArrayList<BeanMapItem> list = new ArrayList<>();
			for (int i = 0; i < children.size(); i++) {
				list.add(createBeanMapItem(this, children.get(i)));
			}
			return list.toArray();
		}
		return new Object[0];
	}

	// @Override
	// public Object[] getChildren() {
	// if (children!=null) {
	// ArrayList<BeanMapItem> list = new ArrayList<BeanMapItem>();
	// for (int i=0;i<children.size();i++) {
	// list.add(createBeanMapItem(this,children.get(i)));
	// }
	// return list.toArray();
	// }
	// return new Object[0];
	// }

	@Override
	public boolean hasChildren() {
		return ((children != null) && (children.size() > 0));
	}

	@Override
	public String getUniqueKey() {
		return getParent().getUniqueKey().concat("/") + getType().toUpperCase().concat("S"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	@Override
	public void refresh() {
		loadData();
		getViewer().refresh(this);
	}

	@Override
	public List<Integer> getExpectedRigths() {
		return null;
	}

	public abstract String getType();

	public abstract BeanMapItem createBeanMapItem(BeanMapContainer parent, BeanMap child);

}
