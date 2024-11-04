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
package com.arcadsoftware.afs.client.server.ui.views;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.ui.views.properties.IPropertySheetPage;
import org.eclipse.ui.views.properties.tabbed.ITabbedPropertySheetPageContributor;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;

import com.arcadsoftware.aev.core.ui.container.Container;
import com.arcadsoftware.aev.core.ui.container.IContainer;
import com.arcadsoftware.aev.core.ui.viewers.columned.impl.ColumnedTreeViewer;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.core.BeanMapSelectionManager;
import com.arcadsoftware.afs.client.core.ui.containers.BeanMapItem;
import com.arcadsoftware.afs.client.core.ui.containers.IOpenViewContainer;
import com.arcadsoftware.afs.client.core.ui.containers.ISelfSelectionManager;
import com.arcadsoftware.afs.client.server.internals.Activator;
import com.arcadsoftware.afs.client.server.ui.containers.ServerContainer;
import com.arcadsoftware.afs.client.server.ui.containers.ServerItem;
import com.arcadsoftware.afs.framework.ui.containers.viewer.ContainerTreeViewer;
import com.arcadsoftware.afs.framework.ui.views.AbstractNavigatorView;
import com.arcadsoftware.beanmap.BeanMap;

public abstract class AbstractConnectedNavigationView extends AbstractNavigatorView
		implements ITabbedPropertySheetPageContributor {

	public static final String PROPERTY_CONTRIBUTOR_ID = "com.arcadsoftware.afs.client.server.view.navigation.propertyId"; //$NON-NLS-1$

	private ServerContainer serverContainer;
	private Action refreshAction;

	@Override
	public Object[] getRootChildren(Container root) {
		if (serverContainer == null) {
			serverContainer = getServerContainer(root);
		}
		return new Object[] { serverContainer };
	}

	protected ServerContainer getServerContainer(Container parent) {
		return new ServerContainer(parent, true);
	}

	@Override
	protected void viewerCreated(ContainerTreeViewer viewer) {
		getSite().setSelectionProvider(viewer.getViewer());
		((ColumnedTreeViewer) viewer.getViewer()).expandAll();
	}

	@Override
	protected void defineActions() {
		refreshAction = new Action() {
			@Override
			public void run() {
				final IContainer c = viewer.getSelectedElement();
				viewer.refresh(c);
			}
		};
		refreshAction.setText(Activator.resString("action.refresh.text")); //$NON-NLS-1$
		refreshAction.setToolTipText(Activator.resString("action.refresh.text.tooltip")); //$NON-NLS-1$
		refreshAction.setImageDescriptor(AFSIcon.REFRESH.imageDescriptor());
	}

	@Override
	protected void fillFixedContainerAction(IMenuManager manager) {
		super.fillFixedContainerAction(manager);
		manager.add(new Separator());
		manager.add(refreshAction);
	}

	@Override
	protected void doubleClickOnContainer(IContainer o) {
		if (o instanceof ServerItem) {
			final ServerItem item = ((ServerItem) o);
			item.connect();
			if (item.isConnected()) {
				doOnConnect(item);
			}
		}
	}

	@Override
	protected void containerSelected(IContainer o) {
		if (o instanceof ISelfSelectionManager) {
			((ISelfSelectionManager) o).selected();
		}
		if (o instanceof IOpenViewContainer) {
			final IOpenViewContainer c = (IOpenViewContainer) o;
			c.OpenManagementView();
		}
		if (o instanceof BeanMapItem) {
			final BeanMap selected = ((BeanMapItem) o).getBeanMap();
			BeanMapSelectionManager.getInstance().fireBeanMapSelection(selected);
		}
	}

	@Override
	public String getContributorId() {
		return PROPERTY_CONTRIBUTOR_ID;
	}

	@Override
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public Object getAdapter(Class adapter) {
		if (adapter == IPropertySheetPage.class) {
			return new TabbedPropertySheetPage(this);
		}
		return super.getAdapter(adapter);
	}

	public void doOnConnect(ServerItem item) {}

}
