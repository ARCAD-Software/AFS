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

import org.eclipse.ui.IViewPart;

import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.listeners.IBeanMapProvider;
import com.arcadsoftware.afs.client.core.ui.listeners.IBeanMapSelectionListener;
import com.arcadsoftware.afs.client.core.ui.views.AbstractConnectedView;
import com.arcadsoftware.afs.framework.ui.views.AbstractAFSView;
import com.arcadsoftware.beanmap.BeanMap;

public abstract class ViewManagedContainer extends AbstractConnectedContainer
		implements IOpenViewContainer {

	AbstractConnectedContainerProvider provider;
	private IBeanMapProvider beanMapProvider = null;

	public ViewManagedContainer(AbstractConnectedContainerProvider parent) {
		super(parent);
		provider = parent;
		if (parent instanceof IBeanMapProvider) {
			beanMapProvider = (IBeanMapProvider) parent;
		}
	}

	@Override
	public void setParent(AbstractConnectedContainerProvider parent) {
		super.setParent(parent);
		provider = parent;
		if (parent instanceof IBeanMapProvider) {
			beanMapProvider = (IBeanMapProvider) parent;
		}
	}

	@Override
	public void setParent(AbstractConnectedContainer parent) {
		super.setParent(parent);
		if (parent instanceof IBeanMapProvider) {
			beanMapProvider = (IBeanMapProvider) parent;
		}
	}

	public void setBeanMapProvider(IBeanMapProvider beanMapProvider) {
		this.beanMapProvider = beanMapProvider;
	}

	public IBeanMapProvider getBeanMapProvider() {
		return beanMapProvider;
	}

	@Override
	public void OpenManagementView() {
		// String viewId = getManagementViewId();
		//
		// IViewPart view = Activator.getDefault().showView(viewId);
		// if (view instanceof AbstractConnectedView) {
		// ((AbstractConnectedView)view).setConnection(this.getServerConnection());
		// ((AbstractConnectedView)view).activate();
		// doOnView((AbstractConnectedView)view);
		// }
		// if ((view instanceof IBeanMapSelectionListener) &&
		// (beanMapProvider!=null)){
		// IBeanMapSelectionListener listener = (IBeanMapSelectionListener)view;
		// BeanMap beanMap = beanMapProvider.providedBeanMap();
		// listener.beanMapSelected(beanMap);
		// }
		//
		//
		final String viewId = getManagementViewId();
		final IViewPart view = Activator.getDefault().showView(viewId);
		if (view instanceof AbstractConnectedView) {
			final AbstractConnectedView connectedView = (AbstractConnectedView) view;
			connectedView.setConnection(getServerConnection());
			// connectedView.activate();
			doOnView(connectedView);
		}
		if ((view instanceof IBeanMapSelectionListener) &&
				(beanMapProvider != null)) {
			final IBeanMapSelectionListener listener = (IBeanMapSelectionListener) view;
			final BeanMap beanMap = beanMapProvider.providedBeanMap();

			if (view instanceof IPropertyListListener) {
				final IPropertyListListener propertyListener = (IPropertyListListener) view;
				propertyListener.setProperties(getProps());
			}
			listener.beanMapSelected(beanMap);
		}

		if (view instanceof AbstractAFSView) {
			((AbstractAFSView) view).activate();
		}

	}

	protected void doOnView(AbstractConnectedView view) {

	}

	@Override
	public boolean hasChildren() {
		return false;
	}

	@Override
	public void refresh() {

	}

	@Override
	public Object[] getFixedChildren() {
		return null;
	}

}
