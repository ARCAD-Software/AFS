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
package com.arcadsoftware.afs.client.core.ui.containers;

import org.eclipse.ui.IViewPart;

import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.listeners.IBeanMapSelectionListener;
import com.arcadsoftware.afs.client.core.ui.views.AbstractConnectedView;
import com.arcadsoftware.afs.framework.ui.views.AbstractAFSView;
import com.arcadsoftware.beanmap.BeanMap;

public abstract class ViewManagedContainerProvider extends AbstractConnectedContainerProvider 
implements IOpenViewContainer{

	AbstractConnectedContainer provider;
	
	public ViewManagedContainerProvider(AbstractConnectedContainer parent) {
		super(parent);
		this.provider = parent;
		
	}

	
	public void OpenManagementView(){
		String viewId = getManagementViewId();
		
		IViewPart view = Activator.getDefault().showView(viewId);
		if (view instanceof AbstractConnectedView) {
			AbstractConnectedView connectedView = (AbstractConnectedView)view;
			connectedView.setConnection(this.getServerConnection());
			//connectedView.activate();
			doOnView(connectedView);		
		}
		if ((view instanceof IBeanMapSelectionListener) &&
			(getParent() instanceof BeanMapItem)){	
			IBeanMapSelectionListener listener = (IBeanMapSelectionListener)view;
			BeanMap beanMap = ((BeanMapItem)getParent()).getBeanMap();			
			listener.beanMapSelected(beanMap);			
		}
		
		if(view instanceof AbstractAFSView) {
			((AbstractAFSView)view).activate();
		}
	}
	
	protected void doOnView(AbstractConnectedView view) {
		
	}
	
	public boolean hasChildren() {
		return false;
	}	
	
	@Override
	public Object[] getFixedChildren() {
		return null;
	}
	
}
