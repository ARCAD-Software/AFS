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

import java.util.ArrayList;
import java.util.Properties;

import com.arcadsoftware.aev.core.ui.container.Container;
import com.arcadsoftware.aev.core.ui.container.IContainer;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.framework.ui.views.AFSRootContainerInput;

public abstract class AbstractConnectedContainer extends Container 
implements ISelfSelectionManager{

	private ServerConnection connection = null;

	private String containerId = null;
	private int order = 0;
	private Properties props = null;
	
	//private ArrayList<AbstractConnectedContainerProvider> childContainerProviders;
	//private ArrayList<AbstractConnectedContainer> childContainers;
	private ArrayList<IContainer> childContainers;
	
	
	public AbstractConnectedContainer(AbstractConnectedContainerProvider parent) {
		super(parent);
		if (parent!=null){
			setServerConnection(parent.getServerConnection());
			loadContainers();
		}
		
	}

	public void setParent(AbstractConnectedContainer parent) {
		super.setParent(parent);
		setServerConnection(parent.getServerConnection());
		loadContainers();
	}
	
	public void setParent(AbstractConnectedContainerProvider parent) {
		super.setParent(parent);
		setServerConnection(parent.getServerConnection());
		loadContainers();
	}
	
	public void setServerConnection(ServerConnection connection){
		this.connection = connection;
	}	
	
	public ServerConnection getServerConnection() {
		return connection;
	}
	
	public boolean isConnected(){
		return (connection!=null) && (connection.isConnected());
	}	

	public void selected(){
		
	}	
	
	private void loadContainers(){
		String parentViewId = null;
		if (this.getRootContainerInput() instanceof AFSRootContainerInput) {
			parentViewId = ((AFSRootContainerInput)getRootContainerInput()).getParentViewId();
		}
		//childContainerProviders = new ArrayList<AbstractConnectedContainerProvider>();
		childContainers = new ArrayList<IContainer>();
		ConnectedContainerExtensionManager.getInstance().createChildContainers(this, childContainers, parentViewId);
		
		
//		childContainers = new ArrayList<AbstractConnectedContainer>();
//		ConnectedContainerExtensionManager.getInstance().createChildContainers(
//				(AbstractConnectedContainerProvider)this.getParent(), childContainers, getContainerId(), parentViewId);
	}

	public void setContainerId(String containerId){
		this.containerId=containerId;
	}
	
	public String getContainerId(){
		return containerId;
	}
	
	

	private Object[] mergeChildren(Object[] kids){
		if (kids==null)
			kids = new Object[0]; 
		//Object[] supers = childContainerProviders.toArray();
		Object[] containers;
		if (childContainers != null)
			containers = childContainers.toArray();
		else
			containers = new Object[0];
		
		if ((kids.length==0) && (containers.length==0)){
			return new Object[0];
		} else {
			Object[] result = new Object[kids.length+containers.length];
			//System.arraycopy(supers, 0, result, 0, supers.length);
			System.arraycopy(kids, 0, result, 0, kids.length);
			System.arraycopy(containers, 0, result, kids.length, containers.length);
			return result;
		}
	}
	
	public int getOrder() {
		return order;
	}
	
	public void setOrder(int order) {
		this.order = order;
	}
	
	public Object[] getChildren() {
		return mergeChildren(getFixedChildren());
	}
	
	public void setProps(Properties props) {
		this.props = props;
	}
	public Properties getProps() {
		return props;
	}
	
	public String getProperty(String key){
		if (props!=null) {
			return props.getProperty(key);
		}
		return null;
	}
	
	
	public abstract Object[] getFixedChildren();
	
}
