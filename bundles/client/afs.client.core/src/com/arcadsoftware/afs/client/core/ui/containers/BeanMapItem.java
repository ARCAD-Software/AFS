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

import java.util.List;

import com.arcadsoftware.aev.core.ui.actions.ArcadAction;
import com.arcadsoftware.afs.client.core.listeners.IBeanMapProvider;
import com.arcadsoftware.afs.client.core.ui.actions.AbstractContainerConnectedActions;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapFormater;

public abstract class BeanMapItem extends AbstractConnectedContainerProvider 
implements IBeanMapProvider{
	
	protected BeanMap beanMap;
	BeanMapFormater formatter;
	
	public BeanMapItem(AbstractConnectedContainer parent,BeanMap beanMap) {
		this(parent, beanMap, false);
	}

	public BeanMapItem(AbstractConnectedContainer parent,BeanMap beanMap, boolean fixedFirst) {
		super(parent,fixedFirst);
		this.beanMap = beanMap;
		formatter = new BeanMapFormater( getFormatString(),beanMap.getType(),false);
	}
	
	public String getUniqueKey() {
		 return getParent().getUniqueKey().concat("/") //$NON-NLS-1$
		                   .concat(beanMap.getType().toUpperCase()+":"+beanMap.getId());//$NON-NLS-1$
	}

	public BeanMap getBeanMap() {
		return beanMap;
	}
	
	public String getLabel() {
		return formatter.format(beanMap);
	}

	public abstract String getFormatString();

	public BeanMap providedBeanMap() {
		return getBeanMap();
	}
	
	public void setExtentedActions(){
		List<ArcadAction> extendedList = getExtentedActions();
		((AbstractContainerConnectedActions)this.actions).setExtentedActions(extendedList);
	}

	public List<ArcadAction> getExtentedActions() {
		return null;
	}
	
	/**
	 * Method to start BeanMapItem editor.
	 * @return true if editor exists, false otherwise.
	 */
	public boolean edit(){
		return false;
	}
	
}
