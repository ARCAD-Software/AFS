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
package com.arcadsoftware.afs.client.core.ui.actions;

import java.util.ArrayList;
import java.util.List;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.framework.ui.beanmap.actions.AbstractBeanMapAction;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;


public abstract class AbstractConnectedBeanMapGenericAction extends AbstractBeanMapAction
implements IActionActivationManager{

	protected DataAccessHelper helper;
	protected ServerConnection connection;
	protected List<IBeanMapActionListener>listeners;
	
	public AbstractConnectedBeanMapGenericAction(ServerConnection connection){
		super();
		helper = new DataAccessHelper(connection);
		this.connection = connection; 
	}

	@Override
	protected boolean execute() {
		setCancelled(false);
		BeanMap b = getBeanMapToManage();
		if (b!=null) {			
			return doOnBeanMap(b);
		} else {
			BeanMapList list = getBeanMapListToManage();
			for (BeanMap bean : list) {
				if (!doOnBeanMap(bean)){
					return false;
				}
			}
			return true;
		}
	}

	public boolean allowMultiSelection() {
		return true;
	}	
	
	public boolean isAvailable() {
		return canExecute();
	}	
	
	@Override
	protected void doAfterRun() {
		super.doAfterRun();
		if (isRunOk() && !isCancelled()){
			if (listeners != null && getActionType() != IBeanMapActionListener.ACTION_NONE){
				BeanMapList beanMaps = getActionBeanMapList();
				if (beanMaps != null) {
					for (IBeanMapActionListener listener : listeners) {
						listener.actionDone(getActionType(), beanMaps);
					}
				} else {
					BeanMap beanMap = getActionBeanMap();
					if (beanMap != null){
						for (IBeanMapActionListener listener : listeners) {
							listener.actionDone(getActionType(), beanMap);
						}
					}
				}
			}
		}
	}
	
	protected BeanMapList getActionBeanMapList(){
		return getBeanMapListToManage();
	}	
	
	protected BeanMap getActionBeanMap(){
		return getBeanMapToManage();
	}
	
	
	protected int getActionType(){
		return IBeanMapActionListener.ACTION_NONE;
	}
	
	public void addActionListener(IBeanMapActionListener listener){
		if (listeners == null){
			listeners = new ArrayList<IBeanMapActionListener>();
		}
		if (!listeners.contains(listener)){
			listeners.add(listener);
		}
	}
	
	public abstract boolean doOnBeanMap(BeanMap b);	
}
