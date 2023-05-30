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
package com.arcadsoftware.afs.framework.ui.beanmap.actions;

import com.arcadsoftware.aev.core.ui.actions.ArcadAction;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

/**
 * Define an action associated to a BeanMap or a list of BeanMap (or both).
 * 
 * <p>
 * Note that a BeanMap <b>or</b> a BeanMapList is enough to activate this action.
 * 
 * @author ARCAD Software
 */
public abstract  class AbstractBeanMapAction extends ArcadAction {
	
	@Override
	protected boolean canExecute() {
		if (getBeanMapToManage() == null) {
			final BeanMapList beanMapList = getBeanMapListToManage();
			return (beanMapList != null) && (beanMapList.size() > 0);
		}
		return true;
	}
	
	/**
	 * Override this method to associate a BeanMap to this Action
	 * @return
	 */
	protected BeanMap getBeanMapToManage() {
		return null;
	}
	
	/**
	 * Override this method to associate a BeanMapList to this Action.
	 * @return
	 */
	protected BeanMapList getBeanMapListToManage() {
		return null;
	}	
}
