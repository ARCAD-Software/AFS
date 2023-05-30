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
package com.arcadsoftware.afs.client.core.model;

import com.arcadsoftware.aev.core.model.ArcadEntity;
import com.arcadsoftware.afs.client.core.listeners.IBeanMapProvider;
import com.arcadsoftware.beanmap.BeanMap;

public class BeanMapCollectionItem extends  ArcadEntity implements IBeanMapProvider{

	private BeanMap beanMap;
	
	public BeanMapCollectionItem( BeanMap beanMap) {
		 this.beanMap = beanMap;
	}
	
	public String getLabel() {
		return null;
	}

	public BeanMap getBeanMap(){
		return beanMap;
	}

	@Override
	public BeanMap providedBeanMap() {
		return beanMap;
	}

}
