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
package com.arcadsoftware.restful.connection.local;

import org.restlet.data.Language;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.IMetaDataSelectionListener;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public class LocalAuthSelectionListener implements IMetaDataSelectionListener {

	private final Activator activator;

	public LocalAuthSelectionListener(Activator activator) {
		super();
		this.activator = activator;
	}
	
	@Override
	public void onSelection(MetaDataEntity entity, BeanMapList selectedItems, IConnectionUserBean user,
			Language language) throws ResourceException {
		for (BeanMap bean: selectedItems) {
			if (bean.get(Activator.LOCALAUTH_LOCKED) != null) {
				bean.put(Activator.LOCALAUTH_LOCKED, bean.getInt(Activator.LOCALAUTH_LOCKED) > activator.getMaxLockCount());
			}
		}
	}

}
