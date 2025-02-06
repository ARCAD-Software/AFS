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
import com.arcadsoftware.metadata.IMetaDataDeleteListener;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public class LocalAuthDeleteListener implements IMetaDataDeleteListener {
	
	private final Activator activator;
	
	public LocalAuthDeleteListener(Activator activator) {
		super();
		this.activator = activator;
	}

	public boolean testDeletion(MetaDataEntity entity, BeanMap originalItem, IConnectionUserBean user, Language language) throws ResourceException {
		return true;
	}

	public void postDeletion(MetaDataEntity entity, BeanMap originalItem, IConnectionUserBean user, Language language) throws ResourceException {
		if ((originalItem != null) && originalItem.contains(Activator.LOCALAUTH_USERID)) {
			activator.purgeConnectionCache(originalItem.getInt(Activator.LOCALAUTH_USERID));
		}
		// Reset the "LocalAuth" index.
		activator.initializeAuthCache();
	}
}
