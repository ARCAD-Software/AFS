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
package com.arcadsoftware.metadata.server.user.internal;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.IMetaDataDeleteListener;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.rest.connection.IConnectionCache;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import org.restlet.data.Language;
import org.restlet.resource.ResourceException;

public class DeleteListener implements IMetaDataDeleteListener {

	private final Activator activator;
	
	public DeleteListener(Activator activator) {
		super();
		this.activator = activator;
	}

	@Override
	public boolean testDeletion(MetaDataEntity entity, BeanMap originalItem, IConnectionUserBean user, Language language) throws ResourceException {
		return true;
	}

	@Override
	public void postDeletion(MetaDataEntity entity, BeanMap originalItem, IConnectionUserBean user, Language language)
			throws ResourceException {
		if (Activator.TYPE_USER.equals(entity.getType())) {
			for (IConnectionCache cache: activator.getServices(IConnectionCache.class)) {
				if (cache != null) {
					cache.purge(Activator.TYPE_USER, originalItem.getId());
				}
			}
		} else {
			for (IConnectionCache cache: activator.getServices(IConnectionCache.class)) {
				if (cache != null) {
					cache.purgeAll(Activator.TYPE_USER);
				}
			}
		}
	}
}