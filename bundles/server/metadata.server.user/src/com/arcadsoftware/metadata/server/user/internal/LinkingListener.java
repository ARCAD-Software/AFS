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
package com.arcadsoftware.metadata.server.user.internal;

import org.restlet.data.Language;
import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.IMetaDataLinkingListener;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.rest.connection.IConnectionCache;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public class LinkingListener implements IMetaDataLinkingListener {

	private final Activator activator;
	
	public LinkingListener(Activator activator) {
		super();
		this.activator = activator;
	}

	@Override
	public boolean testLink(MetaDataLink link, BeanMap sourceItem, BeanMap destItem, IConnectionUserBean user,
			Language language) throws ResourceException {
		purgeCache(sourceItem);
		purgeCache(destItem);
		return true;
	}

	@Override
	public boolean testUnlink(MetaDataLink link, BeanMap sourceItem, BeanMap destItem, IConnectionUserBean user,
			Language language) throws ResourceException {
		if (Activator.PROTECTADMINUSER && (sourceItem.getId() == 1) && (destItem.getId() == 1)) {
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, "The System Adminitrator can not be unlinked to the \"All Rights\" profile. The operation is aborted.");
		}
		purgeCache(sourceItem);
		purgeCache(destItem);
		return true;
	}

	private void purgeCache(BeanMap item) {
		if ((item != null) && Activator.TYPE_USER.equals(item.getType())) {
			for (IConnectionCache cache: activator.getServices(IConnectionCache.class)) {
				if (cache != null) {
					cache.purge(Activator.TYPE_USER, item.getId());
				}
			}
		}
	}
}
