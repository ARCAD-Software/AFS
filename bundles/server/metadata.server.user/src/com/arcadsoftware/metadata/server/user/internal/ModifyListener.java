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

import java.util.List;

import org.restlet.data.Language;
import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.IMetaDataModifyListener;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.rest.connection.IConnectionCache;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public class ModifyListener implements IMetaDataModifyListener {
	
	private final Activator activator;
	
	public ModifyListener(Activator activator) {
		super();
		this.activator = activator;
	}

	@Override
	public boolean testModification(MetaDataEntity entity, BeanMap originalItem, BeanMap modifiedItem,
			List<MetaDataAttribute> attributes, IConnectionUserBean user, Language language) throws ResourceException {
		if (activator.isUserMaxlock() && (activator.getUserMax() > 0) && // User maximal number activated,
				(originalItem == null) && // this operation is a creation (POST),
				Activator.TYPE_USER.equals(entity.getType()) && // and modified data is a user...
				(activator.getUserMax() <= entity.dataCount())) { // maximal number of undeleted user is reach
			activator.warn("Limit of maximal number of user reach ({} users).", activator.getUserMax());
			throw new ResourceException(Status.SERVER_ERROR_INSUFFICIENT_STORAGE, String.format("The current number of user declared in the application reach the fixed limitation of %d users.", activator.getUserMax()));
		}
		return true;
	}

	@Override
	public void postModification(MetaDataEntity entity, BeanMap originalItem, BeanMap modifiedItem,
			List<MetaDataAttribute> attributes, IConnectionUserBean user, Language language) throws ResourceException {
		// Purge the User cache only on update (no need to purge on creation).
		if (originalItem != null) {
			if (Activator.TYPE_USER.equals(entity.getType())) {
				//Do that only in Update mode: ie originalItem is not null
				for (IConnectionCache cache: activator.getServices(IConnectionCache.class)) {
					if (cache != null) {
						cache.purge(Activator.TYPE_USER, originalItem.getId());
					}
				}
			} else {
				if (Activator.TYPE_PROFILERIGHT.equals(entity.getType()) && (originalItem.getInt("profile") == 1)) {
					Object v = modifiedItem.get("profile");
					if ((v != null) && !v.equals(1)) {
						throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, "The Profile \"All Rights\" can not be modified.");
					}
				}
				// Other modification may relate to some users so we purge the whole cache.
				for (IConnectionCache cache: activator.getServices(IConnectionCache.class)) {
					if (cache != null) {
						cache.purgeAll(Activator.TYPE_USER);
					}
				}
			}
		}
	}

}
