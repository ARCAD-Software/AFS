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

import java.util.List;

import org.restlet.data.Language;
import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.metadata.IMetaDataModifyListener;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.arcadsoftware.rest.connection.IPasswordComplexityTester;

public class LocalAuthModifyListener implements IMetaDataModifyListener {

	private static final String OLDPASSWORD = "oldpassword"; //$NON-NLS-1$
	
	private final Activator activator;
	
	public LocalAuthModifyListener(Activator activator) {
		super();
		this.activator = activator;
	}

	public boolean testModification(MetaDataEntity entity, BeanMap originalItem, BeanMap modifiedItem,
			List<MetaDataAttribute> attributes, IConnectionUserBean user, Language language) throws ResourceException {
		// Locked can be returned with a boolean value.
		if (modifiedItem.get(Activator.LOCALAUTH_LOCKED) != null) {
			if ("true".equalsIgnoreCase(modifiedItem.getString(Activator.LOCALAUTH_LOCKED))) {  //$NON-NLS-1$
				modifiedItem.put(Activator.LOCALAUTH_LOCKED, activator.getMaxLockCount() + 1);
			} else if ("false".equalsIgnoreCase(modifiedItem.getString(Activator.LOCALAUTH_LOCKED))) {  //$NON-NLS-1$
				modifiedItem.put(Activator.LOCALAUTH_LOCKED, 0);
			}
		}
		char[] password = modifiedItem.getCharArray(Activator.LOCALAUTH_PASSWORD);
		if ((password != null) && (password.length > 0)) {
			try {
				String login = modifiedItem.getString(Activator.LOCALAUTH_LOGIN);
				if (login == null) {
					if (originalItem != null) {
						login = originalItem.getString(Activator.LOCALAUTH_LOGIN);
					}
					if (login == null) {
						throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, Activator.getMessage("Error_empty_login", language)); //$NON-NLS-1$
					}
				}
				// test to check if password is not already hashed... basic test
				if (!Crypto.isHashSecure(password)) {
					// Test the new password complexity....
					char[] oldpassword = modifiedItem.getCharArray(OLDPASSWORD);
					try {
						int i = activator.getTester().isPasswordAcceptable(login, oldpassword, password);
						if (i != IPasswordComplexityTester.REASON_OK) {
							throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, activator.getTester().getTextualReason(i, language));
						}
					} finally {
						Crypto.clear(oldpassword);
					}
					// Hash the password.
					modifiedItem.put(Activator.LOCALAUTH_PASSWORD, Crypto.hash(password));
				}
				// Mise à jour de la limite de modification du mot de passe.
				if (modifiedItem.getDate(Activator.LOCALAUTH_PWDUPDATE) == null) {
					modifiedItem.put(Activator.LOCALAUTH_PWDUPDATE, activator.getPwdNextLimitDate());
					attributes.add(entity.getAttribute(Activator.LOCALAUTH_PWDUPDATE));
				}
				// TODO Reset of Lockcount (if the password is changed) ?
			} finally {
				if (!(modifiedItem.get(Activator.LOCALAUTH_PASSWORD) instanceof char[])) {
					Crypto.clear(password);
				}
			}
		}
		return true;
	}

	public void postModification(MetaDataEntity entity, BeanMap originalItem, BeanMap modifiedItem,
			List<MetaDataAttribute> attributes, IConnectionUserBean user, Language language) throws ResourceException {
		// Remove the User from the global Connection cache.
		if ((originalItem != null) && ((modifiedItem.get(Activator.LOCALAUTH_LOGIN) != null) || (modifiedItem.get(Activator.LOCALAUTH_PASSWORD) != null))) {
			activator.purgeConnectionCache(originalItem.getInt(Activator.LOCALAUTH_USERID));
		}
		// Reset the "LocalAuth" index.
		activator.initializeAuthCache();
	}

}
