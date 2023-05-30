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
package com.arcadsoftware.restful.connection.ldap;

import java.util.List;

import org.restlet.data.Language;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.IMetaDataModifyListener;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.rest.connection.ConnectionUserBean;
import com.arcadsoftware.rest.connection.IConnectionCredential;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.unboundid.ldap.sdk.LDAPConnection;
import com.unboundid.ldap.sdk.LDAPException;

public class LdapAuthModifyListener implements IMetaDataModifyListener {

	private static final String PASSWORD = "password";
	private static final String OLDPASSWORD = "oldpassword"; //$NON-NLS-1$

	private Activator activator;
	
	public LdapAuthModifyListener(Activator activator) {
		super();
		this.activator = activator;
	}

	public boolean testModification(MetaDataEntity entity, BeanMap originalItem, BeanMap modifiedItem,
			List<MetaDataAttribute> attributes, IConnectionUserBean user, Language language) throws ResourceException {
		char[] password = modifiedItem.getCharArray(PASSWORD);
		String login = modifiedItem.getString(Activator.LDAPAUTH_LOGIN);
		if ((login == null) && (password == null)) {
			return true;
		}
		if (login == null) {
			if (originalItem == null) {
				return false;
			}
			login = originalItem.getString(Activator.LDAPAUTH_LOGIN);
		}
		if (password != null) {
			attributes.remove(entity.getAttribute(PASSWORD));
		}
		if ((password != null) && (user instanceof ConnectionUserBean)) {
			List<IConnectionCredential> creds = ((ConnectionUserBean) user).getCredential(LdapConnectionCredential.class);
			if (!creds.isEmpty() && (creds.get(0) instanceof LdapConnectionCredential)) {
				final LdapAuthentificationService parent = ((LdapConnectionCredential) creds.get(0)).getParent();
				if (!parent.isAlreadybinded()) {
					activator.error("Changing other users password is not allowed through LDAP.");
					return false;
				}
				LDAPConnection cn = parent.getConnection();
				LDAPException e = null;
				try {
					String dn = parent.getUserDN(cn, login);
					if (dn == null) {
						return false; 
					}
					String error = parent.changePWD(cn, dn, modifiedItem.getCharArray(OLDPASSWORD), password);
					if (!error.isEmpty()) {
						activator.error(error);
						return false;
					}
				} catch (LDAPException ee) {
					activator.error(ee);
					e = ee;
					return false;
				} finally {
					parent.closeConnection(cn, e);
				}
			}
		}
		return true;
	}

	public void postModification(MetaDataEntity entity, BeanMap originalItem, BeanMap modifiedItem,
			List<MetaDataAttribute> attributes, IConnectionUserBean user, Language language) throws ResourceException {
		if (originalItem != null) {
			activator.purgeConnectionCache(originalItem.getId());
		}
	}
}