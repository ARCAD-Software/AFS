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
package com.arcadsoftware.restful.connection.config.internal;

import org.restlet.data.Language;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.rest.connection.AutoProfile;
import com.arcadsoftware.rest.connection.ConnectionUserBean;
import com.arcadsoftware.rest.connection.IConnectionCredential;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.arcadsoftware.rest.connection.IPatchUserCredential;
import com.arcadsoftware.rest.connection.IUpdatableCredential;
import com.arcadsoftware.rest.connection.Profile;

public class ConnectionCredential implements IConnectionCredential, IUpdatableCredential, IPatchUserCredential {

	private Activator activator;
	private String login;
	private int id;
	private String hash;
	
	public ConnectionCredential(Activator activator, String login, int id, String hash) {
		super();
		this.activator = activator;
		this.login = login;
		this.id = id;
		this.hash = hash;
	}

	@Override
	public String getText() {
		return login + Messages.ConnectionCredential_Text;
	}

	@Override
	public boolean authenticate(IConnectionUserBean user, String login, char[] secret) {
		return login.equals(login) && Crypto.matches(hash, secret);
	}

	@Override
	public String getUniqueId() {
		return '(' + login + ")@config"; //$NON-NLS-1$
	}

	@Override
	public boolean isOutOfDate() {
		return false;
	}

	@Override
	public int loadUserId() {
		return id;
	}

	@Override
	public void update(IConnectionUserBean user) {
		user.setCanChangePWD(true);
		if ((user instanceof ConnectionUserBean) && (user.getId() < 0)) {
			((ConnectionUserBean) user).setFullname(login);
			((ConnectionUserBean) user).setPrincipal(0);
			((ConnectionUserBean) user).setLogin(login);
			if ((id >= -1) && (ConnectionUserBean.STANDALONECONNECTIONS != null)) {
				((ConnectionUserBean) user).setProfile(new Profile(user, ConnectionUserBean.STANDALONECONNECTIONS));
			} else {
				((ConnectionUserBean) user).setProfile(new AutoProfile(id, user));
			}
		}
	}

	@Override
	public String getUserType() {
		if (id < 0) {
			return null;
		}
		return "user"; //$NON-NLS-1$
	}

	@Override
	public String updatePassword(IConnectionUserBean user, char[] oldPassword, char[] newPassword, Language language) throws ResourceException {
		if ((newPassword == null) || !Crypto.matches(hash, oldPassword)) {
			return Messages.ConnectionCredential_Error_InvalidPassword;
		}
		hash = Crypto.hash(newPassword);
		activator.setConnectionCredentials(login, id, hash);
		return ""; //$NON-NLS-1$
	}

	@Override
	public void patchUser(IConnectionUserBean user) {
		update(user);
	}

	@Override
	public boolean checkSecret(char[] secret) {
		return (hash != null) && Crypto.matches(hash, secret);
	}

}
