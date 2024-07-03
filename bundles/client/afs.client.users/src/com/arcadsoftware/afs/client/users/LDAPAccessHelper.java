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
package com.arcadsoftware.afs.client.users;

import com.arcadsoftware.afs.client.core.connection.ISRVMessages;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.afs.framework.messages.UserMessageManager;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.rest.ServerErrorException;

public class LDAPAccessHelper {

	private final ServerConnection connection;
	private UserMessage lastMessage;

	public LDAPAccessHelper(ServerConnection connection) {
		super();
		this.connection = connection;
	}

	private void manageException(Throwable e) {
		if (e instanceof ServerErrorException) {
			connection.manageErrorException((ServerErrorException) e);
			lastMessage = connection.getErrorMessage();
		} else {
			lastMessage = UserMessageManager.getInstance().getMessage(ISRVMessages.ERR_COR_EXCEPTION, e);
		}
	}

	public UserMessage getLastMessage() {
		return lastMessage;
	}

	/**
	 * Get a list of User candidate to be imported from the LDAP Server.
	 * <p>
	 * These user a "new users" that do not exist into this application data base. Each of these users contain a special
	 * attribute 'ldap.login' and all attributes that can be initialized from the LDAP Import/Export mapping.
	 *
	 * @return a Users list.
	 */
	public BeanMapList getImportCandidates() {
		return getImportCandidates(null);
	}

	public BeanMapList getImportCandidates(String filter) {
		try {
			lastMessage = null;
			String url = IUsersConsts.LDAP_IMPORT_SERVICE_GET;
			if (filter != null) {
				url += "?search=" + filter; //$NON-NLS-1$
			}
			return connection.getDataAccess().getList(url, IUsersConsts.ENTITY_USER);
		} catch (final ServerErrorException e) {
			manageException(e);
			return null;
		}
	}

	/**
	 * Import a list of users into the application.
	 * <p>
	 * The import operation is done in one call.
	 *
	 * @param userModel
	 *            contain attributes to over ride.
	 * @param profile
	 *            The profile to associate the corresponding users.
	 * @param logins
	 *            The list of logins to import.
	 * @return null if the operation is a success, if some users can not be imported the result is the details of the
	 *         error.
	 */
	public String importUsers(BeanMap userModel, int profile, String... logins) {
		return importUsers(userModel, new int[] { profile }, logins);
	}

	/**
	 * Import a list of users into the application.
	 * <p>
	 * The import operation is done in one call.
	 *
	 * @param userModel
	 *            contain attributes to over ride.
	 * @param profiles
	 *            a list of profiles to link the users to.
	 * @param logins
	 *            the list of import.
	 * @return
	 */
	public String importUsers(BeanMap userModel, int[] profiles, String... logins) {
		if ((logins == null) || (logins.length == 0)) {
			return null;
		}
		try {
			lastMessage = null;
			if ((profiles != null) && (profiles.length > 0)) {
				userModel = new BeanMap(userModel);
				final StringBuilder pr = new StringBuilder();
				for (final int p : profiles) {
					if (pr.length() > 0) {
						pr.append(' ');
					}
					pr.append(p);
				}
				userModel.put("profiles", pr.toString()); //$NON-NLS-1$
			}
			final StringBuilder l = new StringBuilder();
			for (final String login : logins) {
				if (l.length() > 0) {
					l.append('+');
				}
				l.append(login);
			}
			if (l.length() == 0) {
				return null;
			}
			connection.getDataAccess().post(IUsersConsts.LDAP_IMPORT_SERVICE_POST + l, userModel);
			return null;
		} catch (final ServerErrorException e) {
			manageException(e);
			return e.getDescription();
		}
	}

	/**
	 * Import a single user candidate.
	 *
	 * @param user
	 *            the user candidate to import.
	 * @param profiles
	 *            one or more profiles to link the user to.
	 */
	public void importUser(BeanMap user, int... profiles) {
		try {
			final String login = user.getString(IUsersConsts.LDAPIMPORT_LOGIN);
			if (login == null) {
				return;
			}
			lastMessage = null;
			if ((profiles != null) && (profiles.length > 0)) {
				user = new BeanMap(user);
				final StringBuilder pr = new StringBuilder();
				for (final int p : profiles) {
					if (pr.length() > 0) {
						pr.append(' ');
					}
					pr.append(p);
				}
				user.put("profiles", pr.toString()); //$NON-NLS-1$
			}
			connection.getDataAccess().post(IUsersConsts.LDAP_IMPORT_SERVICE_POST + login, user);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
	}
}
