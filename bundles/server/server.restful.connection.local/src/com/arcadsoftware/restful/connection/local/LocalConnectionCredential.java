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
package com.arcadsoftware.restful.connection.local;

import java.util.ArrayList;
import java.util.Date;
import java.util.GregorianCalendar;

import org.restlet.data.Language;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.rest.connection.ConnectionUserBean;
import com.arcadsoftware.rest.connection.IConnectionCredential;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.arcadsoftware.rest.connection.IPasswordComplexityTester;
import com.arcadsoftware.rest.connection.IPatchUserCredential;
import com.arcadsoftware.rest.connection.IUpdatableCredential;

public class LocalConnectionCredential implements IConnectionCredential, IUpdatableCredential, IPatchUserCredential {

	private final Activator activator;
	private final String login; // 
	private final GregorianCalendar limit = new GregorianCalendar(); // the password validity date
	private final String uniqueid; // the unique ID
	private final int id; //the auth table id. 
	private final int uid; // the userid
	private String hash; // 
	private int locked; // lock state

	public LocalConnectionCredential(Activator activator, String login, BeanMap auth) {
		super();
		this.activator = activator;
		this.login = login;
		// Prevalidation... (no more informations required)
		// Because this identification mode is the really first one (native) it is not prefixed.
		uniqueid = login.replaceAll(":", "::"); //$NON-NLS-1$ //$NON-NLS-2$
		id = auth.getId();
		uid = auth.getInt(Activator.LOCALAUTH_USERID);
		hash = auth.getString(Activator.LOCALAUTH_PASSWORD);
		// Ascendant compatibility...
		if ((hash != null) && (hash.length() < 128)) {
			hash = Crypto.hash(hash.toCharArray());
		}
		Object o = auth.get(Activator.LOCALAUTH_PWDUPDATE);
		if (o instanceof Date) {
			limit.setTime((Date) o);
		}
		locked = auth.getInt(Activator.LOCALAUTH_LOCKED);
	}
	
	@Override
	public boolean authenticate(IConnectionUserBean user, String login, char[] secret) {
		if (isLocked()) {
			return false;
		}
		if (validate(login, secret)) {
			updateLockCount(0);
			return true;
		}
		// Set up the lock count...
		updateLockCount(locked + 1);
		return false;
	}

	private boolean validate(String login, char[] password) {
		// Validate login...
		if (login == null) {
			return false;
		}
		if (activator.isCaseSensitive()) {
			if (!login.equals(this.login)) {
				return false;
			}
		} else if (!login.equalsIgnoreCase(this.login)) {
			return false;
		}
		// Validate password...
		if (password == null) {
			return true;
		}
		return Crypto.matches(hash, password);
	}

	private void updateLockCount(final int lock) {
		locked = lock;
		BeanMap auth = activator.getAuth(uid);
		if (auth != null) {
			// Cache update.
			auth.put(Activator.LOCALAUTH_LOCKED, lock);
			// Database update.
			MetaDataEntity entity = MetaDataEntity.loadEntity(Activator.LOCALAUTH);
			if ((entity != null) && (entity.getMapper() != null)) {
				entity.getMapper().update(id, entity.getAttribute(Activator.LOCALAUTH_LOCKED), lock);
				// TODO we should fire an Entity change event....
			}
		}
		if (lock == activator.getMaxLockCount()) {
			activator.recordUserLock(uid);
		}
	}
		
	private boolean isLocked() {
		return locked >= activator.getMaxLockCount();
	}

	@Override
	public String getText() {
		if (login != null) {
			return login + Messages.LocalConnectionCredential_Locallogin;
		}
		return Messages.LocalConnectionCredential_EmptyLogin;
	}

	@Override
	public String getUniqueId() {
		return uniqueid;
	}
	
	@Override
	public boolean equals(Object obj) {
		return (obj instanceof IConnectionCredential) && getUniqueId().equals(((IConnectionCredential) obj).getUniqueId());
	}

	public Date getLimit() {
		return limit.getTime();
	}

	@Override
	public boolean isOutOfDate() {
		return new GregorianCalendar().after(limit);
	}

	public int getId() {
		return id;
	}

	public int getUserId() {
		return uid;
	}

	public String getLogin() {
		return login;
	}

	public String getPasswordHash() {
		return hash;
	}

	@Override
	public int loadUserId() {
		// the user id is already loaded...
		return uid;
	}

	@Override
	public void update(IConnectionUserBean user) {
		// set transient information about this authentification...
		if (!login.equals(user.getLogin()) && (user instanceof ConnectionUserBean)) {
			((ConnectionUserBean) user).setLogin(login);
		}
		user.setCanChangePWD(true);
		user.setChangePWD(isOutOfDate());
		user.setLocked(isLocked());
	}

	@Override
	public String updatePassword(IConnectionUserBean user, char[] oldPassword, char[] newPassword, Language language) {
		// Test current password.
		if ((hash != null) && (oldPassword != null) && !Crypto.matches(hash, oldPassword)) {
			return activator.getMessage("Error_invalid_password_test", language); //$NON-NLS-1$
		}
		// Test the new password.
		int i = activator.getTester().isPasswordAcceptable(login, oldPassword, newPassword);
		if (i != IPasswordComplexityTester.REASON_OK) {
			return activator.getTester().getTextualReason(i, language);
		}
		// Update the password.
		MetaDataEntity entity = MetaDataEntity.loadEntity(Activator.LOCALAUTH);
		if ((entity == null) || (entity.getMapper() == null)) {
			return null;
		}
		String nph = Crypto.hash(newPassword);
		Date newLimit = activator.getPwdNextLimitDate();
		ArrayList<Object> values = new ArrayList<Object>(2);
		values.add(nph);
		values.add(newLimit);
		// Update the Database
		if (entity.getMapper().update(entity, id, Activator.LOCALAUTH_PASSWORD + ' ' + Activator.LOCALAUTH_PWDUPDATE, values)) {
			// Update the local authentification cache.
			BeanMap auth = activator.getAuth(uid);
			if (auth != null) {
				auth.put(Activator.LOCALAUTH_PASSWORD, nph);
				auth.put(Activator.LOCALAUTH_PWDUPDATE, newLimit);
			}
			// Update this object.
			hash = nph;
			limit.setTime(newLimit);
			// Update the connection cache.
			activator.purgeConnectionCache(uid);
			// TODO we should fire an Entity change event....
		}
		return ""; //$NON-NLS-1$
	}

	@Override
	public void patchUser(IConnectionUserBean user) {
		update(user);
		// Expire in less than six hours. (ask for password change...)
		Date d = getLimit();
		if (d != null) {
			GregorianCalendar c = new GregorianCalendar();
			c.setTimeInMillis(d.getTime() - 21600000);
			user.setChangePWD(new GregorianCalendar().after(c));
		}
	}

	@Override
	public String getUserType() {
		return "user"; //$NON-NLS-1$
	}

	@Override
	public boolean checkSecret(char[] secret) {
		if (Crypto.matches(hash, secret)) {
			// Remise à zéro du lock (connection valide).
			if ((!isLocked()) && (locked > 0)) {
				updateLockCount(0);
			}
			return true;
		}
		return false;
	}
}
