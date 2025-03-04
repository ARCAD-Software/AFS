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
package com.arcadsoftware.restful.connection.ldap;

import org.restlet.data.Language;

import com.arcadsoftware.rest.connection.AutoProfile;
import com.arcadsoftware.rest.connection.ConnectionUserBean;
import com.arcadsoftware.rest.connection.IConnectionCredential;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.arcadsoftware.rest.connection.IUpdatableCredential;
import com.arcadsoftware.rest.connection.Profile;
import com.unboundid.ldap.sdk.BindResult;
import com.unboundid.ldap.sdk.LDAPConnection;
import com.unboundid.ldap.sdk.LDAPException;
import com.unboundid.ldap.sdk.ResultCode;
import com.unboundid.ldap.sdk.controls.PasswordExpiredControl;
import com.unboundid.ldap.sdk.controls.PasswordExpiringControl;

public class LdapConnectionCredential implements IConnectionCredential, IUpdatableCredential {

	private final LdapAuthentificationService parent;
	private final String login;
	private final String uid;
	private final int userId;
	private boolean locked;
	private boolean outOfDate;
	private char[] secret;
	
	public LdapConnectionCredential(LdapAuthentificationService parent, String login, int userId) {
		super();
		this.parent = parent;
		this.login = login;
		if (parent.isLoginCaseSensitive()) {
			uid = "ldap:" + login; //$NON-NLS-1$
		} else {
			uid = "ldap:" + login.toLowerCase(); //$NON-NLS-1$
		}
		this.userId = userId;
	}

	@Override
	public boolean authenticate(IConnectionUserBean user, String login, char[] secret) {
		LDAPConnection cn = parent.getConnection();
		if (cn == null) {
			parent.getActivator().error("Unable to get a valid connection to the LDAP server.");
			return false;
		}
		LDAPException e = null;
		locked = false;
		outOfDate = false;
		try {
			BindResult br = parent.bind(cn, login, secret);
			if (br != null) {
				try {
					if (PasswordExpiringControl.get(br) != null) {
						outOfDate = true;
					}
				} catch (LDAPException e1) {
					parent.getActivator().info(e1);
				}
				// The secret may have been changed and must be cached for potential other bind...
				this.secret = secret;
				return true;
			}
		} catch (LDAPException ee) {
			parent.getActivator().info("LDAP Binding error: " + ee.getLocalizedMessage());
			parent.getActivator().debug(ee);
			e = ee;
			try {
				if (PasswordExpiredControl.get(e.toLDAPResult()) != null) {
					locked = true;
				}
			} catch (LDAPException e1) {
				parent.getActivator().info(e1);
			}
			ResultCode rc = e.getResultCode();
			if (rc == ResultCode.INVALID_CREDENTIALS) {
				// https://ldapwiki.com/wiki/Common%20Active%20Directory%20Bind%20Errors
				String hex = LdapAuthentificationService.getADErrorCode(e.getDiagnosticMessage());
				if ("533".equals(hex) || "773".equals(hex) || "80090346".equals(hex) || //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
						"701".equals(hex) ||  "530".equals(hex)) { //$NON-NLS-1$ //$NON-NLS-2$
					locked = true;
				}
			} else {
				parent.getActivator().error("Unexpected LDAP Binding error: " + ee.getLocalizedMessage());
			}
		} finally {
			parent.closeConnection(cn, e);
		}
		return false;
	}
	
	@Override
	public String getText() {
		return login + Messages.LdapConnectionCredential_LDAPLogin;
	}

	@Override
	public String getUniqueId() {
		return uid;
	}

	@Override
	public boolean isOutOfDate() {
		return outOfDate;
	}

	@Override
	public int loadUserId() {
		return userId;
	}

	@Override
	public void update(IConnectionUserBean user) {
		user.setCanChangePWD(parent.isCanChangePWD());
		user.setChangePWD(isOutOfDate());
		user.setLocked(isLocked());
		if ((user instanceof ConnectionUserBean) && (user.getId() < 0)) {
			((ConnectionUserBean) user).setFullname(login);
			((ConnectionUserBean) user).setPrincipal(0);
			((ConnectionUserBean) user).setLogin(login);
			if (ConnectionUserBean.STANDALONECONNECTIONS != null) {
				((ConnectionUserBean) user).setProfile(new Profile(user, ConnectionUserBean.STANDALONECONNECTIONS));
			} else {
				((ConnectionUserBean) user).setProfile(new AutoProfile(userId, user));
			}
		}
	}

	public boolean isLocked() {
		return locked;
	}

	@Override
	public String updatePassword(IConnectionUserBean user, char[] oldPassword, char[] newPassword, Language language) {
		LDAPConnection cn = parent.getConnection();
		LDAPException e = null;
		try {
			BindResult br = parent.bind(cn, login, secret);
			if (br == null) {
				return "User not found into the LDAP server.";
			}
			String dn = br.getMatchedDN();
			if (dn == null) {
				dn = parent.getUserDN(cn, login);
				if (dn == null) {
					return "The LDAP congifuration is unable to identify the LDAP user only from his \"login\"."; 
				}
			}
			String error = parent.changePWD(cn, dn, oldPassword, newPassword);
			if (error.isEmpty()) {
				// The secret may have been changed and must be cache for potential other bind...
				synchronized (this) {
					this.secret = newPassword;
				}
				return ""; //$NON-NLS-1$
			}
			return error;
		} catch (LDAPException ee) {
			e = ee;
			return e.getDiagnosticMessage();
		} finally {
			parent.closeConnection(cn, e);
		}
	}

	@Override
	public String getUserType() {
		if (userId < 0) {
			return null;
		}
		return "user"; //$NON-NLS-1$
	}

	@Override
	public boolean checkSecret(char[] secret) {
		synchronized (this) {
			if ((this.secret == null) || (this.secret.length != secret.length)) {
				return false;
			}
			for (int i = 0; i < secret.length; i++) {
				if (this.secret[i] != secret[i]) {
					return false;
				}
			}
		}
		return true;
	}
	
	public LdapAuthentificationService getParent() {
		return parent;
	}
	
	protected LDAPConnection getLDAPConnection() {
		LDAPConnection cn = parent.getConnection();
		try {
			if (parent.bind(cn, login, secret) != null) {
				return cn;
			}
		} catch (LDAPException e) {
			parent.closeConnection(cn, e);
		}
		return null;
	}
	
	protected void closeLDAPConnection(LDAPConnection connection, LDAPException error) {
		parent.closeConnection(connection, error);
	}
}