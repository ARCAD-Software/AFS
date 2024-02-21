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
package com.arcadsoftware.rest.connection;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.List;

import com.arcadsoftware.rest.internal.Messages;

/**
 * This bean store the user identification and authorization.
 * 
 * One user can have multiples identification schemes 
 * but only one ID an profile.
 * 
 */
public class ConnectionUserBean implements Cloneable, IConnectionUserBean, Serializable {

	private static final long serialVersionUID = 4504489046218633114L;
	
	/**
	 * If different from null state that this server is run without connections MetaDataEntities, and should
	 * use the given list of rights for all authenticated connections.
	 */
	public static final List<Right> STANDALONECONNECTIONS;
	
	static {
		String s = System.getProperty("com.arcadsoftware.rest.connection.standalone"); //$NON-NLS-1$
		if ((s != null) && !s.isEmpty()) {
			String[] rs = s.split(" "); //$NON-NLS-1$
			ArrayList<Right> ls = new ArrayList<Right>();
			for(int i = rs.length - 1; i >= 0; i--) {
				try {
					if (rs[i].indexOf('-') == 0) {
						int x = -Integer.parseInt(rs[i]);
						while (x > 0) {
							ls.add(new Right(x--));
						}
					} else if (rs[i].indexOf('-') > 0) {
						String[] iv = rs[i].split("\\-"); //$NON-NLS-1$
						int x = Integer.parseInt(iv[0]);
						int y = Integer.parseInt(iv[1]);
						while (x <= y) {
							ls.add(new Right(x++));
						}
					} else {
						ls.add(new Right(Integer.parseInt(rs[i])));
					}
				} catch (NumberFormatException e) {}
			}
			STANDALONECONNECTIONS = Collections.unmodifiableList(ls);
		} else {
			STANDALONECONNECTIONS = null;
		}
	}

	private transient Hashtable<Object, IConnectionCredential> credentials = new Hashtable<Object, IConnectionCredential>(1);
	private transient Date storeDate = new Date();
	private Profile profile;
	// Persistant properties
	private int id;
	private int principal;
	private String fullname;
	// transients properties (set by Credentials)
	private boolean changePWD;
	private boolean canChangePWD;
	private transient boolean locked;
	private transient String password;
	private transient String login;
	private transient String userType;
	
	public ConnectionUserBean() {
		super();
	}
	
	public ConnectionUserBean(int id) {
		super();
		this.id = id;
	}
	
	public ConnectionUserBean(String userType, int id) {
		super();
		this.id = id;
		this.userType = userType;
	}
	
	public Profile getProfile() {
		return profile;
	}
	
	/**
	 * Set the user profile.
	 * @param profile
	 */
	public void setProfile(Profile profile) {
		this.profile = profile;
	}
	
	public String getFullname() {
		return fullname;
	}
	
	/**
	 * Set the user full name, for presentation only.
	 * @param fullname
	 */
	public void setFullname(String fullname) {
		this.fullname = fullname;
	}
	
	public List<IConnectionCredential> getCredential(Class<? extends IConnectionCredential> clazz) {
		ArrayList<IConnectionCredential> result = new ArrayList<IConnectionCredential>();
		Enumeration<IConnectionCredential> elements = credentials.elements();
		while (elements.hasMoreElements()) {
			IConnectionCredential credential = elements.nextElement();
			if (clazz.isInstance(credential)) {
				result.add(credential);
			}
		}
		return result;
	}

	public IConnectionCredential getCredential(String uniqueID) {
		return credentials.get(uniqueID);
	}
	
	/**
	 * Add a new identification mode for this user.
	 * 
	 * Adding or updating a identification mode increment the validity date
	 * of this user in the cache.  
	 * 
	 * @param credential
	 */
	public void addCredential(IConnectionCredential credential) {
		// Adding a new credential to this bean update the cache limit.
		storeDate = new Date();
		credentials.put(credential.getUniqueId(),credential);
	}
	
	/**
	 * Remove an identification mode for this user.
	 * @param credential
	 */
	public void removeCredential(IConnectionCredential credential) {
		credentials.remove(credential.getUniqueId());
	}

	public List<IConnectionCredential> getCredentials() {
		return Collections.list(credentials.elements()); 
	}

	/**
     * Creates and returns a copy of this object. 
	 * 
	 * <p>the copy does not clone the profile (witch is considered as a final field).
	 * 
	 * <p>The credential list is not cloned too, it is supposed useless.
	 */
	@Override
	public ConnectionUserBean clone() {
		ConnectionUserBean result = new ConnectionUserBean(userType,id);
		result.storeDate = storeDate;
		// Identification: Données non volatiles.
		result.setFullname(fullname);
		result.setPrincipal(principal);
		result.setProfile(profile);
		// State: Données volatiles liées au dernier Credential utilisé.
		// [ML] 07/2015: On ne duplique pas les données volatiles...
		// Credential, Login and Password are not cloned.
		return result;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(fullname);
		sb.append(" ["); //$NON-NLS-1$
		sb.append(id);
		sb.append("] "); //$NON-NLS-1$
		sb.append(Messages.ConnectionUserBean_Locked);
		sb.append(locked);
		return sb.toString();
	}

	/**
	 * Get the date of the first usage of this user.
	 * 
	 * @return a non null Date
	 */
	public Date getStoreDate() {
		return storeDate;
	}

	/**
	 * Change the user Id (form the USERS entity table into the database). 
	 * 
	 * @param id the user id to set
	 */
	public void setId(int id) {
		this.id = id;
	}

	public int getId() {
		return id;
	}

	/**
	 * Change the User principal.
	 * 
	 * <p>
	 * The principal is a group or client id, any user organization that can regroup multiple users.
	 * 
	 * @param principal
	 */
	public void setPrincipal(int principal) {
		this.principal = principal;
	}

	public int getPrincipal() {
		return principal;
	}

	public void setChangePWD(boolean changePWD) {
		this.changePWD = changePWD;
	}

	public boolean isChangePWD() {
		return changePWD;
	}

	public void setLocked(boolean locked) {
		this.locked = locked;
	}

	public boolean isLocked() {
		return locked;
	}
	
	public void setCanChangePWD(boolean canChangePWD) {
		this.canChangePWD = canChangePWD;
	}

	public boolean isCanChangePWD() {
		return canChangePWD;
	}

	/**
	 * Set the currently user password.
	 * 
	 * <p>
	 * This property is volatile. Its value depends on the last "Connection Service" used by this user
	 * to connect to the web-service and may change from one call to another (is a use have multiple
	 * login credential).
	 * 
	 * @param password
	 */
	public void setPassword(String password) {
		this.password = password;
	}

	public String getPassword() {
		return password;
	}

	/**
	 * Locally test the user password to the given one.
	 * 
	 * @param secret
	 * @return
	 */
	public boolean equalPassword(char[] secret) {
		return (password != null) && password.equals(new String(secret));
	}

	/**
	 * Set the currently used user login.
	 * <p>
	 * This property is volatile. Its value depends on the last "Connection Service" used by this user
	 * to connect to the web-service and may change from one call to another (is a use have multiple
	 * login credential).
	 * @param login
	 */
	public void setLogin(String login) {
		this.login = login;
	}

	public String getLogin() {
		return login;
	}

	/**
	 * Set the User Data Entity Type required to store the user information into the database.
	 * @param userType
	 */
	public void setUserType(String userType) {
		this.userType = userType;
	}

	public String getUserType() {
		return userType;
	}
}
