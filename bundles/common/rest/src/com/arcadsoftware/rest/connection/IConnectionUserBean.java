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
package com.arcadsoftware.rest.connection;

import java.util.List;

/**
 * This interface define the identification and authorization that the server must provide to any secured connection.
 * 
 * <p>
 * In some circumstances theses information can b associated to partial or fake information, but most of the time
 * they comes from an <code>ConnectionUserBean</code> implementation.
 * 
 * @see ConnectionUserBean
 */
public interface IConnectionUserBean {
	
	/**
	 * This is the HTTP request attribute that is associated with the user identified.
	 */
	public static final String CONNECTED_USER = "connected.user"; //$NON-NLS-1$

	/**
	 * This is the HTTP request attribute associated with the uniquID of the identification method used.
	 */
	public static final String CONNECTED_UNIQUEID = "connected.uniqueid"; //$NON-NLS-1$

	/**
	 * This profile object contain the list of all the owned rights of this user.
	 * 
	 * @return The current user Profile.
	 */
	public Profile getProfile();
	
	/**
	 * The User full name can be used for printing.
	 * 
	 * @return a human readable user full name.
	 */
	public String getFullname();
	
	/**
	 * If any, return the identification modes corresponding to this class.
	 * @param clazz
	 * @return
	 */
	public List<IConnectionCredential> getCredential(Class<? extends IConnectionCredential> clazz);
	
	/**
	 * Return the identification mode corresponding to this uniqueID. 
	 * 
	 * @param uniqueID
	 * @return
	 */
	public IConnectionCredential getCredential(String uniqueID);
	
	/**
	 * Return the credential list 
	 * @return
	 */
	public List<IConnectionCredential> getCredentials();
	
	/**
	 * Return the user Id (form the USERS entity table into Customer database). 
	 * 
	 * @return the id
	 */
	public int getId();
	
	/**
	 * Return the client entity defined as the owner of this user.
	 * 
	 * <p>
	 * The principal can be a group or a client id, any user organization that can regroup multiple users.
	 * @return
	 */
	public int getPrincipal();

	/**
	 * Ask the user to change his password now. He granted a limited access to services.
	 * 
	 * <p>This method should be only used by Authentification providers.
	 * 
	 * <p>
	 * This property is volatile. Its value depends on the last "Connection Service" used by this user
	 * to connect to the web-service and may change from one call to another (is a use have multiple
	 * login credential).
	 * 
	 * @param changePWD
	 */
	public void setChangePWD(boolean changePWD);

	/**
	 * State that the user must change its password before to access to web-services.
	 * 
	 * <p>
	 * This property is volatile. Its value depends on the last "Connection Service" used by this user
	 * to connect to the web-service and may change from one call to another (is a use have multiple
	 * login credential).
	 * 
	 * @return true if the current user must change his password
	 */
	public boolean isChangePWD();

	/**
	 * Lock the user connection, any connection retry will be rejected.
	 * 
	 * <p>This method should be only used by Authentification providers.
	 * 
	 * <p>
	 * This property is volatile. Its value depends on the last "Connection Service" used by this user
	 * to connect to the web-service and may change from one call to another (is a use have multiple
	 * login credential).
	 * 
	 * @param locked
	 */
	public void setLocked(boolean locked);

	/**
	 * <p>Actually, a locked user should not access to web-resources.
	 * 
	 * <p>
	 * This property is volatile. Its value depends on the last "Connection Service" used by this user
	 * to connect to the web-service and may change from one call to another (is a use have multiple
	 * login credential).
	 * 
	 * @return true if the current user is locked.
	 */
	public boolean isLocked();

	/**
	 * Indicate that the user has the possibility to modify his own password.
	 * 
	 * <p>This method should be only used by Authentification providers.
	 * 
	 * <p>
	 * This property is volatile. Its value depends on the last "Connection Service" used by this user
	 * to connect to the web-service and may change from one call to another (is a use have multiple
	 * login credential).
	 * 
	 * @param canChangePWD
	 */
	public void setCanChangePWD(boolean canChangePWD);

	/**
	 * Indicate if the user can change its own password.
	 * 
	 * <p>
	 * This property is volatile. Its value depends on the last "Connection Service" used by this user
	 * to connect to the web-service and may change from one call to another (is a use have multiple
	 * login credential).
	 * 
	 * @return true if the current user can change its password (with the /currentuser web-resource).
	 */
	public boolean isCanChangePWD();
	
	/**
	 * If given, this password is a valid one.
	 * 
	 * <p>
	 * This property is volatile. Its value depends on the last "Connection Service" used by this user
	 * to connect to the web-service and may change from one call to another (is a use have multiple
	 * login credential).
	 * 
	 * @return the User password.
	 */
	public String getPassword();

	/**
	 * This login is associated to a valid Credential authenticated.
	 * 
	 * <p>
	 * This property is volatile. Its value depends on the last "Connection Service" used by this user
	 * to connect to the web-service and may change from one call to another (is a use have multiple
	 * login credential).
	 * 
	 * @return The User Login.
	 */
	public String getLogin();
	
	/**
	 * Get the suer Data Entity Type used to store the user information into the database.  
	 * @return The user entity Type.
	 */
	public String getUserType();
}
