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

import java.util.List;

/**
 * This service generate an object that is used to identify a User.
 * 
 * <p>The created object is cached and will be asked to identify the user several times.
 */
public interface IAuthentificationService {

	/**
	 * The OSGi service name.
	 */
	public static final String clazz = IAuthentificationService.class.getName();

	/**
	 * OSGi Service property identify the authentification entity name.
	 */
	public static final String ENTITYNAME = "entityname"; //$NON-NLS-1$

	/**
	 * OSGi Service property identify the authentification among other ones, specifiyng a list of Realm name that can be used to identify the service.
	 */
	public static final String REALM = "realm"; //$NON-NLS-1$

	/**
	 * If this Authentification Service maintain a connection cache then this method must purge it. 
	 */
	public void purgeConnectionCache();

	/**
	 * If this Authentification Service maintain a connection cache then this method must purge it for the given user id. 
	 * 
	 * @param id a user ID.
	 */
	public void purgeConnectionCache(int id);
	
	/**
	 * Return the list of login that the given user may use to connect to services.
	 * 
	 * @param userType The User type entity.
	 * @param userId A User Id.
	 * @return an empty list if this user do not own any login.
	 */
	public List<String> getUserLogins(String userType, int userId);

}
