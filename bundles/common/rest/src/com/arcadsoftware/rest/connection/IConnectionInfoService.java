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
package com.arcadsoftware.rest.connection;

/**
 * This service is able to return connections and authorization informations about a specific user.
 * 
 * <p>
 * Informations corresponds to the {@link IConnectionUserBean} properties :
 * 
 * <ul>
 * <li>A List of Rights numbers.
 * <li>A presentation name (form information purposes, that may by the user full name).
 * <li>A optional principal number (principals are groups of Connection user or organizational units).
 * </ul>
 */
public interface IConnectionInfoService {

	/**
	 * The OSGi service reference name.
	 */
	public static final String clazz = IConnectionInfoService.class.getName();
	
	/**
	 * Load the user authorization information about the given user Id.
	 * 
	 * <p>If the type or the id does not correspond to any connection user managed by this
	 * service it must return null. Other services may manage it.
	 * 
	 * @param userType the User entity type.
	 * @param id an user internal id.
	 * @return null if this user does not exist.
	 */
	public ConnectionUserBean loadUser(String userType, int id);
}
