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

import java.util.List;

/**
 * This OSGi service offer access to IConnectionUserBean that can be used to perform secured operation.
 * 
 * <p>
 * Theses Object content basic user information (type, id, principal) and Rights owned.
 * 
 * @author ARCAD Software
 *
 */
public interface IConnectionUserRepository {

	public static final String clazz  = IConnectionUserRepository.class.getName();
	
	/**
	 * Return an User Connection information Bean that can be used to access to privileged operation.
	 * 
	 * <p>
	 * This user information does not content any login information, nor password.
	 *
	 * @param userType The User type entity.
	 * @param userId A User Id.
	 * @return an copy of the User connection information.
	 */
	public IConnectionUserBean getUser(String userType, int userId);

	/**
	 * Return an User Connection information Bean that can be used to access to privileged operation.
	 * 
	 * <p>
	 * This user information contents the given login information, but none password is returned.
	 * 
	 * @param login A user login.
	 * @return an copy of the User connection information.
	 */
	public IConnectionUserBean getUser(String login);
	
	/**
	 * Return the list of login that the given user may use to connect to services.
	 * 
	 * @param userType The User type entity.
	 * @param userId A User Id.
	 * @return an empty list if this user do not own any login.
	 */
	public List<String> getUserLogins(String userType, int userId);
	
}
