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

import org.restlet.data.Language;
import org.restlet.resource.ResourceException;

/**
 * This interface can be implemented by IConnectionCredential that allow the current user to change its password.
 * 
 * @see IConnectionCredential
 * @see IConnectionUserBean
 */
public interface IUpdatableCredential {

	/**
	 * Allow the current user to modify this own password. 
	 * 
	 * <p>This can also be done by a service that already known the current password of this user. 
	 * 
	 * @param connectionUserBean
	 * @param oldPassword
	 * @param newPassword
	 * @param language
	 * @return an empty string if this is a success, null or a error message if the new password is rejected.
	 */
	public String updatePassword(IConnectionUserBean user, char[] oldPassword, char[] newPassword, Language language) throws ResourceException;

}
