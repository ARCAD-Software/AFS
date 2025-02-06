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
 * This interface can be implemented by IConnectionCredential that can modify the current user definition.
 * 
 * @see IConnectionCredential
 */
public interface IPatchUserCredential {

	/**
	 * pre-process the current user information just before it is given to the client.
	 * 
	 * @param user
	 */
	public void patchUser(IConnectionUserBean user);
}
