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

import org.restlet.resource.ResourceException;

/**
 * Define an OSGi service allowing to validate the connection of a user.
 * 
 * <p>
 * This service is called 
 * 
 * @author ARCAD Software
 */
public interface IConnectionUserCheck {

	/**
	 * This method is called each time a validated user access a resource.
	 * 
	 * <p>
	 * This method lock the current resource call and should ends quickly.
	 * 
	 * @param user
	 * @throws ResourceException
	 */
	public void check(IConnectionUserBean user) throws ResourceException;
}
