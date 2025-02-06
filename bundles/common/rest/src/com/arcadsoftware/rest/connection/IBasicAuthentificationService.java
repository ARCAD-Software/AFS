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

import org.restlet.Request;

/**
 * Define an OSGi service used to authenticate users using the HTTP BASIC schema. 
 * 
 * @author ARCAD Software
 */
public interface IBasicAuthentificationService extends IAuthentificationService {
	
	/**
	 * Generate a new credential for theses identifiers.
	 * 
	 * <p>This method must return false if this service refuse to deal with this request.
	 * 
	 * @param request
	 * @param identifier
	 * @return
	 */
	public IConnectionCredential generateCredential(Request request, String identifier);

}
