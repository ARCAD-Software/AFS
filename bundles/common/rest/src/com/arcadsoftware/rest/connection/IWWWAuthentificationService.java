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

import org.restlet.Request;
import org.restlet.Response;
import org.restlet.data.ChallengeRequest;
import org.restlet.resource.ResourceException;

/**
 * Dedicated Authentification service using "non Basic" HTTP Authentification mode (like DIGEST, NTLM, etc...)
 * 
 * @author ARCAD Software
 * @see IBasicAuthentificationService
 */
public interface IWWWAuthentificationService extends IAuthentificationService {
	
	/**
	 * Check the HTTP Request and Response to generate the User connection information.
	 * 
	 * <p>
	 * This implementation replace the default process (and cache) of IConnectionUserBean.
	 * 
	 * @param request
	 * @param response
	 * @return
	 */
	public IConnectionUserBean checkCredential(Request request, Response response) throws ResourceException;

	/**
	 * Generate the challenge request to be sent back to the HTTP Client.
	 * 
	 * <p>
	 * This challenge will be added to all the challenge type provided by this server (by default the BASIC one).
	 * 
	 * @param request
	 * @param defaultRealm
	 * @return
	 */
	public ChallengeRequest getChallengeRequest(Request request, String defaultRealm);
	
}
