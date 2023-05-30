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
package com.arcadsoftware.rest.internal;

import org.restlet.Request;
import org.restlet.data.ChallengeResponse;

import com.arcadsoftware.osgi.ILoggedPlugin;
import com.arcadsoftware.rest.WebServiceAccess;

public class WebForwardServiceAccess extends WebServiceAccess {

	private final ChallengeResponse challengeResponse;
	
	public WebForwardServiceAccess(ILoggedPlugin activator, Request request) {
		super(activator);
		challengeResponse = request.getChallengeResponse();
	}

	@Override
	public String getLogin() {
		throw new RuntimeException("Internal Error: Usage of getLogin() method is not available from Server side REST Call !");
	}

	@Override
	protected Request preprocess(Request request) {
		// Forward the challenge response from the original request !
		request.setChallengeResponse(challengeResponse);
		return request;
	}
	
}
