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
package com.arcadsoftware.restful.connection.local;

import org.restlet.Context;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.Restlet;
import org.restlet.data.Status;

import com.arcadsoftware.rest.BaseResource;

public class LocalAuthLoginRestlet extends Restlet {

	private final Activator activator;
	
	public LocalAuthLoginRestlet(Activator activator, Context context) {
		super(context);
		this.activator = activator;
	}

	@Override
	public void handle(Request request, Response response) {
		super.handle(request, response);
		String login = (String) request.getAttributes().get("login"); //$NON-NLS-1$
		if ((login != null) && (login.length() > 0)) {
			if (login.length() > 255) {
				response.setStatus(Status.CLIENT_ERROR_BAD_REQUEST);
			} else if (activator.getAuth(login) == null) {
				response.setStatus(Status.SUCCESS_NO_CONTENT);
			} else {
				response.setStatus(Status.CLIENT_ERROR_CONFLICT, Activator.getMessage("Error_Login_invalid",  //$NON-NLS-1$
						BaseResource.getClientPreferedLanguage(request)));
			}
		} else {
			response.setStatus(Status.CLIENT_ERROR_BAD_REQUEST);
		}
	}
}
