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
package com.arcadsoftware.restful.connection.config.internal;

import org.restlet.Context;
import org.restlet.Restlet;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.data.Status;

public class AuthLoginRestlet extends Restlet {

	public AuthLoginRestlet(Context context) {
		super(context);
	}

	@Override
	public void handle(Request request, Response response) {
		super.handle(request, response);
		String login = (String)request.getAttributes().get("login"); //$NON-NLS-1$
		if ((login != null) && Activator.getInstance().exists(login)) {
			response.setStatus(Status.SUCCESS_NO_CONTENT);
		} else {
			response.setStatus(Status.CLIENT_ERROR_NOT_FOUND);
		}
	}
}
