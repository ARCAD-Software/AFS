/**
 * Copyright (c) Arcad-Software (2009). All Rights Reserved.
 * 
 * Creation date: 17 sept. 2009
 * @author ARCAD Software
 * 
 */
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
