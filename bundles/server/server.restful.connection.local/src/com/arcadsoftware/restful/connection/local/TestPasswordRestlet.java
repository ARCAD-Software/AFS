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
package com.arcadsoftware.restful.connection.local;

import java.nio.CharBuffer;

import org.restlet.Context;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.Restlet;
import org.restlet.data.CharacterSet;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Status;
import org.restlet.representation.StringRepresentation;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.rest.BaseResource;

public class TestPasswordRestlet extends Restlet {

	private final Activator activator;

	public TestPasswordRestlet(Activator activator, Context context) {
		super(context);
		this.activator = activator;
	}

	@Override
	public void handle(Request request, Response response) {
		super.handle(request, response);
		String login = (String) request.getAttributes().get("login"); //$NON-NLS-1$
		String oldp = (String) request.getAttributes().get("oldpassword"); //$NON-NLS-1$
		String p = (String) request.getAttributes().get("newpassword"); //$NON-NLS-1$
		final char[] password;
		if ((p == null) || (p.length() == 0)) {
			password = null;
		} else {
			password = p.toCharArray();
		}
		final char[] oldpassword;
		if ((oldp == null) || (oldp.length() == 0)) {
			oldpassword = null;
		} else {
			oldpassword = oldp.toCharArray();
		}
		try {
			if (password == null) {
				char[] np = activator.getTester().generateAcceptablePassword(login, oldpassword);
				if (np != null) {
					response.setEntity(new StringRepresentation(CharBuffer.wrap(np), MediaType.TEXT_PLAIN, Language.ENGLISH_US, CharacterSet.UTF_8));
				} else {
					response.setStatus(Status.CLIENT_ERROR_NOT_FOUND,
							activator.getMessage("Error_password_generation_failed", //$NON-NLS-1$
									BaseResource.getClientPreferedLanguage(request)));
				}
				//Crypto.clear(np); Can not clear the array !
			} else {
				int reason = activator.getTester().isPasswordAcceptable(login, oldpassword, password);
				if (reason == PasswordComplexityTester.REASON_OK) {
					response.setStatus(Status.SUCCESS_NO_CONTENT);
				} else {
					response.setStatus(Status.CLIENT_ERROR_NOT_FOUND,
							activator.getTester().getTextualReason(reason, BaseResource.getClientPreferedLanguage(request)));
				}
			}
		} catch (Exception e) {
			activator.error(e);
			response.setStatus(Status.SERVER_ERROR_INTERNAL);
		} finally {
			if (oldpassword != null) {
				Crypto.clear(oldpassword);
			}
			if (password != null) {
				Crypto.clear(password);
			}
		}
	}
}
