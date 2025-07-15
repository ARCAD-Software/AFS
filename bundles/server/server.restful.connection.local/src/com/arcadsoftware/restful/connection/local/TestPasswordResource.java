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
package com.arcadsoftware.restful.connection.local;

import java.nio.CharBuffer;

import org.restlet.data.CharacterSet;
import org.restlet.data.Form;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.representation.Representation;
import org.restlet.representation.StringRepresentation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.rest.connection.ConnectionUserBean;
import com.arcadsoftware.rest.connection.IConnectionCredential;
import com.arcadsoftware.rest.connection.IPasswordComplexityTester;
import com.arcadsoftware.rest.connection.IPatchUserCredential;

public class TestPasswordResource extends UserLinkedResource {

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		getAllowedMethods().add(Method.GET);
		getAllowedMethods().add(Method.POST);
		getAllowedMethods().add(Method.PUT);
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		return post(getRequestEntity(), variant);
	}

	@Override
	protected Representation put(Representation representation, Variant variant) throws ResourceException {
		return post(representation, variant);
	}

	@Override
	protected Representation post(Representation entity, Variant variant) throws ResourceException {
		Form form = getRequestForm();
		Language language = getClientPreferedLanguage();
		String login = form.getFirstValue("login"); //$NON-NLS-1$
		String oldp = form.getFirstValue("oldpassword"); //$NON-NLS-1$
		String p = form.getFirstValue("newpassword"); //$NON-NLS-1$
		if (login == null) {
			ConnectionUserBean user = (ConnectionUserBean) getUser();
			IConnectionCredential cc = (IConnectionCredential) getRequest().getAttributes().get(IConnectionCredential.CONNECTED_CREDENTIAL);
			login = user.getLogin();
			oldp = user.getPassword();
			if (cc instanceof IPatchUserCredential) {
				user = (ConnectionUserBean) user.clone();
				((IPatchUserCredential) cc).patchUser(user);
				if (user.getPassword() != null) {
					oldp = user.getPassword();
				}
				if(user.getLogin() != null) {
					login = user.getLogin();
				}
			}
			if (oldp == null) {
				oldp = ""; //$NON-NLS-1$
			}
		} else if (oldp == null) {
			if (login.equals(getUser().getLogin())) {
				oldp = getUser().getPassword();
				IConnectionCredential cc = (IConnectionCredential) getRequest().getAttributes().get(IConnectionCredential.CONNECTED_CREDENTIAL);
				if (cc instanceof IPatchUserCredential) {
					ConnectionUserBean user = ((ConnectionUserBean) getUser()).clone();
					((IPatchUserCredential) cc).patchUser(user);
					if ((user.getPassword() != null) && login.equals(user.getLogin())) {
						oldp = user.getPassword();
					}
				}
			} else {
				oldp = ""; //$NON-NLS-1$
			}
		}
		if (login == null) {
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, Activator.getMessage("Error_missing_login", language)); //$NON-NLS-1$
		}
		if (oldp == null) {
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, Activator.getMessage("Error_missing_oldpassword", language)); //$NON-NLS-1$
		}
		IPasswordComplexityTester tester = getOSGiService(IPasswordComplexityTester.class);
		if (tester == null) {
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, Activator.getMessage("Error_no_service_available", language)); //$NON-NLS-1$
		}
		final char[] password;
		if ((p == null) || (p.length() == 0)) {
			password = null;
		} else {
			password = p.toCharArray();
		}
		final char[] oldpassword;
		if (oldp.length() == 0) {
			oldpassword = null;
		} else {
			oldpassword = oldp.toCharArray();
		}
		try {
			if (password == null) {
				char[] np = tester.generateAcceptablePassword(login, oldpassword);
				if (np == null) {
					throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, Activator.getMessage("Error_password_generation_failed", language)); //$NON-NLS-1$
				}
				return new StringRepresentation(CharBuffer.wrap(np), MediaType.TEXT_PLAIN, Language.ENGLISH_US, CharacterSet.UTF_8);
			}
			int reason = tester.isPasswordAcceptable(login, oldpassword, password);
			if (reason == PasswordComplexityTester.REASON_OK) {
				setStatus(Status.SUCCESS_NO_CONTENT);
				return null;
			}
			throw new ResourceException(Status.CLIENT_ERROR_NOT_FOUND,tester.getTextualReason(reason, language));
		} catch (Exception e) {
			getOSGiApplication().getActivator().error(e.getLocalizedMessage(), e);
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, e.getLocalizedMessage());
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
