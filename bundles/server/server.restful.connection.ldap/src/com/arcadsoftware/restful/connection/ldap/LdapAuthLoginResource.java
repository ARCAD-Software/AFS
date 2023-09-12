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
package com.arcadsoftware.restful.connection.ldap;

import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.unboundid.ldap.sdk.LDAPConnection;
import com.unboundid.ldap.sdk.LDAPException;

public class LdapAuthLoginResource extends LDAPUserLinkedResource {

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		if (hasRight(38)) {
			getAllowedMethods().add(Method.GET);
		}
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		String login = getAttribute("login"); //$NON-NLS-1$
		// test if this login is not already used...
		if ((login != null) && (getLdapAuthentificationService().getActivator().getAuth(login) > 0)) {
			setStatus(Status.CLIENT_ERROR_CONFLICT);
		} else {
			LDAPConnection cn = getLDAPConnection();
			LDAPException e = null;
			try {
				// Test if this login exists in the LDAP server.
				if (getLdapAuthentificationService().isLoginExists(cn, login)) {
					setStatus(Status.SUCCESS_NO_CONTENT);
				} else {
					setStatus(Status.CLIENT_ERROR_NOT_FOUND);
				}
			} catch (LDAPException ee) {
				e = ee;
				getLdapAuthentificationService().getActivator().error(e);
			} finally {
				closeLDAPConnection(cn, e);
			}
		}
		return null;
	}
	
}
