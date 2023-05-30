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

import org.osgi.framework.BundleContext;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;
import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.rest.connection.IAuthentificationService;
import com.arcadsoftware.rest.connection.IConnectionCredential;
import com.unboundid.ldap.sdk.LDAPConnection;
import com.unboundid.ldap.sdk.LDAPException;

public abstract class LDAPUserLinkedResource extends UserLinkedResource {

	private LdapAuthentificationService ldap;
	private LdapConnectionCredential credential;

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		BundleContext ctx = getBundleContext();
		try {
			for (ServiceReference<IAuthentificationService> sr: ctx.getServiceReferences(IAuthentificationService.class, null)) {
				IAuthentificationService service = ctx.getService(sr);
				if (service instanceof LdapAuthentificationService) {
					ldap = (LdapAuthentificationService) service;
					break;
				}
			}
		} catch (InvalidSyntaxException e) {}
		if (ldap != null) {
			Object connectedUserCredential = getRequest().getAttributes().get(IConnectionCredential.CONNECTED_CREDENTIAL);
			if (connectedUserCredential instanceof LdapConnectionCredential) {
				credential = (LdapConnectionCredential) connectedUserCredential;
			} else if (!ldap.isAlreadybinded()) {
				throw new ResourceException(Status.CLIENT_ERROR_FORBIDDEN, "To perform this operation the server require a user connected with a valid LDAP authentication.");
			}
		} else {
			setExisting(false);
		}
	}
	
	protected LdapAuthentificationService getLdapAuthentificationService() {
		return ldap;
	}
	
	protected LDAPConnection getLDAPConnection() {
		if (credential != null) {
			return credential.getLDAPConnection();
		}
		return ldap.getConnection();
	}
	
	protected void closeLDAPConnection(LDAPConnection connection, LDAPException error) {
		if (credential != null) {
			credential.closeLDAPConnection(connection, error);
		} else {
			ldap.closeConnection(connection, error);
		}
	}
	
	protected Activator getActivator() {
		return ldap.getActivator();
	}
}
