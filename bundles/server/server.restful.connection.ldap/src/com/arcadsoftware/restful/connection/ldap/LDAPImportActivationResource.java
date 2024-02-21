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
package com.arcadsoftware.restful.connection.ldap;

import org.restlet.data.Method;
import org.restlet.representation.Representation;
import org.restlet.representation.StringRepresentation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

public class LDAPImportActivationResource extends LDAPUserLinkedResource {
	
	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		if (hasRight(31)) {
			getAllowedMethods().add(Method.GET);
		}
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		if (getLdapAuthentificationService().isUserImportEnabled()) {
			return new StringRepresentation("true");
		}
		return new StringRepresentation("false");
	}
}
