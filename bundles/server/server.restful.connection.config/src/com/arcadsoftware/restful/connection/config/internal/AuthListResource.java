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

import java.util.ArrayList;

import org.restlet.data.Method;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.rest.BaseResource;
import com.arcadsoftware.rest.XMLRepresentation;
import com.arcadsoftware.rest.XStreamCompact;

public class AuthListResource extends BaseResource {

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		getAllowedMethods().add(Method.OPTIONS);
		getAllowedMethods().add(Method.HEAD);
		getAllowedMethods().add(Method.GET);
		setVariants(MEDIATYPES_BASE_XMLJSON);
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		XStreamCompact xs = new XStreamCompact(AuthListResource.class.getClassLoader());
		xs.alias("list", ArrayList.class); //$NON-NLS-1$
		xs.alias("configauth", LoginBean.class); //$NON-NLS-1$
		xs.useAttributeFor(LoginBean.class, "login"); //$NON-NLS-1$
		xs.useAttributeFor(LoginBean.class, "id"); //$NON-NLS-1$
		return new XMLRepresentation(xs.toXML(Activator.getInstance().getLogins()), getClientPreferedLanguage());
	}
}
