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
package com.arcadsoftware.connection.internal;

import java.io.File;
import java.util.ArrayList;

import org.osgi.framework.ServiceReference;
import org.restlet.data.CharacterSet;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.representation.Representation;
import org.restlet.representation.StringRepresentation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.rest.JSONFriendlyList;
import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.rest.XMLRepresentation;
import com.arcadsoftware.rest.XStreamCompact;
import com.arcadsoftware.rest.connection.IAuthentificationService;

public class AuthServicesResource extends UserLinkedResource {

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		getAllowedMethods().add(Method.GET);
		addVariants(MEDIATYPES_BASESUP);
		getVariants().add(new Variant(MediaType.TEXT_PLAIN));
		getVariants().add(new Variant(MediaType.TEXT_XML));
		getVariants().add(new Variant(MediaType.APPLICATION_W3C_SCHEMA));
		getVariants().add(new Variant(MediaType.APPLICATION_XHTML));
		getVariants().add(new Variant(MediaType.TEXT_HTML));
	}
	
	@Override
	protected Representation get(Variant variant) throws ResourceException {
		if (isXML(variant)) {
			XStreamCompact x = new XStreamCompact();
			x.alias("list", ArrayList.class); //$NON-NLS-1$
			return new XMLRepresentation(x.toXML(getAuthList()), variant.getMediaType());
		}
		if (isJSON(variant)) {
			return new StringRepresentation(new JSONFriendlyList<>(getAuthList()).toJson(), MediaType.APPLICATION_JSON, Language.ENGLISH, CharacterSet.UTF_8);
		}
		if (isXSD(variant)) {
			File file = Activator.getInstance().getSchema("/schema/authservices.xsd"); //$NON-NLS-1$
			if ((file != null) && file.isFile()) {
				return XMLRepresentation.fromFile(file, MediaType.APPLICATION_W3C_SCHEMA);
			}
			throw new ResourceException(Status.CLIENT_ERROR_GONE);
		}
		if (isHTML(variant)) {
			StringBuilder sb = new StringBuilder("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><title>Authentification services registered</title></head><body>"); //$NON-NLS-1$
			for (String auth: getAuthList()) {
				sb.append("<p>"); //$NON-NLS-1$
				sb.append(auth);
				sb.append(" (<a href=\"/"); //$NON-NLS-1$
				sb.append(auth);
				sb.append("/\">list</a>)</p>"); //$NON-NLS-1$
			}
			sb.append("</body></html>"); //$NON-NLS-1$
			return new StringRepresentation(sb, MediaType.TEXT_HTML, Language.ENGLISH, CharacterSet.UTF_8);
		}
		if (MediaType.TEXT_PLAIN.equals(variant.getMediaType())) {
			return new StringRepresentation(getAuthList().toString(), MediaType.TEXT_PLAIN, Language.ENGLISH, CharacterSet.UTF_8);
		}
		return super.get(variant);
	}

	@SuppressWarnings("rawtypes")
	private ArrayList<String> getAuthList() {
		ArrayList<String> list = new ArrayList<String>();
		for (ServiceReference sr:Activator.getInstance().getAuthServiceTracker().getServiceReferences()) {
			Object o = sr.getProperty(IAuthentificationService.ENTITYNAME);
			if (o != null) {
				String v = o.toString();
				if ((v.length() > 0) && !list.contains(v)) {
					list.add(v);
				}
			}
		}
		return list;
	}
}
