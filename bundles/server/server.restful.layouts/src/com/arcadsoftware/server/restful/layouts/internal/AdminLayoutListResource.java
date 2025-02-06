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
package com.arcadsoftware.server.restful.layouts.internal;

import java.util.ArrayList;
import java.util.Collections;

import org.restlet.data.CharacterSet;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.representation.FileRepresentation;
import org.restlet.representation.Representation;
import org.restlet.representation.StringRepresentation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.rest.JSONRepresentation;
import com.arcadsoftware.rest.JsonStreamCompact;
import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.rest.XMLRepresentation;
import com.arcadsoftware.rest.XStreamCompact;

public class AdminLayoutListResource extends UserLinkedResource {

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		getAllowedMethods().add(Method.GET);
		addVariants(MEDIATYPES_BASESUP);
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		ArrayList<String> result = new ArrayList<String>();
		for(String n:Activator.getInstance().getFileList(null, true, true)) {
			if ((n != null) && (n.length() > 0)) {
				if (n.charAt(0) == '/') {
					n = n.substring(1);
				}
				if ((n.length() > 3) && (n.charAt(n.length() - 3) == '_')) {
					n = n.substring(0,n.length() - 3);
				}
				if (!result.contains(n)) {
					result.add(n);
				}
			}
		}
		Collections.sort(result);
		if (isJSON(variant)) {
			JsonStreamCompact js = new JsonStreamCompact(AdminLayoutListResource.class.getClassLoader(), true);
			js.alias("list", ArrayList.class); //$NON-NLS-1$
			return new JSONRepresentation(js.toXML(result), getClientPreferedLanguage());
		}
		if (isXML(variant)) {
			XStreamCompact xs = new XStreamCompact();
			xs.alias("list", ArrayList.class); //$NON-NLS-1$
			return new XMLRepresentation(xs.toXML(result), getClientPreferedLanguage());
		}
		if (isXSD(variant)) {
			return new FileRepresentation(Activator.getInstance().getBundleFile("schemas/layoutlist.xsd"), MediaType.APPLICATION_W3C_SCHEMA); //$NON-NLS-1$
		}
		if (isHTML(variant)) {
			StringBuilder s = new StringBuilder("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n<head>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n<title>Layouts</title>\n</head>\n<body>\n"); //$NON-NLS-1$
			String ref = ""; //$NON-NLS-1$
			if (!getRequest().getResourceRef().toString().endsWith("/")) {
				ref = "layout/"; //$NON-NLS-1$
			}
			s.append("<ul>"); //$NON-NLS-1$
			for(String n:result) {
				s.append("<li><a href=\""); //$NON-NLS-1$
				s.append(ref);
				s.append(n);
				s.append("\">"); //$NON-NLS-1$
				s.append(n);
				s.append("</a></li>\n"); //$NON-NLS-1$
			}
			s.append("</ul></body></html>"); //$NON-NLS-1$
			return new StringRepresentation(s.toString(), MediaType.APPLICATION_XHTML, getClientPreferedLanguage(), CharacterSet.UTF_8);
		}
		return super.get(variant);
	}
}
