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
package com.arcadsoftware.server.properties.internal;

import java.util.Date;

import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.representation.Representation;
import org.restlet.resource.ResourceException;
import org.restlet.representation.Variant;

import com.arcadsoftware.rest.JSONRepresentation;
import com.arcadsoftware.rest.JsonStreamCompact;
import com.arcadsoftware.rest.OSGiResource;
import com.arcadsoftware.rest.XMLRepresentation;
import com.arcadsoftware.rest.XStreamCompact;

public class PropertiesResource extends OSGiResource {

	private Activator.PropertiesDated pd;
	private String domainname;
	private Language language;
	
	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		// Read only not secured resource.
		domainname = getRequest().getResourceRef().getRemainingPart();
		if ((domainname == null) || (domainname.length() == 0)) {
			setExisting(false);
		} else {
			// Determine the existence of the specified properties resource bundle and
			// get its last modification date.
			language = getClientPreferedLanguage();
			pd = Activator.getInstance().getTranslations(domainname, language);
			setExisting(pd.date.after(new Date(0)));
			setLastModification(pd.date);
			getAllowedMethods().add(Method.GET);
			addVariants(MediaType.TEXT_PLAIN);
		}
	}

	@Override
	public Representation get(Variant variant) throws ResourceException {
		// "Properties" (aka. plain text) is the default format if any format is acceptable.
		// (Required for ascendent compatibility.)
		if (isPrefered(getClientInfo().getAcceptedMediaTypes(),MediaType.ALL,1F)) {
			return new PropertiesRepresentation(pd.properties, domainname, pd.date, language);
		}
		if (isJSON(variant)) {
			return new JSONRepresentation(new JsonStreamCompact().toXML(pd.properties), language, pd.date);
		}
		if (isXML(variant)) {
			return new XMLRepresentation(new XStreamCompact().toXML(pd.properties), language, pd.date);
		}
		return new PropertiesRepresentation(pd.properties, domainname, pd.date, language);
	}
}
