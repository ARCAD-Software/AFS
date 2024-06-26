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
package com.arcadsoftware.rest.console.internal;

import java.io.File;
import java.util.Date;
import java.util.List;

import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.representation.FileRepresentation;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.rest.JSONRepresentation;
import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.rest.XMLRepresentation;
import com.arcadsoftware.rest.console.ConsoleField;
import com.arcadsoftware.rest.console.IActivableConsoleNode;
import com.arcadsoftware.rest.console.IRestConsoleSection;

public class SectionResource extends UserLinkedResource {

	private Activator activator;
	private Language language;
	private IRestConsoleSection section;
	private File file;
	
	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		activator = Activator.getInstance();
		if (activator == null) {
			setExisting(false);
			return;
		}
		language = getClientPreferedLanguage();
		if (!hasRight(2)) {
			throw new ResourceException(Status.CLIENT_ERROR_FORBIDDEN, activator.localize("error.noright", language)); //$NON-NLS-1$
		}
		String name = getAttribute("section"); //$NON-NLS-1$
		section = activator.getSection(name); //$NON-NLS-1$
		if ((section == null) || ((section instanceof IActivableConsoleNode) && !((IActivableConsoleNode)section).isActivated())) {
			file = activator.getBundleFile("/files/" + name); //$NON-NLS-1$
			if ((file == null) || !file.isFile()) {
				setExisting(false);
				return;
			}
			setLastModification(new Date(activator.getContext().getBundle().getLastModified()));
		}
		getAllowedMethods().add(Method.GET);
		addVariants(MediaType.TEXT_XML, MediaType.APPLICATION_W3C_SCHEMA);
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		if (file != null) {
			return new FileRepresentation(file, variant.getMediaType());
		}
		if (isXSD(variant)) {
			return new FileRepresentation(activator.getBundleFile("schemas/form.xsd"), MediaType.APPLICATION_W3C_SCHEMA); //$NON-NLS-1$
		}
		List<ConsoleField> result = section.getForm(language);
		if (result == null) {
			setStatus(Status.SUCCESS_NO_CONTENT);
			return null;
		}
		if (isXML(variant)) {
			return new XMLRepresentation(activator.getXStream().toXML(result),language);
		}
		if (isJSON(variant)) {
			return new JSONRepresentation(ActionResource.js.toXML(result),language);
		}
		return null;
	}
}