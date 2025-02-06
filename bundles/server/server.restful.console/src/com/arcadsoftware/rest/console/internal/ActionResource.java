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
package com.arcadsoftware.rest.console.internal;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.List;

import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.representation.FileRepresentation;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.rest.JSONRepresentation;
import com.arcadsoftware.rest.JsonStreamCompact;
import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.rest.XMLRepresentation;
import com.arcadsoftware.rest.console.ConsoleAction;
import com.arcadsoftware.rest.console.ConsoleField;
import com.arcadsoftware.rest.console.ConsoleProperty;
import com.arcadsoftware.rest.console.ConsoleSet;
import com.arcadsoftware.rest.console.ConsoleText;
import com.arcadsoftware.rest.console.IActivableConsoleNode;
import com.arcadsoftware.rest.console.IRestConsoleSection;

public class ActionResource extends UserLinkedResource {

	public static final JsonStreamCompact js = new JsonStreamCompact(ActionResource.class.getClassLoader(), true, true, true);
	
	static {
		js.alias("list", Collection.class); //$NON-NLS-1$
		js.alias("list", ArrayList.class); //$NON-NLS-1$
		js.alias("action", ConsoleAction.class); //$NON-NLS-1$
		js.registerConverter(new ActionConverter()); //$NON-NLS-1$
		js.alias("set", ConsoleSet.class); //$NON-NLS-1$
		js.alias("text", ConsoleText.class); //$NON-NLS-1$
		js.alias("property", ConsoleProperty.class); //$NON-NLS-1$
		js.aliasAttribute("default", "defaultvalue"); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	private Activator activator;
	private Language language;
	private IRestConsoleSection section;
	private String action;
	
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
		section = activator.getSection(getAttribute("section")); //$NON-NLS-1$
		if ((section == null) || ((section instanceof IActivableConsoleNode) && !((IActivableConsoleNode)section).isActivated())) {
			setExisting(false);
			return;
		}
		action = getAttribute("action"); //$NON-NLS-1$
		if (action == null) {
			setExisting(false);
			return;
		}
		getAllowedMethods().add(Method.GET);
		getAllowedMethods().add(Method.POST);
		getAllowedMethods().add(Method.PUT);
		addVariants(MediaType.TEXT_XML, MediaType.APPLICATION_W3C_SCHEMA);
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		return put(getRequestEntity(), variant);
	}

	@Override
	protected Representation post(Representation entity, Variant variant) throws ResourceException {
		return put(entity, variant);
	}

	@Override
	protected Representation put(Representation representation, Variant variant) throws ResourceException {
		if (isXSD(variant)) {
			return new FileRepresentation(activator.getBundleFile("schemas/form.xsd"), MediaType.APPLICATION_W3C_SCHEMA); //$NON-NLS-1$
		}
		List<ConsoleField> result = section.performAction(action, language, getRequestForm());
		broadcastUserAction();
		if (result == null) {
			setStatus(Status.SUCCESS_NO_CONTENT, activator.localize("info.success", language)); //$NON-NLS-1$
			return null;
		}
		if (isXML(variant)) {
			return new XMLRepresentation(activator.getXStream().toXML(result),language);
		}
		if (isJSON(variant)) {
			return new JSONRepresentation(js.toXML(result),language);
		}
		return null;
	}

	private void broadcastUserAction() {
		EventAdmin ea = activator.getService(EventAdmin.class);
		if (ea != null) {
			Hashtable<String, Object> props = new Hashtable<String, Object>();
			props.put("uid", getUser().getId()); //$NON-NLS-1$
			if (getUser().getLogin() != null) {
				props.put("login", getUser().getLogin()); //$NON-NLS-1$
			}
			props.put("code", "console"); //$NON-NLS-1$ //$NON-NLS-2$
			props.put("message", String.format("Administrative Console action \"%s\" perfomed on section \"%s\".", action, section.getLabel(Language.DEFAULT))); //$NON-NLS-1$ //$NON-NLS-2$
			props.put("date", new Date()); //$NON-NLS-1$
			ea.postEvent(new Event("com/arcadsoftware/user/action", (Dictionary<String, Object>) props)); //$NON-NLS-1$
		}
	}
}