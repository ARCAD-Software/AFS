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

import java.io.IOException;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Properties;
import java.util.Map.Entry;

import org.osgi.framework.Constants;
import org.osgi.framework.ServiceReference;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;
import org.restlet.data.CharacterSet;
import org.restlet.data.Form;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.representation.FileRepresentation;
import org.restlet.representation.Representation;
import org.restlet.representation.StringRepresentation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.rest.JSONRepresentation;
import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.rest.XMLRepresentation;

public class ConfigResource extends UserLinkedResource {

	private Activator activator;
	private Configuration config;
	
	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		activator = Activator.getInstance();
		if (activator == null) {
			setExisting(false);
			return;
		}
		ServiceReference<ConfigurationAdmin> ref = activator.getContext().getServiceReference(ConfigurationAdmin.class);
		if (ref == null) {
			activator.warn("No ConfigurationAdmin service reference (required for Rest resource /config/)");
			setExisting(false);
			return;
		}
		ConfigurationAdmin configAdmin = activator.getContext().getService(ref);
		if (configAdmin == null) {
			activator.warn("No ConfigurationAdmin service (required for Rest resource /config/)");
			setExisting(false);
			return;
		}
		String pid = getAttribute("pid"); //$NON-NLS-1$
		if (pid == null) {
			setExisting(false);
			return;
		}
		try {
			config = configAdmin.getConfiguration(pid, null);
			if (config == null) {
				setExisting(false);
				return;
			}
		} catch (IOException e) {
			activator.log(e);
			setExisting(false);
			return;
		}
		if (hasRight(6)) {
			getAllowedMethods().add(Method.GET);
		}
		if (hasRight(7)) {
			getAllowedMethods().add(Method.PUT);
			getAllowedMethods().add(Method.POST);
			getAllowedMethods().add(Method.DELETE);
		}
		addVariants(MediaType.TEXT_XML, MediaType.APPLICATION_W3C_SCHEMA, MediaType.APPLICATION_XHTML, MediaType.TEXT_HTML);
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		if (!hasRight(6)) {
			setStatus(Status.CLIENT_ERROR_FORBIDDEN);
			return null;
		}
		Language language = getClientPreferedLanguage();
		if (isXSD(variant)) {
			return new FileRepresentation(activator.getBundleFile("schemas/configuration.xsd"), MediaType.APPLICATION_W3C_SCHEMA); //$NON-NLS-1$
		}
		@SuppressWarnings("rawtypes")
		Dictionary props = config.getProperties();
		if (props == null) {
			props = new Properties();
		}
		HashMap<String, String> result = new HashMap<String, String>(props.size());
		@SuppressWarnings("rawtypes")
		Enumeration enu = props.keys();
		while(enu.hasMoreElements()) {
			Object k = enu.nextElement();
			Object v = props.get(k);
			if (v != null) {
				result.put(k.toString(),v.toString());
			}
		}
		if (isHTML(variant)) {
			StringBuilder res = new StringBuilder();
			res.append("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><title>Configurations list</title></head><body><p>"); //$NON-NLS-1$;
			res.append(getAttribute("pid")); //$NON-NLS-1$
			res.append("</p><table border=\"1\">"); //$NON-NLS-1$
			for(Entry<String, String> e:result.entrySet()) {
				res.append("<tr><td><strong>"); //$NON-NLS-1$
				res.append(e.getKey());
				res.append("</strong></td><td>"); //$NON-NLS-1$
				res.append(e.getValue());
				res.append("</td></tr>"); //$NON-NLS-1$
			}
			res.append("</table></body></html>"); //$NON-NLS-1$	
			return new StringRepresentation(res.toString(), MediaType.TEXT_HTML, language, CharacterSet.UTF_8);
		}
		if (isXML(variant)) {
			return new XMLRepresentation(ConfigPublicResource.xs.toXML(result), language);
		}
		if (isJSON(variant)) {
			return new JSONRepresentation(ConfigPublicResource.js.toXML(result),language);
		}
		return null;
	}

	@Override
	protected Representation delete(Variant variant) throws ResourceException {
		if (!hasRight(7)) {
			setStatus(Status.CLIENT_ERROR_FORBIDDEN);
			return null;
		}
		Hashtable<String, Object> props = new Hashtable<String, Object>();
		props.put(Constants.SERVICE_PID, getAttribute("pid")); //$NON-NLS-1$
		try {
			config.update(props);
		} catch (IOException e) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, e.getLocalizedMessage());
		}
		setStatus(Status.SUCCESS_NO_CONTENT);
		return null;
	}

	@Override
	protected Representation post(Representation entity, Variant variant) throws ResourceException {
		return put(entity, variant);
	}

	@Override
	protected Representation put(Representation representation, Variant variant) throws ResourceException {
		if (!hasRight(7)) {
			setStatus(Status.CLIENT_ERROR_FORBIDDEN);
			return null;
		}
		Form form = getRequestForm();
		Dictionary<String, Object> props = config.getProperties();
		if (props == null) {
			props = new Hashtable<String, Object>();
			props.put(Constants.SERVICE_PID, getAttribute("pid")); //$NON-NLS-1$
		}
		for(String key : form.getNames()) {
			if (!Constants.SERVICE_PID.equals(key)) {
				props.put(key, form.getFirstValue(key));
			}
		}
		try {
			config.update(props);
		} catch (IOException e) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, e.getLocalizedMessage());
		}
		setStatus(Status.SUCCESS_NO_CONTENT, activator.localize("info.success", getClientPreferedLanguage())); //$NON-NLS-1$
		return null;
	}
}
