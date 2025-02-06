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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Dictionary;

import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;
import org.restlet.data.CharacterSet;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.representation.FileRepresentation;
import org.restlet.representation.Representation;
import org.restlet.representation.StringRepresentation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.osgi.AbstractConfiguredActivator;
import com.arcadsoftware.rest.JSONRepresentation;
import com.arcadsoftware.rest.JsonStreamCompact;
import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.rest.XMLRepresentation;

public class ConfigPublicListResource extends UserLinkedResource {

	public static final JsonStreamCompact js = new JsonStreamCompact(ConfigPublicListResource.class.getClassLoader(), true);
	
	static {
		js.alias("list", ArrayList.class); //$NON-NLS-1$
	}

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		getAllowedMethods().add(Method.GET);
		addVariants(MediaType.TEXT_XML, MediaType.APPLICATION_W3C_SCHEMA, MediaType.TEXT_HTML, MediaType.APPLICATION_XHTML);
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		Activator activator = Activator.getInstance();
		if (activator == null) {
			setExisting(false);
			return null;
		}
		ServiceReference<ConfigurationAdmin> ref = activator.getContext().getServiceReference(ConfigurationAdmin.class);
		if (ref == null) {
			activator.warn("No ConfigurationAdmin service reference (required for Rest resource /config/)");
			setExisting(false);
			return null;
		}
		ConfigurationAdmin configAdmin = activator.getContext().getService(ref);
		if (configAdmin == null) {
			activator.warn("No ConfigurationAdmin service (required for Rest resource /config/)");
			setExisting(false);
			return null;
		}
		Configuration[] configs;
		ArrayList<String> result = new ArrayList<String>();
		try {
			configs = configAdmin.listConfigurations(null);
			if (configs != null) {
				for (Configuration config: configs) {
					Dictionary<String, Object> props = config.getProperties();
					if (props.get(AbstractConfiguredActivator.PROP_PUBLIC_PROPS) != null) {
						result.add(config.getPid());
					}
				}
			}
		} catch (IOException | InvalidSyntaxException e) {
			activator.log(e);
			setExisting(false);
			return null;
		}
		Language language = getClientPreferedLanguage();
		if (isXSD(variant)) {
			return new FileRepresentation(activator.getBundleFile("schemas/configurations.xsd"), MediaType.APPLICATION_W3C_SCHEMA); //$NON-NLS-1$
		}
		if (isHTML(variant)) {
			String prefix = ""; //$NON-NLS-1$
			if (!getReference().getHierarchicalPart().endsWith("/")) {
				prefix = "config/"; //$NON-NLS-1$
			}
			StringBuilder res = new StringBuilder();
			res.append("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><title>Configurations list</title></head><body><p>"); //$NON-NLS-1$;
			res.append(activator.localize("config.list", language)); //$NON-NLS-1$
			res.append("</p>"); //$NON-NLS-1$
			for(String c:result) {
				res.append("<p><a href=\""); //$NON-NLS-1$
				res.append(prefix);
				res.append(c);
				res.append("\">"); //$NON-NLS-1$
				res.append(c);
				res.append("</a></p>"); //$NON-NLS-1$
			}
			res.append("</body></html>"); //$NON-NLS-1$	
			return new StringRepresentation(res.toString(), MediaType.TEXT_HTML, language, CharacterSet.UTF_8);
		}
		if (isXML(variant)) {
			return new XMLRepresentation(activator.getXStream().toXML(result), language);
		}
		if (isJSON(variant)) {
			return new JSONRepresentation(js.toXML(result),language);
		}
		return null;
	}
}
