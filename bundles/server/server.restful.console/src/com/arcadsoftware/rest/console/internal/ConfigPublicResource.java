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
import java.util.Collections;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

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

import com.arcadsoftware.rest.JSONRepresentation;
import com.arcadsoftware.rest.JsonStreamCompact;
import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.rest.XMLRepresentation;
import com.arcadsoftware.rest.XStreamCompact;
import com.arcadsoftware.rest.xml.HashMapBeanConverter;

public class ConfigPublicResource extends UserLinkedResource {
	
	public static final JsonStreamCompact js = new JsonStreamCompact();
	public static final XStreamCompact xs = new XStreamCompact();
	
	static {
		js.alias("config", Map.class); //$NON-NLS-1$
		js.registerConverter(new HashMapBeanConverter());
		xs.alias("config", Map.class); //$NON-NLS-1$
		xs.registerConverter(new HashMapBeanConverter());
	}

	@SuppressWarnings("rawtypes")
	private Dictionary props;
	private String requiredProp = null;
	private Activator activator;
	
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
			Configuration config = configAdmin.getConfiguration(pid, null);
			if (config != null) {
				props = config.getProperties();
			}
		} catch (IOException e) {
			activator.log(e);
			setExisting(false);
			return;
		}
		if (props == null) {
			setExisting(false);
			return;
		}
		requiredProp = getAttribute("prop"); //$NON-NLS-1$
		if (requiredProp != null) {
			if (props.get(requiredProp) == null) {
				activator.log("Config Property is not defined: " + requiredProp);
				setExisting(false);
				return;
			}
		}
		getAllowedMethods().add(Method.GET);
		addVariants(MediaType.TEXT_XML, MediaType.APPLICATION_W3C_SCHEMA, MediaType.APPLICATION_XHTML, MediaType.TEXT_HTML);
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		Language language = getClientPreferedLanguage();
		if (isXSD(variant)) {
			return new FileRepresentation(activator.getBundleFile("schemas/configuration.xsd"), MediaType.APPLICATION_W3C_SCHEMA); //$NON-NLS-1$
		}
		if (props == null) {
			props = new Properties();
		}
		HashMap<String, String> result = new HashMap<String, String>(props.size());
		ArrayList<String> publics = new ArrayList<String>(props.size());
		Object p = props.get(Activator.PROP_PUBLIC_PROPS);
		if (p != null) {
			Collections.addAll(publics, p.toString().split(" ")); //$NON-NLS-1$
		}
		@SuppressWarnings("rawtypes")
		Enumeration enu = props.keys();
		while(enu.hasMoreElements()) {
			Object k = enu.nextElement();
			if (publics.contains(k) && ((requiredProp == null) || requiredProp.equals(k))) {
				Object v = props.get(k);
				if (v != null) {
					result.put(k.toString(),v.toString());
				}
			}
		}
		if (isHTML(variant)) {
			StringBuilder res = new StringBuilder();
			res.append("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><title>Configurations list</title></head><body><p>"); //$NON-NLS-1$;
			res.append(getAttribute("pid"));
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
			return new XMLRepresentation(xs.toXML(result), language);
		}
		if (isJSON(variant)) {
			return new JSONRepresentation(js.toXML(result),language);
		}
		return null;
	}
}
