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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;

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
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.arcadsoftware.rest.console.Category;
import com.arcadsoftware.rest.console.IActivableConsoleNode;
import com.arcadsoftware.rest.console.IRestConsoleSection;
import com.arcadsoftware.rest.console.ISecuredConsoleSection;
import com.arcadsoftware.rest.console.SectionId;
import com.arcadsoftware.rest.console.XmlConsoleStream;

public class SectionsListResource extends UserLinkedResource {

	private static final XmlConsoleStream xs = new XmlConsoleStream(SectionsListResource.class.getClassLoader());
	private static final JsonStreamCompact js = new JsonStreamCompact(SectionsListResource.class.getClassLoader(), true, true, true);
	
	static {
		//JSON...
		js.alias("categories", ArrayList.class); //$NON-NLS-1$
		js.alias("category", Category.class); //$NON-NLS-1$
		js.alias("section", SectionId.class); //$NON-NLS-1$
	}
	
	private Activator activator;
	private Language language;
	
	@Override
	protected void doInit() {
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
		getAllowedMethods().add(Method.GET);
		getAllowedMethods().add(Method.POST);
		setName(Messages.SectionsListResource_Name);
		setDescription(Messages.SectionsListResource_Description);
		addVariants(MediaType.TEXT_XML, MediaType.APPLICATION_W3C_SCHEMA);
	}

	@Override
	protected Representation post(Representation entity, Variant variant) {
		return get(variant);
	}

	@Override
	protected Representation get(Variant variant) {
		try {
			if (isXSD(variant)) {
				return new FileRepresentation(activator.getBundleFile("schemas/sections.xsd"), MediaType.APPLICATION_W3C_SCHEMA); //$NON-NLS-1$
			}
			HashMap<String, Category> list = new HashMap<String, Category>();
			IConnectionUserBean user = getUser();
			for (IRestConsoleSection s: activator.getSections()) {
				if (((s instanceof IActivableConsoleNode) && !((IActivableConsoleNode) s).isActivated()) ||
						((s instanceof ISecuredConsoleSection) && !((ISecuredConsoleSection) s).hasRight(user))) {
					continue;
				}
				String cat = s.getCategory(language);
				if (cat == null) {
					cat = ""; //$NON-NLS-1$
				}
				Category c = list.get(cat);
				if (c == null) {
					c = new Category();
					c.setLabel(cat);
					list.put(cat, c);
				}
				boolean notadded = true;
				SectionId sec = new SectionId();
				sec.setId(s.getId());
				sec.setIcon(s.getIcon());
				sec.setOrder(s.getOrder());
				sec.setLabel(s.getLabel(language));
				sec.setHelp(s.getHelp(language));
				if (s.getKeywords() != null) {
					sec.setKeywords(s.getKeywords().split(" ")); //$NON-NLS-1$
				}
				for (int i = 0; i < c.getList().size(); i++) {
					if (c.getList().get(i).getOrder() > sec.getOrder()) {
						c.getList().add(i, sec);
						notadded = false;
						break;
					}
				}
				if (notadded) {
					c.getList().add(sec);
				}
			}
			ArrayList<Category> categories = new ArrayList<Category>(list.size());
			categories.addAll(list.values());
			for (Category c: categories) {
				HashSet<String> kw = new HashSet<String>();
				for (SectionId sid: c.getList()) {
					if (sid.getKeywords() != null) {
						for (String k: sid.getKeywords()) {
							kw.add(k);
						}
					}
				}
				c.setKeywords(kw.toArray(new String[0]));
			}
			Collections.sort(categories);
			if (isXML(variant)) {
				return new XMLRepresentation(xs.toXML(categories),language);
			}
			if (isJSON(variant)) {
				return new JSONRepresentation(js.toXML(categories),language);
			}
			return null;
		} catch (Exception e) {
			activator.error(e);
			throw new ResourceException(Status.SERVER_ERROR_SERVICE_UNAVAILABLE, e);
		}
	}
}