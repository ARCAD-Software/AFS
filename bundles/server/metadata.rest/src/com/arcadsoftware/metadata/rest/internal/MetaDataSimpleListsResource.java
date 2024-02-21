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
package com.arcadsoftware.metadata.rest.internal;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.restlet.data.CharacterSet;
import org.restlet.data.Form;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.representation.Representation;
import org.restlet.representation.StringRepresentation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.xml.JSonBeanMapStream;
import com.arcadsoftware.beanmap.xml.XmlBeanMapStream;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataTest;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.criteria.AndCriteria;
import com.arcadsoftware.metadata.criteria.ConstantCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.xml.XmlCriteriaStream;
import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.rest.JSONRepresentation;
import com.arcadsoftware.rest.XMLRepresentation;

public class MetaDataSimpleListsResource extends UserLinkedResource {

	private static final String XSD_LISTSTART = "\t<xsd:element name=\"list\">\n\t\t<xsd:complexType>\n\t\t\t<xsd:choice minOccurs=\"0\" maxOccurs=\"unbounded\">"; //$NON-NLS-1$
	private static final String XSD_LISTELEMENT = "\n\t\t\t\t<xsd:element ref=\"%s\" />"; //$NON-NLS-1$
	private static final String XSD_LISTEND = 
			"\n\t\t\t</xsd:choice>" +  //$NON-NLS-1$
			"\n\t\t\t<xsd:attribute name=\"count\" type=\"xsd:positiveInteger\" use=\"required\">"+ //$NON-NLS-1$
			"\n\t\t\t\t<xsd:annotation><xsd:documentation>%1$s</xsd:documentation></xsd:annotation>\n\t\t\t</xsd:attribute>"+ //$NON-NLS-1$
			"\n\t\t\t<xsd:attribute name=\"rank\" type=\"xsd:integer\">"+ //$NON-NLS-1$
			"\n\t\t\t\t<xsd:annotation><xsd:documentation>%2$s</xsd:documentation></xsd:annotation>\n\t\t\t</xsd:attribute>"+ //$NON-NLS-1$
			"\n\t\t\t<xsd:attribute name=\"total\" type=\"xsd:positiveInteger\">"+ //$NON-NLS-1$
			"\n\t\t\t\t<xsd:annotation><xsd:documentation>%3$s</xsd:documentation></xsd:annotation>\n\t\t\t</xsd:attribute>"+ //$NON-NLS-1$
			"\n\t\t\t<xsd:attribute name=\"date\" type=\"xsd:string\">"+ //$NON-NLS-1$
			"\n\t\t\t\t<xsd:annotation><xsd:documentation>%4$s</xsd:documentation></xsd:annotation>\n\t\t\t</xsd:attribute>"+ //$NON-NLS-1$
			"\n\t\t</xsd:complexType>\n\t</xsd:element>\n"; //$NON-NLS-1$

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		setExisting(true);
		getAllowedMethods().add(Method.GET);
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		if (isXSD(variant)) {
			return getXSDFileRepresentation();
		}
		Language language = getClientPreferedLanguage();
		Form form = getRequest().getResourceRef().getQueryAsForm();
		ArrayList<BeanMapList> result = new ArrayList<BeanMapList>();
		// Get the result offset.
		int first = getFirst(form);
		int number = getPageCount(form, first);
		// check other parameters... 
		boolean deleted = isParameter(form,"deleted"); //$NON-NLS-1$
		boolean distincts = isParameter(form,"distincts"); //$NON-NLS-1$
		boolean translate = !isParameter(form,"notranslation"); //$NON-NLS-1$
		ISearchCriteria criteria = getCriteria(form);
		boolean nocriteria = ConstantCriteria.TRUE.equals(criteria) || (criteria == null);
		String order = getColumns(form, "orders"); //$NON-NLS-1$
		String attribute = getColumns(form, "attributes"); //$NON-NLS-1$
		for(MetaDataEntity entity:MetaDataEntity.loadEntities()) {
			if (entity.getType().startsWith("list/")) { //$NON-NLS-1$
				BeanMapList l = getList(entity, attribute, criteria, order, deleted, distincts, first, number, language, translate, nocriteria);
				if ((l != null) && (l.size() > 0)) {
					result.add(l);
				}
			}
		}
		// mise en forme du résultat.
		if (isJSON(variant)) {
			return getJSONRepresentation(result, language);
		}
		return getXMLRepresentation(result, language);
	}
	
	protected String getColumns(Form form, String key) {
		String result = form.getValues(key," ",true); //$NON-NLS-1$
		if (result == null) {
			result = getAttribute(key);
		}
		return result;
	}

	protected ISearchCriteria getCriteria(Form form) {
		String result = form.getFirstValue("criteria"); //$NON-NLS-1$
		if (result == null) {
			result = getAttribute("criteria"); //$NON-NLS-1$
		}
		if ((result == null) || 
				result.equalsIgnoreCase("all") ||  //$NON-NLS-1$
				result.equalsIgnoreCase("<all/>")) { //$NON-NLS-1$
			return ConstantCriteria.TRUE;
		}
		XmlCriteriaStream xs = new XmlCriteriaStream();
		return (ISearchCriteria) xs.fromXML(result);
	}

	protected int getPageCount(Form form, int first) {
		String s = form.getFirstValue("pagecount"); //$NON-NLS-1$
		if (s == null) {
			s = getAttribute("pagecount"); //$NON-NLS-1$
		}
		if (s == null) {
			s = form.getFirstValue("limit"); //$NON-NLS-1$
			if (s == null) {
				s = getAttribute("limit"); //$NON-NLS-1$
			}
			if (s != null) {
				try {
					return Integer.parseInt(s) - first;
				} catch (NumberFormatException e) {}
			}
		} else {
			try {
				return Integer.parseInt(s);
			} catch (NumberFormatException e) {}
		}
		return getDefaultPageCount(first);
	}

	protected int getDefaultPageCount(int first) {
		return 20;
	}

	protected boolean isParameter(Form form,String key) {
		if (form == null) {
			return false;
		}
		String s = form.getFirstValue(key);
		if (s == null) {
			s = getAttribute(key);
		}
		return (s != null);
	}

	protected int getFirst(Form form) {
		String s = form.getFirstValue("pagestart"); //$NON-NLS-1$
		if (s == null) {
			s = getAttribute("pagestart"); //$NON-NLS-1$
		}
		if (s == null) {
			s = form.getFirstValue("offset"); //$NON-NLS-1$
		}
		if (s == null) {
			s = getAttribute("offset"); //$NON-NLS-1$
		}
		if (s != null) {
			try {
				return Integer.parseInt(s);
			} catch (NumberFormatException e) {}
		}
		return 0;
	}

	protected Representation getXMLRepresentation(Object object, Language language) {
		return new XMLRepresentation(new XmlBeanMapStream().toXML(object), language);
	}

	protected Representation getJSONRepresentation(Object object, Language language) {
		return new JSONRepresentation(new JSonBeanMapStream().toXML(object), language);
	}

	private Representation getXSDFileRepresentation() {
		Language language = getClientPreferedLanguage();
		StringBuilder sbs = new StringBuilder();
		StringBuilder sbl = new StringBuilder(XSD_LISTSTART);
		int version = 1;
		for(MetaDataEntity entity:MetaDataEntity.loadEntities()) {
			if (entity.getType().startsWith("list/")) { //$NON-NLS-1$
				String type = entity.getType().replace(BeanMapList.SLASH_TYPE, BeanMapList.SLASH_TAG);
				sbl.append(String.format(XSD_LISTELEMENT, type));
				int i = entity.getVersion();
				if (i > version) {
					version = i;
				}
				MetaDataParentResource.	generateEntityXSD(sbs, type, entity.clone(language), language);

			}
		}
		sbl.append(String.format(XSD_LISTEND,
				Activator.getMessage("xsd.annotation.count", language), //$NON-NLS-1$
				Activator.getMessage("xsd.annotation.rank", language), //$NON-NLS-1$
				Activator.getMessage("xsd.annotation.total", language), //$NON-NLS-1$
				Activator.getMessage("xsd.annotation.date", language))); //$NON-NLS-1$
		return new StringRepresentation(
				String.format(MetaDataParentResource.XSD_HEADER, "lists", "lists", version) + //$NON-NLS-1$ //$NON-NLS-2$
				sbl.toString() + "\n" + sbs.toString() + //$NON-NLS-1$
				MetaDataParentResource.XSD_END, //
				MediaType.APPLICATION_W3C_SCHEMA, language, CharacterSet.UTF_8);
	}

	private BeanMapList getList(MetaDataEntity entity, String attribute, ISearchCriteria criteria, String order, boolean deleted, boolean distincts, int first, int number, Language language, boolean translate, boolean nocriteria) {
		// Build the requested attribute list.
		List<ReferenceLine> attributes;
		if (attribute == null) {
			attributes = entity.getListables();
		} else {
			attributes = entity.getAttributeLines(attribute); //$NON-NLS-1$
		}
		Iterator<ReferenceLine> itt = attributes.iterator();
		while(itt.hasNext()) {
			ReferenceLine att = itt.next();
			if (att.isEmpty()) {
				itt.remove();
			} else {
				MetaDataAttribute a = att.getLastAttribute();
				// TODO Ce test devrait être complété par un test sur toute la jointure !
				if (a.getRightRead(false) != null) {
					MetaDataEntity e = (MetaDataEntity)a.getParent();
					if (!e.getMapper().test(e, a.getRightRead(false), getUser())) {
						itt.remove();
					}
				}
			}
		}
		if (attributes.size() == 0) {
			attributes = entity.getListables();
		}
		// Build the requested sort order list.
		List<ReferenceLine> orders = entity.getAttributeLines(order); //$NON-NLS-1$
		itt = orders.iterator();
		while(itt.hasNext()) {
			ReferenceLine att = itt.next();
			if (att.isEmpty()) {
				itt.remove();
			} else {
				MetaDataAttribute a = att.getLastAttribute();
				if (a.getRightRead(false) != null) {
					// TODO Ce test devrait être complété par un test sur la jointure !
					MetaDataEntity e = (MetaDataEntity)a.getParent();
					if (!e.getMapper().test(e, a.getRightRead(false), getUser())) {
						itt.remove();
						continue;
					}
				}
				// Si la colonne n'est pas sélectionnée on l'ajoute.
				if (!attributes.contains(itt.next())) {
					attributes.add(att);
				}
			}
		}
		// Build the search criteria
		if (nocriteria) {
			criteria = entity.getRightList();
		} else {
			ISearchCriteria rc = entity.getRightList();
			if ((rc != null) && !ConstantCriteria.TRUE.equals(rc)) {
				criteria = new AndCriteria(criteria, rc);
			}
		}
		if (translate) {
			List<ReferenceLine> allattributes = new ArrayList<ReferenceLine>(attributes);
			for(ReferenceLine rl:attributes) {
				if (rl.isTranslatable()) {
					ReferenceLine trl = rl.getTranslateCode();
					if ((trl != null) && !allattributes.contains(trl)) {
						allattributes.add(trl);
					}
				}
			}
			attributes = allattributes;
		}
		// Process to selection...
		BeanMapList result = entity.dataSelection(attributes, deleted, criteria, distincts, orders, getUser(), first, number);
		if (nocriteria && (result.size() == 0)) {
			return null;
		}
		if (translate) {
			for(ReferenceLine rl:attributes) {
				if (rl.isTranslatable()) {
					for (BeanMap bm:result) {
						rl.translate(bm, language);
					}
				}
			}
		}
		Activator.getInstance().test(MetaDataTest.EVENTCODE_LIST,entity,result,getUser(),language);
		if (entity.getMetadata().getBoolean(MetaDataEntity.METADATA_EVENTONSELECTION)) {
			Activator.getInstance().fireSelectionEvent(entity,result,getUser());
		}
		return result;
	}

}
