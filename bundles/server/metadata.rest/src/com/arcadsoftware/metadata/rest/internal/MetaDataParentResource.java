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
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.restlet.data.CharacterSet;
import org.restlet.data.Form;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.representation.Representation;
import org.restlet.representation.StringRepresentation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapPartialList;
import com.arcadsoftware.metadata.IByPassListener;
import com.arcadsoftware.metadata.IMetaDataDeleteListener;
import com.arcadsoftware.metadata.IMetaDataModifyListener;
import com.arcadsoftware.metadata.IMetaDataSelectionListener;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.MetaDataTest;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.criteria.AndCriteria;
import com.arcadsoftware.metadata.criteria.ConstantCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.rest.DataParentResource;
import com.arcadsoftware.metadata.xml.XmlCriteriaStream;

/*
 * Gestion des données :
 * 
 * /data/{type}
 * - GET: Sélection de tout ou parties des données. Supporte tous les paramètres de sélection (dont le critère et les jointures sur attributs).
 * - POST: Création d'une nouvelle données. renvoie cette données telle qu'elle a été créée.
 * - PUT: subtitution de GET.
 * 
 */
public class MetaDataParentResource extends DataParentResource {

	public static final String XSD_HEADER = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + //$NON-NLS-1$
			"\"<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" " + //$NON-NLS-1$ 
			"targetNamespace=\"http://xml.arcadsoftware.com/metadata/%1$s/%3$d/%2$s.xsd\" " + //$NON-NLS-1$ 
			"xmlns=\"http://xml.arcadsoftware.com/metadata/%1$s/%3$d/%2$s.xsd\" elementFormDefault=\"qualified\">\n"; //$NON-NLS-1$
	private static final String XSD_LIST = "\t<xsd:element name=\"%1$s\">\n\t\t<xsd:complexType>" + //$NON-NLS-1$
			"\n\t\t\t<xsd:choice minOccurs=\"0\" maxOccurs=\"unbounded\"><xsd:element ref=\"%2$s\" /></xsd:choice>" +  //$NON-NLS-1$
			"\n\t\t\t<xsd:attribute name=\"count\" type=\"xsd:positiveInteger\" use=\"required\">"+ //$NON-NLS-1$
			"\n\t\t\t\t<xsd:annotation><xsd:documentation>%3$s</xsd:documentation></xsd:annotation>\n\t\t\t</xsd:attribute>"+ //$NON-NLS-1$
			"\n\t\t\t<xsd:attribute name=\"rank\" type=\"xsd:integer\">"+ //$NON-NLS-1$
			"\n\t\t\t\t<xsd:annotation><xsd:documentation>%4$s</xsd:documentation></xsd:annotation>\n\t\t\t</xsd:attribute>"+ //$NON-NLS-1$
			"\n\t\t\t<xsd:attribute name=\"total\" type=\"xsd:positiveInteger\">"+ //$NON-NLS-1$
			"\n\t\t\t\t<xsd:annotation><xsd:documentation>%5$s</xsd:documentation></xsd:annotation>\n\t\t\t</xsd:attribute>"+ //$NON-NLS-1$
			"\n\t\t\t<xsd:attribute name=\"date\" type=\"xsd:string\">"+ //$NON-NLS-1$
			"\n\t\t\t\t<xsd:annotation><xsd:documentation>%6$s</xsd:documentation></xsd:annotation>\n\t\t\t</xsd:attribute>"+ //$NON-NLS-1$
			"\n\t\t</xsd:complexType>\n\t</xsd:element>\n"; //$NON-NLS-1$
	private static final String XSD_ITEM_START = "\t<xsd:element name=\"%1$s\">\n\t\t<xsd:annotation>"+ //$NON-NLS-1$
			"<xsd:documentation xml:lang=\"%2$s\">%3$s</xsd:documentation></xsd:annotation>"+//$NON-NLS-1$
			"\n\t\t<xsd:complexType>\n\t\t\t<xsd:choice minOccurs=\"0\" maxOccurs=\"unbounded\">\n"; //$NON-NLS-1$
	private static final String XSD_ITEM_ELEMENT = "\t\t\t\t<xsd:element name=\"%1$s\" type=\"xsd:string\" minOccurs=\"0\" maxOccurs=\"1\">" + //$NON-NLS-1$
			"\n\t\t\t\t\t<xsd:annotation><xsd:documentation xml:lang=\"%2$s\">%3$s</xsd:documentation></xsd:annotation>\n\t\t\t\t</xsd:element>\n"; //$NON-NLS-1$
	private static final String XSD_ITEM_ATTRIBUTE = "\t\t\t<xsd:attribute name=\"%1$s\" type=\"%2$s\">" + //$NON-NLS-1$
			"\n\t\t\t\t<xsd:annotation><xsd:documentation xml:lang=\"%3$s\">%4$s</xsd:documentation></xsd:annotation>\n\t\t\t</xsd:attribute>\n"; //$NON-NLS-1$
	private static final String XSD_ITEM_ENDCHOICE = "\t\t\t</xsd:choice>" + //$NON-NLS-1$
			"\n\t\t\t<xsd:attribute name=\"id\" type=\"xsd:positiveInteger\" use=\"required\">"+ //$NON-NLS-1$
			"\n\t\t\t\t<xsd:annotation><xsd:documentation xml:lang=\"%5$s\">%1$s</xsd:documentation></xsd:annotation>\n\t\t\t</xsd:attribute>"+ //$NON-NLS-1$
			"\n\t\t\t<xsd:attribute name=\"type\" type=\"xsd:positiveInteger\">"+ //$NON-NLS-1$
			"\n\t\t\t\t<xsd:annotation><xsd:documentation xml:lang=\"%5$s\">%2$s</xsd:documentation></xsd:annotation>\n\t\t\t</xsd:attribute>"+ //$NON-NLS-1$
			"\n\t\t\t<xsd:attribute name=\"deleted\" type=\"xsd:boolean\">"+ //$NON-NLS-1$
			"\n\t\t\t\t<xsd:annotation><xsd:documentation xml:lang=\"%5$s\">%3$s</xsd:documentation></xsd:annotation>\n\t\t\t</xsd:attribute>"+ //$NON-NLS-1$
			"\n\t\t\t<xsd:attribute name=\"date\" type=\"xsd:string\">"+ //$NON-NLS-1$
			"\n\t\t\t\t<xsd:annotation><xsd:documentation xml:lang=\"%5$s\">%4$s</xsd:documentation></xsd:annotation>\n\t\t\t</xsd:attribute>\n"; //$NON-NLS-1$
	
	public static final String XSD_END = "\t</xsd:element>\n</xsd:schema>"; //$NON-NLS-1$
	
	/**
	 * Generate the XSD document associated to the given entity.
	 * @param entity
	 * @param language
	 * @return
	 */
	protected static Representation getXSDFileRepresentation(MetaDataEntity entity, Language language) {
		entity = entity.clone(language);
		StringBuilder sb = new StringBuilder(2000 + (200 * entity.getAttributes().size()));
		String type = entity.getType().replace(BeanMapList.SLASH_TYPE, BeanMapList.SLASH_TAG);
		String entities = BeanMapList.getListTag(type);
		sb.append(String.format(XSD_HEADER, entity.getDomain().replace(':', '_'), entities, entity.getVersion()));
		sb.append(String.format(XSD_LIST, "list", type, //$NON-NLS-1$ 
				Activator.getMessage("xsd.annotation.count", language), //$NON-NLS-1$
				Activator.getMessage("xsd.annotation.rank", language), //$NON-NLS-1$
				Activator.getMessage("xsd.annotation.total", language), //$NON-NLS-1$
				Activator.getMessage("xsd.annotation.date", language))); //$NON-NLS-1$
		sb.append(String.format(XSD_LIST, entities, type, //
				Activator.getMessage("xsd.annotation.count", language), //$NON-NLS-1$
				Activator.getMessage("xsd.annotation.rank", language), //$NON-NLS-1$
				Activator.getMessage("xsd.annotation.total", language), //$NON-NLS-1$
				Activator.getMessage("xsd.annotation.date", language))); //$NON-NLS-1$
		generateEntityXSD(sb, type, entity, language);
		StringRepresentation rep = new StringRepresentation(sb, MediaType.APPLICATION_W3C_SCHEMA, language, CharacterSet.UTF_8);
		rep.setModificationDate(entity.getDate());
		return rep;
	}
	
	/**
	 * Generate an XSD documentation of the given entity. 
	 * 
	 * <p>
	 * this entity object must be translated (i.e. clone(Language) ) if the entity documentation must be include into the XSD document.
	 *   
	 * @param sbs A StringBuilder that will contain the entity XSD declaration.
	 * @param xmlType The entity Type as represented into XML file.
	 * @param entity The translated Entity.
	 * @param language The Language.
	 */
	public static void generateEntityXSD(StringBuilder sbs, String xmlType, MetaDataEntity entity, Language language) {
		String doc = entity.getName();
		if (entity.getDescription() != null) {
			if (doc != null) {
				doc = "<h3>" + doc + "</h3>\n" + xhtmlText(entity.getDescription()); //$NON-NLS-1$ //$NON-NLS-2$
			} else {
				doc = xhtmlText(entity.getDescription());
			}
		}
		sbs.append(String.format(XSD_ITEM_START, xmlType, language.getName(), doc));
		StringBuilder sbAtt = new StringBuilder(200 * entity.getAttributes().size());
		for(MetaDataAttribute a:entity.getAttributes().values()) {
			String code = a.getCode();
			doc = a.getName();
			if (entity.getDescription() != null) {
				if (doc != null) {
					doc = "<h3>" + doc + "</h3>\n" + xhtmlText(a.getDescription()); //$NON-NLS-1$ //$NON-NLS-2$
				} else {
					doc = xhtmlText(a.getDescription());
				}
			}
			if (BeanMap.KEY_TYPE.equals(code) || BeanMap.KEY_DATE.equals(code) || BeanMap.KEY_ID.equals(code) || //
					BeanMap.KEY_DELETED.equals(code)) {
				sbs.append(String.format(XSD_ITEM_ELEMENT, code, language.getName(), doc));
			} else if (MetaDataAttribute.TYPE_INTEGER.equals(a.getType()) || MetaDataAttribute.TYPE_INT.equals(a.getType())) {
				sbAtt.append(String.format(XSD_ITEM_ATTRIBUTE, code, "xsd:integer", language.getName(), doc)); //$NON-NLS-1$
			} else if (!a.isBoolean()) {
				sbAtt.append(String.format(XSD_ITEM_ATTRIBUTE, code, "xsd:boolean", language.getName(), doc)); //$NON-NLS-1$
			} else if (!a.isDate()) {
				sbAtt.append(String.format(XSD_ITEM_ATTRIBUTE, code, "xsd:string", language.getName(), doc)); //$NON-NLS-1$
			} else {
				sbs.append(String.format(XSD_ITEM_ELEMENT, code, language.getName(), doc));
			}
		}
		sbs.append(String.format(XSD_ITEM_ENDCHOICE, //
				Activator.getMessage("xsd.annotation.id", language), //$NON-NLS-1$
				Activator.getMessage("xsd.annotation.type", language), //$NON-NLS-1$
				Activator.getMessage("xsd.annotation.deleted", language), //$NON-NLS-1$
				Activator.getMessage("xsd.annotation.dateitem", language), //$NON-NLS-1$
				language.getName()));
		sbs.append(sbAtt);
		sbs.append(XSD_END);
	}
	
	protected static String xhtmlText(String text) {
		return "<p>" + //$NON-NLS-1$ 
				text.replace("<", "&lt;") //$NON-NLS-1$ //$NON-NLS-2$
					.replace(">", "&gt;") //$NON-NLS-1$ //$NON-NLS-2$
					.replace("\n", "<p></p>") + //$NON-NLS-1$ //$NON-NLS-2$
				"</p>"; //$NON-NLS-1$
	}

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		if (isExisting()) {
			getAllowedMethods().add(Method.PUT);
			if (Method.HEAD.equals(getMethod()) && // 
					(getEntity().getMetadata().getBoolean(MetaDataEntity.METADATA_UPDATABLE) || //
							(getEntity().getMetadata().get("updateCol") != null))) { //$NON-NLS-1$
				computeLastModificationDate();
			}
		}
	}

	private void computeLastModificationDate() {
		final Form form = getRequestForm();
		ISearchCriteria criteria = getCriteria(form);
		if (ConstantCriteria.TRUE.equals(criteria) || (criteria == null)) {
			criteria = getEntity().getRightList();
		} else {
			ISearchCriteria rc = getEntity().getRightList();
			if ((rc != null) && !ConstantCriteria.TRUE.equals(rc)) {
				criteria = new AndCriteria(criteria, rc);
			}
		}
		// Get the result offset.
		int first = getFirst(form);
		int number = getPageCount(form, first);
		// check other parameters... 
		boolean deleted = isParameter(form, "deleted"); //$NON-NLS-1$
		boolean distincts = isParameter(form, "distincts"); //$NON-NLS-1$
		List<ReferenceLine> orders = getEntity().getPublicAttributeLines(getColumns(form, "orders")); //$NON-NLS-1$
		Iterator<ReferenceLine> itt = orders.iterator();
		while (itt.hasNext()) {
			ReferenceLine att = itt.next();
			if (att.isEmpty()) {
				itt.remove();
			} else {
				MetaDataAttribute a = att.getLastAttribute();
				if ((a != null) && (a.getRightRead(false) != null)) {
					// TODO Ce test devrait être complété par un test sur la jointure !
					MetaDataEntity e = (MetaDataEntity) a.getParent();
					if (!e.getMapper().test(e, a.getRightRead(false), getUser())) {
						itt.remove();
						continue;
					}
				}
			}
		}
		BeanMapList result = getEntity().dataSelection(orders, deleted, criteria, distincts, orders, getUser(), first, number);
		Date date = new Date(0l);
		// Return EPOCH for empty lists...
		if (result.size() > 0) {
			for (IMetaDataSelectionListener listener: Activator.getInstance().getSelectionListener(getEntity().getType())) {
				listener.onSelection(getEntity(), result, getUser(), Language.ENGLISH_US);
			}
			for (BeanMap b: result) {
				Date d = b.getDate();
				if ((d != null) && date.before(d)) {
					date = d;
				}
			}
		}
		setLastModification(date);
	}

	@Override
	protected Representation put(Representation representation, Variant variant) throws ResourceException {
		// The default implementation is to switch to a "GET" request.
		// If a true "multi update" method is required we can add a required field to identify and "update".
		// For instance "order" may identify a selection (but this is optional parameter).
		return list(variant);
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		if (isXSD(variant)) {
			return getXSDFileRepresentation(getEntity(), getClientPreferedLanguage());
		}
		return list(variant);
	}

	protected Representation list(Variant variant) {
		Language language = getClientPreferedLanguage();
		final Form form = getRequestForm();
		// Build the requested attribute list.
		String atts = getColumns(form, "attributes"); //$NON-NLS-1$ 
		List<ReferenceLine> attributes;
		if (atts == null) {
			attributes = getEntity().getListables();
		} else {
			attributes = getEntity().getPublicAttributeLines(atts);
		}
		Iterator<ReferenceLine> itt = attributes.iterator();
		while (itt.hasNext()) {
			ReferenceLine att = itt.next();
			if (att.isEmpty()) {
				itt.remove();
			} else {
				MetaDataAttribute a = att.getLastAttribute();
				// TODO Test all attributes from the line... 
				if ((a != null) && (a.getRightRead(false) != null)) {
					MetaDataEntity e = (MetaDataEntity) a.getParent();
					if (!e.getMapper().test(e, a.getRightRead(false), getUser())) {
						itt.remove();
					}
				}
			}
		}
		// Build the requested sort order list.
		List<ReferenceLine> orders;
		if (attributes.size() > 0) {
			orders = getEntity().getPublicAttributeLines(getColumns(form, "orders")); //$NON-NLS-1$
			itt = orders.iterator();
			while (itt.hasNext()) {
				ReferenceLine att = itt.next();
				if (att.isEmpty()) {
					itt.remove();
				} else {
					MetaDataAttribute a = att.getLastAttribute();
					if ((a != null) && (a.getRightRead(false) != null)) {
						// TODO Ce test devrait être complété par un test sur la jointure !
						MetaDataEntity e = (MetaDataEntity) a.getParent();
						if (!e.getMapper().test(e, a.getRightRead(false), getUser())) {
							itt.remove();
							continue;
						}
					}
					// Si la colonne n'est pas sélectionnée on l'ajoute.
					if (!attributes.contains(att)) {
						attributes.add(att);
					}
				}
			}
		} else {
			orders = new ArrayList<ReferenceLine>();
		}
		// Build the search criteria
		ISearchCriteria criteria = getCriteria(form);
		if (ConstantCriteria.TRUE.equals(criteria) || (criteria == null)) {
			criteria = getEntity().getRightList();
		} else {
			ISearchCriteria rc = getEntity().getRightList();
			if ((rc != null) && !ConstantCriteria.TRUE.equals(rc)) {
				criteria = new AndCriteria(criteria, rc);
			}
		}
		// Get the result offset.
		int first = getFirst(form);
		int number = getPageCount(form, first);
		// check other parameters... 
		boolean deleted = isParameter(form, "deleted"); //$NON-NLS-1$
		boolean distincts = isParameter(form, "distincts"); //$NON-NLS-1$
		boolean translate = !isParameter(form, "notranslation"); //$NON-NLS-1$
		if (translate) {
			List<ReferenceLine> allattributes = new ArrayList<ReferenceLine>(attributes);
			for(ReferenceLine rl: attributes) {
				if (rl.isTranslatable()) {
					ReferenceLine trl = rl.getTranslateCode();
					if ((trl != null) && !allattributes.contains(trl)) {
						allattributes.add(trl);
					}
				}
			}
			attributes = allattributes;
		}
		// first of all count the total selection...
		int count = getEntity().getMapper().count(getEntity(), deleted, criteria, distincts, getUser());
		// Process to selection...
		BeanMapList result = getEntity().getMapper().selection(getEntity(), attributes, deleted, criteria, distincts, orders, getUser(), first, number);
		if (result instanceof BeanMapPartialList) {
			((BeanMapPartialList) result).setTotal(count);
			((BeanMapPartialList) result).setRank(first);			
		}
		if (translate) {
			for(ReferenceLine rl:attributes) {
				if (rl.isTranslatable()) {
					for (BeanMap bm: result) {
						rl.translate(bm, language);
					}
				}
			}
		}
		// Post selection management
		Activator.getInstance().test(MetaDataTest.EVENTCODE_LIST, getEntity(), result, getUser(), language);
		if (getEntity().getMetadata().getBoolean(MetaDataEntity.METADATA_EVENTONSELECTION)) {
			Activator.getInstance().fireSelectionEvent(getEntity(), result, getUser());
		}
		// Add a parameter "links" to insert all links into the results list...
		String links = getColumns(form, "links"); //$NON-NLS-1$
		if ((links == null) || links.isEmpty()) {
			// legacy:
			links = getColumns(form, "attributes"); //$NON-NLS-1$
		}
		if ((links != null) && !links.isEmpty()) {
			for (String l: links.split(" ")) { //$NON-NLS-1$
				MetaDataLink link = getEntity().getLink(l);
				if (link != null) {
					addLinkSelection(result, link, translate, deleted, language);
				}
			}
		}
		if (result.size() > 0) {
			for (IMetaDataSelectionListener listener: Activator.getInstance().getSelectionListener(getEntity().getType())) {
				listener.onSelection(getEntity(), result, getUser(), language);
			}
		}
		// Format the resulting response.
		return getRepresentation(variant, form, result, language);
	}

	private void addLinkSelection(BeanMapList result, MetaDataLink link, boolean translate, boolean deleted, Language language) {
		MetaDataEntity linkEntity = link.getRefEntity();
		List<ReferenceLine> linkAttributes = linkEntity.getAllPublicAttributes();
		if (translate) {
			List<ReferenceLine> allattributes = new ArrayList<ReferenceLine>(linkAttributes);
			for (ReferenceLine rl: linkAttributes) {
				if (rl.isTranslatable()) {
					ReferenceLine trl = rl.getTranslateCode();
					if ((trl != null) && !allattributes.contains(trl)) {
						allattributes.add(trl);
					}
				}
			}
			linkAttributes = allattributes;
		}
		ISearchCriteria linkcriteria = link.getRightList(true);
		for (BeanMap r: result) {
			BeanMapList list = getEntity().getMapper().linkSelection(link, r.getId(), linkAttributes, deleted, linkcriteria, false, null, getUser(), 0, -1);
			if (list != null) {
				if (translate) {
					for (ReferenceLine rl: linkAttributes) {
						if (rl.isTranslatable()) {
							for (BeanMap bm: result) {
								rl.translate(bm, language);
							}
						}
					}
				}
				Activator.getInstance().test(MetaDataTest.EVENTCODE_LIST, linkEntity, list, getUser(), language);
				if (getEntity().getMetadata().getBoolean(MetaDataEntity.METADATA_EVENTONSELECTION)) {
					Activator.getInstance().fireSelectionEvent(linkEntity, list, getUser());
				}
				if (list.size() > 0) {
					for (IMetaDataSelectionListener listener: Activator.getInstance().getSelectionListener(linkEntity.getType())) {
						listener.onSelection(linkEntity, list, getUser(), language);
					}
				}
				r.put(link.getCode(), list);
			}
		}
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
	
	protected String getColumns(Form form, String key) {
		String result = form.getValues(key, " ", true); //$NON-NLS-1$
		if (result == null) {
			result = getAttribute(key);
		}
		return result;
	}

	@Override
	protected Representation post(Representation representation, Variant variant) throws ResourceException {
		Language language = getClientPreferedLanguage();
		// Test des droits:
		if (getEntity().isReadOnly()) {
			setStatus(Status.CLIENT_ERROR_FORBIDDEN, Activator.getMessage("error.readonly", language)); //$NON-NLS-1$
			return null;
		}
		if (!getEntity().getMapper().test(getEntity(), getEntity().getRightCreate(), getUser())) {
			setStatus(Status.CLIENT_ERROR_FORBIDDEN, Activator.getMessage("right.nocreate", language)); //$NON-NLS-1$
			return null;
		}
		// Obtention de l'élément tel qu'il doit devrait créé (tel qu'il est passé à la resource).
		Form form = getRequestForm();
		ArrayList<MetaDataAttribute> list = new ArrayList<MetaDataAttribute>();
		BeanMap result = getEntity().formToBean(form, list);
		// Test des droits sur attributs.
		Iterator<MetaDataAttribute> itt = list.iterator();
		while (itt.hasNext()) {
			MetaDataAttribute att = itt.next();
			if ((att.getRightUpdate(false) != null) && //
					!getMapper().test(getEntity(), att.getRightUpdate(false), getUser())) {
				// Si un attribut n'est pas modifiable on le supprime de la liste.
				itt.remove();
				result.remove(att.getCode());
			}
		}
		// Phase de test des attributs et des valeurs qui leur sont affectées.
		// A. Application des listeners et des contraintes de type Mandatory.
		List<IMetaDataModifyListener> listeners = Activator.getInstance().getModifyListener(getType());
		switch (doCreateTest(listeners, result, list, language)) {
		case 0:
			// Contraintes de type "Mandatory" non respectées, ou echec des listener ou du Script de test.
			// if this item already exist, just return it to sender...
			if (result.getId() > 0) {
				removeHiddenAttributes(result);
				return getRepresentation(variant, form, result, language);
			}
			getResponse().setStatus(Status.CLIENT_ERROR_BAD_REQUEST, Activator.getMessage("error.missingattributes", language)); //$NON-NLS-1$
			return null;
		case 1:
			// B. Validation de l'unicité des valeurs.
			for (MetaDataAttribute attribute: list) {
				if (attribute.getMetadata().getBoolean(MetaDataEntity.METADATA_UNIQUE)) {
					Object value = result.get(attribute.getCode());
					if ((value != null) && (value.toString().length() > 0) && // On ignore les affectation à null.
							(getEntity().dataCount(false, attribute.getCode(), value) > 0)) {
						throw new ResourceException(Status.CLIENT_ERROR_PRECONDITION_FAILED, //
								String.format(Activator.getMessage("error.uniqueattribute.create", language), //$NON-NLS-1$ 
										attribute.getName(language), value, attribute.getCode()));
					}
				}
			}
			// TODO Detect any relative data and cascade the creation
			// TODO (for instance user.firstname = "x" create a user with firstname = "x" and link it through with user = id)
			// TODO Use getEntity().formToBean(form, list, true, false false);
			// TODO And then look for sub BeanMap in the result BeanMap...
			// TODO Process each of then (recursively, as it may include property like "subcode.subsubcode")  
			
			// Item creation.
			result = getMapper().create(getEntity(),list,getEntity().getValues(list,result));
		}
		if ((result == null) || (result.getId() == 0)) {
			// Echec de la création.
			getResponse().setStatus(Status.CLIENT_ERROR_BAD_REQUEST, Activator.getMessage("error.badattributes", language)); //$NON-NLS-1$
			return null;
		}
		try {
			doPostCreateTreatment(listeners, result, list, language);
		} catch (Exception e) {
			getMapper().delete(result, true);
			Activator.getInstance().debug(e);
			getResponse().setStatus(Status.SERVER_ERROR_INTERNAL, e);
			return null;
		}
		// Mise en forme du résultat.
		return getRepresentation(variant, form, result, language);
	}

	private void removeHiddenAttributes(BeanMap result) {
		for(MetaDataAttribute att: getEntity().getAttributes().values()) {
			if (!att.isPublic()) {
				result.remove(att.getCode());
			}
		}
	}

	/**
	 * Return 0 if the operation must be aborted, +1 if the operation can by proceeded or -1 if the operation must be ignored.
	 * 
	 * @param listeners
	 * @param result
	 * @param list
	 * @param language
	 * @return
	 */
	private int doCreateTest(List<IMetaDataModifyListener> listeners, BeanMap result, ArrayList<MetaDataAttribute> list, Language language) {
		MetaDataEntity entity = getEntity();
		// Avant tout, appel des Listeners qui peuvent rétablir une situation non conforme.
		boolean byPass = false;
		for (IMetaDataModifyListener listener: listeners) {
			if (!listener.testModification(entity, null, result, list, getUser(), language)) {
				return 0;
			}
			if (listener instanceof IByPassListener) {
				byPass = true;
			}
		}
		// Appel du script de test de validité de l'Entity.
		try {
			if (!Activator.getInstance().test(MetaDataTest.EVENTCODE_BEFORECREATE, entity, result, list, getUser(), language)) {
				// Ce script peut soit déclencher une ResourceException
				// Soit renvoyer false pour signifier un problème de type "mandatory" !  
				return 0;
			}
		} catch (ResourceException e) {
			if (e.getStatus().equals(Status.SUCCESS_OK)) {
				byPass = true;
			} else {
				throw e;
			}
		}
		// Mandatory and Unique attributes...
		for (MetaDataAttribute a: entity.getAttributes().values()) {
			// Un Mandatory absent termine le test.
			if (a.isMandatory() && (list.indexOf(a) < 0)) {
				return 0;
			}
		}
		if (byPass) {
			return -1;
		}
		return 1;
	}

	private void doPostCreateTreatment(List<IMetaDataModifyListener> listeners, BeanMap result, ArrayList<MetaDataAttribute> list, Language language) throws Exception {
		// Translate ? (en théorie les attributs translate ne sont pas modifiés ici !)
		MetaDataEntity entity = getEntity();
		for(IMetaDataModifyListener listener: listeners) {
			listener.postModification(entity, null, result, list, getUser(), language);
		}
		Activator.getInstance().test(MetaDataTest.EVENTCODE_AFTERCREATE, entity, result, list, getUser(), language);
		Activator.getInstance().fireCreateEvent(getEntity(), result, getUser());
		broadcastUserAction("logCreate", result); //$NON-NLS-1$
		removeHiddenAttributes(result);
	}

	@Override
	protected Representation delete(Variant variant) throws ResourceException {
		final Language language = getClientPreferedLanguage();
		final MetaDataEntity entity = getEntity();
		if (entity.isReadOnly()) {
			setStatus(Status.CLIENT_ERROR_FORBIDDEN, Activator.getMessage("error.readonly", language)); //$NON-NLS-1$
			return null;
		}
		final Form form = getRequestForm();
		final boolean hardelete = isParameter(form, "harddelete"); //$NON-NLS-1$
		ISearchCriteria criteria = new AndCriteria(entity.getRightDelete(), getCriteria(form));
		// Faire une primosélection (criteria + right delete)
		BeanMapList items = getEntity().getMapper().selection(entity, (List<ReferenceLine>) null, hardelete, criteria, false, null, getUser(), 0, -1);
		boolean noerrors = true;
		List<IMetaDataDeleteListener> listeners = Activator.getInstance().getDeleteListener(entity.getType());
		for (BeanMap item: items) {
			if (!Activator.getInstance().test(MetaDataTest.EVENTCODE_BEFOREDELETE, entity, item, getUser(), language)) {
				if (noerrors) {
					setStatus(Status.CLIENT_ERROR_FORBIDDEN, Activator.getMessage("right.nodelete", language)); //$NON-NLS-1$
					noerrors = false;
				}
				continue;
			}
			boolean cnt = false;
			boolean byPass = false;
			for (IMetaDataDeleteListener listener: listeners) {
				if (!listener.testDeletion(entity, item, getUser(), language)) {
					cnt = true;
					break;
				}
				if (listener instanceof IByPassListener) {
					byPass = true;
				}
			}
			if (cnt) {
				continue;
			}
			broadcastUserAction("logDelete", item); //$NON-NLS-1$
			if (!byPass) {
				entity.getMapper().delete(item, hardelete);
			}
			item.setDeleted(true);
			Activator.getInstance().test(MetaDataTest.EVENTCODE_AFTERDELETE, entity, item, getUser(), language);
			for(IMetaDataDeleteListener listener: listeners) {
				listener.postDeletion(entity, item, getUser(), language);
			}
			Activator.getInstance().fireDeleteEvent(entity, item, getUser(), hardelete || 
					(entity.getMetadata().get("deleteCol") == null));
		}
		if (noerrors) {
			setStatus(Status.SUCCESS_NO_CONTENT);
		}
		return null;
	}
}
