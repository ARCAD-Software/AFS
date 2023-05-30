/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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
package com.arcadsoftware.metadata.rest;

import java.util.Date;
import java.util.Hashtable;
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
import com.arcadsoftware.beanmap.xml.HTMLSimpleFormater;
import com.arcadsoftware.beanmap.xml.JSonBeanMapStream;
import com.arcadsoftware.beanmap.xml.XmlBeanMapStream;
import com.arcadsoftware.metadata.IMapperService;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataFormater;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.rest.internal.Activator;
import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.rest.JSONRepresentation;
import com.arcadsoftware.rest.XMLRepresentation;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Implement a generic resource that must be attached as reference as the "/data/type" branch. These resource append general services to
 * entities (structural objects or groups of items). 
 * 
 * <p>
 * The "type" part of the path must be a single part (without "/") or you will need to
 * override the <code>getType()</code> method.
 * 
 * <p>
 * Extender just need to check user rights according to the operation type (CRUD) that this resource may performs. 
 */
public class DataParentResource extends UserLinkedResource {

	public static final String PARAMETER_TYPE = "type"; // //$NON-NLS-1$

	private MetaDataEntity entity;
	
	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		entity = MetaDataEntity.loadEntity(getType());
		if ((entity == null) || (entity.getMapper() == null)) {
			setExisting(false);
		} else {
			getAllowedMethods().add(Method.GET);
			getAllowedMethods().add(Method.POST);
			getAllowedMethods().add(Method.DELETE);
			addVariants(MEDIATYPES_BASESUP);
		}
	}

	/**
	 * @return the type value of related Entity.
	 */
	protected String getType() {
		return getAttribute(PARAMETER_TYPE);
	}
	
	/**
	 * @return the MetaDataEntity related to this resource call.
	 */
	protected MetaDataEntity getEntity() {
		return entity;
	}

	/**
	 * Get the Mapper associated with the Entity used by this resource.
	 * 
	 * @return
	 */
	protected IMapperService getMapper() {
		return entity.getMapper();
	}

	/**
	 * Return true if the given "key" is an attribute of the HTTP request. 
	 * 
	 * <p> Such an attribute is passed through the given Form or into the Resource 
	 * path attributes (i.e. {part} of the resource path).  
	 * @param form
	 * @param key
	 * @return
	 */
	protected boolean isParameter(Form form, String key) {
		if (form == null) {
			return false;
		}
		String s = form.getFirstValue(key);
		if (s == null) {
			return getAttribute(key) != null;
		}
		return true;
	}

	/**
	 * Get a representation of the given result Object. This object should be a BeanMap or a BeanMapList
	 * but other java Object should work. The nature of the representation depends on the Variant type.
	 * 
	 * <p>
	 * Supported representation are:
	 * <ul>
	 * <li> HTML: only for BeanMap and BeanMapList.
	 * <li> JSON
	 * <li> XML
	 * </ul>
	 * <p> Default representation is XML.
	 * @param variant The client desired representation.
	 * @param form the Request Form. Used for presentation parameters.
	 * @param result The object to represent.
	 * @param language The client user language.
	 * @return A REST Representation.
	 */
	protected Representation getRepresentation(Variant variant, Form form, Object result, Language language) {
		if (isHTML(variant)) {
			if (result instanceof BeanMapList) {
				return getHTMLRepresentation((BeanMapList) result, isParameter(form, "simple"), language); //$NON-NLS-1$
			}
			if (result instanceof BeanMap) {
				return getHTMLRepresentation((BeanMap) result,  isParameter(form, "simple"), language); //$NON-NLS-1$
			}
		}
		if (isJSON(variant)) {
			return getJSONRepresentation(result, language);
		}
		return getXMLRepresentation(result, language);
	}
	
	/**
	 * Get an XML Representation of the given Object, using an XStream Parser.
	 * 
	 * @param object
	 * @param language
	 * @return
	 */
	protected Representation getXMLRepresentation(Object object, Language language) {
		return new XMLRepresentation(new XmlBeanMapStream().toXML(object), language);
	}

	/**
	 * Get an JSON Representation of the given Object, using an XStream Parser.
	 * 
	 * @param object
	 * @param language
	 * @return
	 */
	protected Representation getJSONRepresentation(Object object, Language language) {
		return new JSONRepresentation(new JSonBeanMapStream().toXML(object), language);
	}
	
	/**
	 * Get an HTML representation of a BeanMapList (a table... with link to data resources.)
	 * @param list
	 * @param simple
	 * @param language
	 * @return
	 */
	protected Representation getHTMLRepresentation(BeanMapList list, boolean simple, Language language) {
		final StringBuilder ret = new StringBuilder();
		List<String> segments = getRequest().getOriginalRef().getSegments();
		if (segments != null) {
			for (int i = segments.size() - 2; i >= 0; i--) {
				if ("data".equalsIgnoreCase(segments.get(i))) { //$NON-NLS-1$
					break;
				}
				ret.append("../"); //$NON-NLS-1$
			}
		}
		HTMLSimpleFormater f = new HTMLSimpleFormater(String.format("%s List", getEntity().getType())) {
			@Override
			protected String getHRef(BeanMap bean, String key, String value) {
				ReferenceLine att = getEntity().getAttributeLine(key);
				if ((att == null) || att.isEmpty() || att.isFinal()) {
					return null;
				}
				return ret.toString() + att.getLastAttribute().getType() + '/' + value;
			}
			
			@Override
			protected String getHRef(BeanMap bean) {
				return ret.toString() + bean.getType() + '/' + bean.getId();
			}
		};
		f.append(list);
		return new StringRepresentation(f.toString(), MediaType.APPLICATION_XHTML, language, CharacterSet.UTF_8);
	}

	/**
	 * Get an HTML representation of a BeanMap.
	 * 
	 * @param bean
	 * @param simple
	 * @param language
	 * @return
	 */
	protected Representation getHTMLRepresentation(BeanMap bean, boolean simple, Language language) {
		final StringBuilder ret = new StringBuilder();
		List<String> segments = getRequest().getOriginalRef().getSegments();
		if (segments != null) {
			for (int i = segments.size() - 2; i >= 0; i--) {
				if ("data".equalsIgnoreCase(segments.get(i))) { //$NON-NLS-1$
					break;
				}
				ret.append("../"); //$NON-NLS-1$
			}
		}
		HTMLSimpleFormater f = new HTMLSimpleFormater(String.format("%s Data", getEntity().getType())) {
			@Override
			protected String getHRef(BeanMap bean, String key, String value) {
				ReferenceLine att = getEntity().getAttributeLine(key);
				if (att.isEmpty() || att.isFinal()) {
					return null;
				}
				return ret.toString() + att.getLastAttribute().getType() + '/' + value;
			}
		};
		f.append(bean);
		return new StringRepresentation(f.toString(), MediaType.APPLICATION_XHTML, language, CharacterSet.UTF_8);
	}
	
	/**
	 * Broadcast, asynchronously a message to the central User Action Log (if it is active !).
	 *  
	 * @param tag The metadata tag used to store the format message string.
	 * @param item
	 */
	protected void broadcastUserAction(String tag, BeanMap item) {
		MetaDataEntity entity;
		if (item.getType().equals(this.entity.getType())) {
			entity = this.entity;
		} else {
			entity = MetaDataEntity.loadEntity(item.getType());
		}
		Object broadcast = entity.getMetadata().get(tag);
		if (broadcast != null) {
			if (broadcast instanceof String) {
				broadcast = new MetaDataFormater((String) broadcast, entity);
				entity.getMetadata().put(tag, broadcast);
			}
			if (broadcast instanceof MetaDataFormater) {
				Hashtable<String, Object> props = new Hashtable<String, Object>();
				IConnectionUserBean user = getUser();
				props.put("uid", user.getId()); //$NON-NLS-1$
				if (user.getLogin() != null) {
					props.put("login", user.getLogin()); //$NON-NLS-1$
				}
				props.put("code", entity.getType()); //$NON-NLS-1$
				if (((MetaDataFormater) broadcast).getReferenceLines().isEmpty()) {
					props.put("message", ((MetaDataFormater) broadcast).getFormatString()); //$NON-NLS-1$
				} else if (((MetaDataFormater) broadcast).isComplete(item, false)) {
					props.put("message", ((MetaDataFormater) broadcast).format(item)); //$NON-NLS-1$
				} else {
					// We nedd to reload the item to complete the message text...
					BeanMap ritem = entity.getMapper().selection(entity, item.getId(), ((MetaDataFormater) broadcast).getReferenceLines(), true);
					if (ritem != null) {
						item = ritem;
					}
					props.put("message", ((MetaDataFormater) broadcast).format(item)); //$NON-NLS-1$
				}
				props.put("date", new Date()); //$NON-NLS-1$
				Activator.getInstance().fireBroadCastEvent(props);
			}
		}
	}
	
}
