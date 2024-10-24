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
package com.arcadsoftware.metadata.rest;

import java.util.Date;
import java.util.Hashtable;
import java.util.List;

import org.restlet.data.CharacterSet;
import org.restlet.data.Form;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.engine.util.DateUtils;
import org.restlet.representation.EmptyRepresentation;
import org.restlet.representation.Representation;
import org.restlet.representation.StringRepresentation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.IDatedBean;
import com.arcadsoftware.beanmap.xml.HTMLSimpleFormater;
import com.arcadsoftware.beanmap.xml.JSonBeanMapStream;
import com.arcadsoftware.beanmap.xml.XmlBeanMapStream;
import com.arcadsoftware.metadata.IMapperService;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataFormater;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.criteria.ConstantCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.criteria.natural.CriteriaParser;
import com.arcadsoftware.metadata.rest.internal.Activator;
import com.arcadsoftware.metadata.xml.JsonCriteriaStream;
import com.arcadsoftware.metadata.xml.XmlCriteriaStream;
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


	public static ISearchCriteria getCriteria(UserLinkedResource resource, Form form) {
		String[] values = form.getValuesArray("criteria"); //$NON-NLS-1$
		if ((values == null) || (values.length == 0)) {
			String c = resource.getAttribute("criteria"); //$NON-NLS-1$
			if (c != null) {
				values = new String[] {c};
			}
		}
		if ((values == null) || (values.length == 0)) {
			return ConstantCriteria.TRUE;
		}
		for (int i = values.length - 1; i >= 0; i--) {
			values[i] = values[i].trim();
		}
		// Support XML format:
		if (values[0].charAt(0) == '<') {
			for (String v: values) {
				if (v.equalsIgnoreCase("<all/>")) { //$NON-NLS-1$
					return ConstantCriteria.TRUE;
				}
			}
			String result;
			if (values.length > 1) {
				StringBuilder sb = new StringBuilder("<and>"); //$NON-NLS-1$
				for (String v: values) {
					sb.append(v);
				}
				result = sb.append("</and>").toString(); //$NON-NLS-1$
			} else {
				result = values[0];
			}
			try {
				return (ISearchCriteria) new XmlCriteriaStream().fromXML(result);
			} catch (Exception e) {
				Activator.getInstance().warn("Invalid XML Selection Criteria received: " + result);
				throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, "The given XML selection criteria is invalid: " + result);
			}
		}
		// Support JSON format:
		if (values[0].charAt(0) == '{') {
			for (String v: values) {
				if (v.startsWith("{\"all\"")) { //$NON-NLS-1$
					return ConstantCriteria.TRUE;
				}
			}
			// We must handle multiple criteria differently in JSON !
			String result;
			if (values.length > 1) {
				StringBuilder sb = new StringBuilder("{\"or\":["); //$NON-NLS-1$
				boolean first = true;
				for (String v: values) {
					if (first) {
						first = false;
					} else {
						sb.append(',');
					}
					sb.append(v);
				}
				result = sb.append("]}").toString(); //$NON-NLS-1$
			} else {
				result = values[0];
			}
			ISearchCriteria r = new JsonCriteriaStream().read(result);
			if (r != null) {
				return r;
			}
			Activator.getInstance().warn("Invalid JSON Selection Criteria received: " + result);
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, "The given JSON selection criteria is invalid: " + result);
		}
		for (String v: values) {
			if (v.equalsIgnoreCase("all")) { //$NON-NLS-1$
				return ConstantCriteria.TRUE;
			}
		}
		String result;
		if (values.length > 1) {
			StringBuilder sb = new StringBuilder();
			boolean first = true;
			for (String v: values) {
				if (first) {
					first = false;
				} else {
					sb.append(" & "); //$NON-NLS-1$
				}
				sb.append(v);
			}
			result = sb.toString(); //$NON-NLS-1$
		} else {
			result = values[0];
		}
		// Support "human mathematical expired format"
		try {
			return CriteriaParser.parse(result);
		} catch (ResourceException e) {
			Activator.getInstance().warn("Invalid Selection Criteria received: " + result);
			throw e;
		}
	}
	
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
		Date lm = null;
		if (object instanceof IDatedBean) {
			lm = ((IDatedBean) object).getDate();
			if ((lm == null) && (object instanceof BeanMapList)) {
				for (BeanMap b: (BeanMapList) object) {
					Date d = b.getDate();
					if ((d != null) && ((lm == null) || d.after(lm))) {
						lm = d;
					}
				}
			}
		}
		XMLRepresentation rep = new XMLRepresentation(new XmlBeanMapStream().toXML(object), language);
		if (lm != null) {
			rep.setModificationDate(lm);
		}
		return rep;
	}

	/**
	 * Get an JSON Representation of the given Object, using an XStream Parser.
	 * 
	 * @param object
	 * @param language
	 * @return
	 */
	protected Representation getJSONRepresentation(Object object, Language language) {
		Date lm = null;
		if (object instanceof IDatedBean) {
			lm = ((IDatedBean) object).getDate();
			if ((lm == null) && (object instanceof BeanMapList)) {
				for (BeanMap b: (BeanMapList) object) {
					Date d = b.getDate();
					if ((d != null) && ((lm == null) || d.after(lm))) {
						lm = d;
					}
				}
			}
		}
		JSONRepresentation rep = new JSONRepresentation(new JSonBeanMapStream().toXML(object), language);
		if (lm != null) {
			rep.setModificationDate(lm);
		}
		return rep;
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
		Date lm = null;
		for (BeanMap b: list) {
			Date d = b.getDate();
			if ((d != null) && ((lm == null) || d.after(lm))) {
				lm = d;
			}
		}
		StringRepresentation rep = new StringRepresentation(f.toString(), MediaType.APPLICATION_XHTML, language, CharacterSet.UTF_8);
		if (lm != null) {
			rep.setModificationDate(lm);
		}
		return rep;
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
		StringRepresentation rep = new StringRepresentation(f.toString(), MediaType.APPLICATION_XHTML, language, CharacterSet.UTF_8);
		if (bean.getDate() != null) {
			rep.setModificationDate(bean.getDate());
		}
		return rep;
	}

	/**
	 * This method allow to manage the HTTP conditional headers after the GET process.
	 * 
	 * <p>
	 * Call this method just before to return the Representation of the result, if it return a representation then 
	 * this representation must be returned. If it return "null" then the actual representation of the result must be sent. 
	 * (the status of the response has been set to 304.)
	 * 
	 * @param result a list of results.
	 * @return  null if the Representation associated to this result must be sent.
	 */
	protected Representation postProcessConditionalHeaders(BeanMapList result) {
		if ((result == null) || result.isEmpty()) {
			return null;
		}
		Date date = getRequest().getConditions().getModifiedSince();
		if (date != null) {
			Date lm = null;
			for (BeanMap bean: result) {
				Date d = bean.getDate();
				if ((d != null) && ((lm == null) || DateUtils.before(d, lm))) {
					lm = d;
				}
			}
			if (lm == null) {
				return null;
			}
			setLastModification(lm);
			if (!DateUtils.before(lm, date)) {
				getResponse().setStatus(Status.REDIRECTION_NOT_MODIFIED);
				// Add the current last-modification date in  the response, for information...
				EmptyRepresentation er = new EmptyRepresentation();
				er.setModificationDate(lm);
				return er;
			}
		} else {
			date = getRequest().getConditions().getUnmodifiedSince();
			if (date != null) {
				Date lm = null;
				for (BeanMap bean: result) {
					Date d = bean.getDate();
					if ((d != null) && ((lm == null) || DateUtils.before(lm, d))) {
						lm = d;
					}
				}
				if (lm == null) {
					return null;
				}
				if (DateUtils.before(lm, date)) {
					getResponse().setStatus(Status.CLIENT_ERROR_PRECONDITION_FAILED);
					//getResponse().getHeaders().set(Header, PARAMETER_TYPE)
					// Add the current last-modification date in  the response, for information...
					EmptyRepresentation er = new EmptyRepresentation();
					er.setModificationDate(lm);
					return er;
				}
			}
		}
		return null;
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
