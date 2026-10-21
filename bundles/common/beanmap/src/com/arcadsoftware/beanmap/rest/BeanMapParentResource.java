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
package com.arcadsoftware.beanmap.rest;

import org.restlet.data.Form;
import org.restlet.data.Language;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapPartialList;
import com.arcadsoftware.beanmap.internal.xml.BeanMapConverter;
import com.arcadsoftware.beanmap.internal.xml.BeanMapListConverter;
import com.arcadsoftware.beanmap.xml.HTMLSimpleFormater;
import com.arcadsoftware.beanmap.xml.XmlBeanMapStream;
import com.arcadsoftware.rest.JSONRepresentation;
import com.arcadsoftware.rest.JsonStreamCompact;
import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.rest.XMLRepresentation;
import com.thoughtworks.xstream.converters.basic.DateConverter;

public abstract class BeanMapParentResource extends UserLinkedResource {

	public static final JsonStreamCompact JSONMapper = new JsonStreamCompact();

	static {
		// Convert date to ISO format by default... (but accept other format as well).
		JSONMapper.registerConverter(new DateConverter("yyyy-MM-dd'T'HH:mm:ss.S", new String[] { //$NON-NLS-1$ 
				"yyyy-MM-dd HH:mm:ss.S z", //$NON-NLS-1$
				"yyyy-MM-dd HH:mm:ss.S a", //$NON-NLS-1$
				"yyyy-MM-dd HH:mm:ssz", //$NON-NLS-1$ 
				"yyyy-MM-dd HH:mm:ss z", //$NON-NLS-1$
				"yyyy-MM-dd HH:mm:ssa", //$NON-NLS-1$
				"EEE MMM dd HH:mm:ss zzz yyyy" })); //$NON-NLS-1$;
		BeanMapConverter beanMapConverter = new BeanMapConverter(JSONMapper.getMapper(), true, false);
		JSONMapper.registerConverter(new BeanMapListConverter(beanMapConverter, JSONMapper.getMapper(), true, false));
		JSONMapper.registerConverter(beanMapConverter);
		JSONMapper.alias(JsonStreamCompact.TAG_BEAN, BeanMap.class);
		JSONMapper.alias(JsonStreamCompact.TAG_LIST, BeanMapList.class);
		JSONMapper.alias(JsonStreamCompact.TAG_LIST, BeanMapPartialList.class);
	}

	private String type;
	
	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		if (hasRight(Method.GET)) {
			getAllowedMethods().add(Method.GET);
		}
		if (hasRight(Method.POST)) {
			getAllowedMethods().add(Method.POST);
		}
	}

	/**
	 * Override this method to change the BeanMap type.
	 * @return
	 */
	public String getType() {
		if (type == null) {
			type = getAttribute("type"); //$NON-NLS-1$
		}
		return type;
	}
	
	public void setType(String type) {
		this.type = type;
	}
	
	/**
	 * Define if the current user possess the access right corresponding to the given method. 
	 * @param method
	 * @return
	 */
	public abstract boolean hasRight(Method method);

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		if (!getAllowedMethods().contains(getMethod())) {
			throw new ResourceException(Status.CLIENT_ERROR_METHOD_NOT_ALLOWED);
		}
		Form form = getRequestForm();
		return getRepresentation(variant, form, list(getBeanMap(form)), getClientPreferedLanguage());
	}

	@Override
	protected Representation post(Representation entity, Variant variant) throws ResourceException {
		if (!getAllowedMethods().contains(getMethod())) {
			throw new ResourceException(Status.CLIENT_ERROR_METHOD_NOT_ALLOWED);
		}
		Form form = getRequestForm();
		BeanMap result = post(getBeanMap(form));
		if (result == null) {
			setStatus(Status.SUCCESS_NO_CONTENT);
			return null;
		}
		return getRepresentation(variant, form, result, getClientPreferedLanguage());
	}

	protected BeanMap getBeanMap(Form form) {
		return new BeanMap(getType(), form);
	}

	protected boolean isParameter(Form form,String key) {
		String s = form.getFirstValue(key);
		if (s == null) {
			s = getAttribute(key);
		}
		return (s != null);
	}
	
	protected Representation getRepresentation(Variant variant, Form form, Object result, Language language) {
		if (isHTML(variant)) {
			return getHTMLRepresentation(result,isParameter(form, "simple"), language); //$NON-NLS-1$
		}
		if (isJSON(variant)) {
			return getJSONRepresentation(result, language);
		}
		return getXMLRepresentation(result, language);
	}
	
	protected Representation getXMLRepresentation(Object object, Language language) {
		return new XMLRepresentation(new XmlBeanMapStream().toXML(object), language);
	}

	protected Representation getJSONRepresentation(Object object, Language language) {
		final String json;
		if (object instanceof BeanMap b) {
			json = b.toJSon();
		} else if (object instanceof BeanMapList l) {
			json = l.toJSon();
		} else {
			json = JSONMapper.toXML(object);
		}
		return new JSONRepresentation(json, language);
	}

	protected Representation getHTMLRepresentation(Object object, boolean simple, Language language) {
		HTMLSimpleFormater formater = new HTMLSimpleFormater();
		if (object instanceof BeanMapList) {
			formater.append((BeanMapList) object);
		} else if (object instanceof BeanMap) {
			formater.append((BeanMap) object);
		} else if (object != null) {
			formater.append(object.toString());
		}
		return formater.toRepresentation(language);
	}

	/**
	 * Read a BeanMap list.
	 */
	protected abstract BeanMapList list(BeanMap parameters) throws ResourceException;
	
	/**
	 * Create a BeanMap
	 * 
	 * @param bean The beanMap to create with initial attributes values.
	 * @return null if this resource should not return the created BeanMap.
	 * @throws ResourceException
	 */
	protected abstract BeanMap post(BeanMap bean) throws ResourceException;
	

}
