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
import com.arcadsoftware.beanmap.xml.JSonBeanMapStream;
import com.arcadsoftware.beanmap.xml.XmlBeanMapStream;
import com.arcadsoftware.rest.JSONRepresentation;
import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.rest.XMLRepresentation;

public abstract class BeanMapParentResource extends UserLinkedResource {

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
		return new JSONRepresentation(new JSonBeanMapStream().toXML(object), language);
	}

	protected Representation getHTMLRepresentation(Object object, boolean simple, Language language) {
		// TODO A implémenter...
		return new XMLRepresentation(new XmlBeanMapStream().toXML(object), language);
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
