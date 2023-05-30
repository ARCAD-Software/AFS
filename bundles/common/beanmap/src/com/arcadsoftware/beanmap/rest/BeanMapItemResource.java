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
import org.restlet.representation.EmptyRepresentation;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.xml.JSonBeanMapStream;
import com.arcadsoftware.beanmap.xml.XmlBeanMapStream;
import com.arcadsoftware.rest.JSONRepresentation;
import com.arcadsoftware.rest.UserLinkedIdentifiedResource;
import com.arcadsoftware.rest.XMLRepresentation;

/**
 * 
 * 
 * 
 * Creation Date: 2 août 2011
 */
public abstract class BeanMapItemResource extends UserLinkedIdentifiedResource {

	private String type;
	
	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		// TODO Support multi-ID !
		if (hasRight(Method.GET)) {
			getAllowedMethods().add(Method.GET);
		}
		if (hasRight(Method.PUT)) {
			getAllowedMethods().add(Method.PUT);
		}
		if (hasRight(Method.DELETE)) {
			getAllowedMethods().add(Method.DELETE);
		}
	}

	/**
	 * Override this method to change the BeanMap type.
	 * @return
	 */
	public String getType() {
		if (type == null) {
			type = getAttribute("type"); //$NON-NLS-1$
			if (type == null) {
				return "item"; //$NON-NLS-1$
			}
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
	protected Representation delete(Variant variant) throws ResourceException {
		delete(new BeanMap(getType(),getId()));
		setStatus(Status.SUCCESS_NO_CONTENT);
		return null;
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		Form form = getRequestForm();
		return getRepresentation(variant, form, get(getBeanMap(form)), getClientPreferedLanguage());
	}

	@Override
	protected Representation post(Representation entity, Variant variant) throws ResourceException {
		return put(entity, variant);
	}

	@Override
	protected Representation put(Representation representation, Variant variant) throws ResourceException {
		Form form = getRequestForm();
		BeanMap bean = getBeanMap(form);
		if (!(representation instanceof EmptyRepresentation)) {
			put(bean);
		}
		//setStatus(Status.SUCCESS_NO_CONTENT);
		return getRepresentation(variant, form, bean, getClientPreferedLanguage());
	}

	protected BeanMap getBeanMap(Form form) {
		return new BeanMap(getType(),getId(), form);
	}

	protected boolean isParameter(Form form,String key) {
		String s = form.getFirstValue(key);
		if (s == null) {
			s = getAttribute(key);
		}
		return (s != null);
	}
	
	protected Representation getRepresentation(Variant variant, Form form, Object result, Language language) {
		if (result == null) {
			setStatus(Status.SUCCESS_NO_CONTENT);
			return null;
		}
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
	 * Read a BeanMap.
	 * 
	 * @param bean the BeanMap to read.
	 * @return A new BeanMap or the given one completed with full attributes.
	 * @throws ResourceException
	 */
	protected abstract BeanMap get(BeanMap bean) throws ResourceException;
	
	/**
	 * Update a BeanMap
	 * 
	 * @param bean The beanMap to update with modified attributes.
	 * @throws ResourceException
	 */
	protected abstract void put(BeanMap bean) throws ResourceException;

	/**
	 * Delete a BeanMap
	 * 
	 * @param bean
	 * @throws ResourceException
	 */
	protected abstract void delete(BeanMap bean) throws ResourceException;
}
