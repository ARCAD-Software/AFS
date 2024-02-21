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
package com.arcadsoftware.metadata.server.user.internal;

import java.util.ArrayList;
import java.util.Collection;

import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;
import org.restlet.data.Form;
import org.restlet.data.Method;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.xml.JSonBeanMapStream;
import com.arcadsoftware.beanmap.xml.XmlBeanMapStream;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.criteria.AndCriteria;
import com.arcadsoftware.metadata.criteria.EqualCriteria;
import com.arcadsoftware.rest.JSONRepresentation;
import com.arcadsoftware.rest.UserLinkedIdentifiedResource;
import com.arcadsoftware.rest.XMLRepresentation;
import com.arcadsoftware.rest.connection.IAuthentificationService;

public class UserAuthsRecource extends UserLinkedIdentifiedResource {

	private MetaDataEntity user;
	private ArrayList<MetaDataEntity> auths;
	
	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		getAllowedMethods().add(Method.GET);
		user = MetaDataEntity.loadEntity(Activator.TYPE_USER);
		auths = new ArrayList<MetaDataEntity>();
		try {
			Collection<ServiceReference<IAuthentificationService>> srs = getBundleContext().getServiceReferences(IAuthentificationService.class, null);
			for (ServiceReference<IAuthentificationService> sr: srs) {
				Object o = sr.getProperty(IAuthentificationService.ENTITYNAME);
				if (o != null) {
					String v = o.toString();
					if (v.length() > 0) {
						MetaDataEntity e = MetaDataEntity.loadEntity(v);
						if ((e != null) && !auths.contains(e)) {
							auths.add(e);
						}
					}
				}
			}
		} catch (InvalidSyntaxException e) {}
		setExisting((user != null) && !auths.isEmpty());
	}
	
	protected boolean isParameterTrue(Form form, String key) {
		if (form == null) {
			return false;
		}
		String s = form.getFirstValue(key);
		if (s == null) {
			s = getAttribute(key);
		}
		return (s != null) && "true".equalsIgnoreCase(s); //$NON-NLS-1$
	}
	@Override
	protected Representation get(Variant variant) throws ResourceException {
		final EqualCriteria uid = new EqualCriteria(Activator.TYPE_USER, getId());
		final BeanMapList result = new BeanMapList();
		final Form form = getRequest().getResourceRef().getQueryAsForm();
		final boolean deleted = isParameterTrue(form, "deleted"); //$NON-NLS-1$
		for (MetaDataEntity e: auths) {
			result.addAll(e.dataSelection(e.getAllAttributes(), deleted, new AndCriteria(uid, e.getRightList()),false, null, getUser(), 0, -1));
		}
		if (isJSON(variant)) {
			return new JSONRepresentation(new JSonBeanMapStream().toXML(result), getClientPreferedLanguage());
		}
		return new XMLRepresentation(new XmlBeanMapStream().toXML(result), getClientPreferedLanguage());
	}

}
