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
package com.arcadsoftware.metadata.rest.internal;

import java.util.ArrayList;
import java.util.List;

import org.restlet.data.Form;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.representation.FileRepresentation;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.xml.JsonMetaDataStream;
import com.arcadsoftware.metadata.xml.XmlMetaDataStream;
import com.arcadsoftware.rest.JSONRepresentation;
import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.rest.XMLRepresentation;

public class EntityParentResource extends UserLinkedResource {

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		getAllowedMethods().add(Method.GET);
		getAllowedMethods().add(Method.POST);
		addVariants(MEDIATYPES_BASE_XMLJSONXSD2);
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		if (isXSD(variant)) {
			return new FileRepresentation(Activator.getInstance().getBundleFile("schemas/entities.xsd"), MediaType.APPLICATION_W3C_SCHEMA); //$NON-NLS-1$
		}
		Form form = getRequestForm();
		String domain = form.getFirstValue("domain"); //$NON-NLS-1$
		List<MetaDataEntity> entities;
		if ((domain != null) && (domain.length() > 0)) {
			entities = MetaDataEntity.loadEntities(domain);
		} else {
			entities = MetaDataEntity.loadEntities();
		}
		ArrayList<MetaDataEntity> result = new ArrayList<MetaDataEntity>();
		Language language = getClientPreferedLanguage();
		if ((entities != null) && (entities.size() > 0)) {
			String[] types = form.getValuesArray("type"); //$NON-NLS-1$
			for (MetaDataEntity entity: entities) {
				if (isInTypes(entity.getType(), types)) {
					result.add(entity.clone(language));
				}
			}
		}
		if (isJSON(variant)) {
			return new JSONRepresentation(new JsonMetaDataStream().toXML(result), language);
		}
		return new XMLRepresentation(new XmlMetaDataStream().toXML(result), language);
	}

	private boolean isInTypes(String type, String[] types) {
		if ((types == null) || (types.length == 0)) {
			return true;
		}
		for (String t: types) {
			if (type.matches(t)) {
				return true;
			}
		}
		return false;
	}

	@Override
	protected Representation post(Representation entity, Variant variant) throws ResourceException {
		if (!hasRight(Activator.METADATA_ADMINRIGHT_CRT)) {
			throw new ResourceException(Status.CLIENT_ERROR_FORBIDDEN);
		}
		// TODO Implement entity creation from Client...
		throw new ResourceException(Status.SERVER_ERROR_NOT_IMPLEMENTED);
	}

}
