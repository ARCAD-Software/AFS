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

public class EntityItemResource extends UserLinkedResource {

	private static final String KEY_TYPE = "type"; //$NON-NLS-1$
	
	private MetaDataEntity entity;
	
	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		String type = getAttribute(KEY_TYPE);
		if (type == null) {
			setExisting(false);
		} else {
			if ("list".equals(type)) {
				type = type + getReference().getRemainingPart();
			}
			entity = MetaDataEntity.loadEntity(type);
			if (entity == null) {
				setExisting(false);
			} else {
				getAllowedMethods().add(Method.GET);
				getAllowedMethods().add(Method.PUT);
				getAllowedMethods().add(Method.DELETE);
				setLastModification(entity.getDate());
				addVariants(MEDIATYPES_BASE_XMLJSONXSD2);
			}
		}
	}

	@Override
	protected Representation delete(Variant variant) throws ResourceException {
		if ((!hasRight(Activator.METADATA_ADMINRIGHT_EDT)) || (!hasRight(Activator.METADATA_ADMINRIGHT_CRT))) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL);
		}
		entity.getRegistry().removeEntity(entity);
		setStatus(Status.SUCCESS_NO_CONTENT);
		return null;
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		if (isXSD(variant)) {
			return new FileRepresentation(Activator.getInstance().getBundleFile("schemas/entities.xsd"), MediaType.APPLICATION_W3C_SCHEMA); //$NON-NLS-1$
		}
		Language language = getClientPreferedLanguage();
		// Translate the entity...
		if (isJSON(variant)) {
			return new JSONRepresentation(new JsonMetaDataStream().toXML(entity.clone(language)), language, entity.getDate());
		}
		return new XMLRepresentation(new XmlMetaDataStream().toXML(entity.clone(language)), language, entity.getDate());
	}

	@Override
	protected Representation put(Representation representation, Variant variant) throws ResourceException {
		if (!hasRight(Activator.METADATA_ADMINRIGHT_EDT)) {
			throw new ResourceException(Status.CLIENT_ERROR_FORBIDDEN);
		}
		// TODO Implement Entity modification from Client...
		throw new ResourceException(Status.SERVER_ERROR_NOT_IMPLEMENTED);
	}
}
