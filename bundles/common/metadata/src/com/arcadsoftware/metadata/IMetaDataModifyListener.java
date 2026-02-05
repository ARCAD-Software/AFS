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
package com.arcadsoftware.metadata;

import java.util.List;

import org.restlet.data.Language;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Listener (synchronized) of items modifications. 
 * 
 * Creation Date: 11 avr. 2011
 */
public interface IMetaDataModifyListener {

	/**
	 * The OSGi Service ID.
	 */
	public static final String clazz = IMetaDataModifyListener.class.getName();
	
	/**
	 * This property define the listened entity type.
	 */
	public static final String PROP_TYPE = IMetaDataSelectionListener.PROP_TYPE;
	
	/**
	 * Called before modification or creation (originalItem is null).
	 * 
	 * <p>
	 * Use this method to test, change or complete the modified attributes of the entity. Any modification to modifiedItem will be recorded.
	 * 
	 * <p>
	 * Default implementation return true. Returning false will cancel the operation.
	 * 
	 * @param entity The corresponding entity.
	 * @param originalItem the currently values of the object into the storage. This parameter may be null if the "modification" is a creation.
	 * @param modifiedItem the modified attributes values.
	 * @param attributes the modified attributes list.
	 * @param user the current user (may be null).
	 * @param language The current user language (or default language).
	 * @return false if the process should be stopped.
	 * @throws ResourceException throw this exception to return a specific error message to the client.
	 */
	public default boolean testModification(MetaDataEntity entity, BeanMap originalItem, BeanMap modifiedItem, List<MetaDataAttribute> attributes, IConnectionUserBean user, Language language) throws ResourceException {
		return true;
	}

	/**
	 * Called just after the item modification or creation (originalItem is null).
	 * 
	 * @param entity The corresponding entity.
	 * @param originalItem the old values of the object into the storage. This parameter may be null if the "modification" is a creation.
	 * @param modifiedItem the recently modified attributes values.
	 * @param attributes the modified attributes list.
	 * @param user the current user (may be null).
	 * @param language The current user language (or default language).
	 * @throws ResourceException throw this exception to return a specific error message to the client.
	 */
	public void postModification(MetaDataEntity entity, BeanMap originalItem, BeanMap modifiedItem, List<MetaDataAttribute> attributes, IConnectionUserBean user, Language language) throws ResourceException;
}
