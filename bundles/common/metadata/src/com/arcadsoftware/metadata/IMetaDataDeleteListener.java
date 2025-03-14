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

import org.restlet.data.Language;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Listener (synchronized) of items modifications. 
 * 
 * Creation Date: 11 avr. 2011
 */
public interface IMetaDataDeleteListener {

	/**
	 * The OSGi Service ID.
	 */
	public static final String clazz = IMetaDataDeleteListener.class.getName();
	
	/**
	 * This property define the listened entity type.
	 */
	public static final String PROP_TYPE = "type"; //$NON-NLS-1$
	
	/**
	 * Called before deletion.
	 * 
	 * <p>
	 * Use this method to test, change or complete the modified attributes of the entity.
	 * 
	 * <p>Default implementation must return true.
	 * 
	 * @param entity The corresponding entity.
	 * @param originalItem the currently values of the object into stockage.
	 * @param user the current user (may be null).
	 * @param language The current user language (or default language).
	 * @return false if the process should be stopped.
	 * @throws ResourceException throw this exception to return a specific error message to the client.
	 */
	public boolean testDeletion(MetaDataEntity entity, BeanMap originalItem, IConnectionUserBean user, Language language) throws ResourceException;

	/**
	 * Called just after the item deletion.
	 * 
	 * @param entity The corresponding entity.
	 * @param originalItem the old values of the object into stockage (The Deleted flag of this BeanMap is true).
	 * @param user the current user (may be null).
	 * @param language The current user language (or default language).
	 * @throws ResourceException throw this exception to return a specific error message to the client.
	 */
	public void postDeletion(MetaDataEntity entity, BeanMap originalItem, IConnectionUserBean user, Language language) throws ResourceException;
}
