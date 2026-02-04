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
 * Listener (synchronized) of items (soft) undeletion. 
 * 
 * Creation Date: 2024/10/21
 */
public interface IMetaDataUndeleteListener {
	
	/**
	 * This property define the listened entity type.
	 */
	public static final String PROP_TYPE = IMetaDataSelectionListener.PROP_TYPE;
	
	/**
	 * Called before deletion.
	 * 
	 * <p>
	 * Use this method to test, change or complete the undeletion of the given item.
	 * 
	 * <p>Default implementation return true.
	 * 
	 * @param entity The corresponding entity.
	 * @param item the currently values of the object into database (The Deleted flag of this BeanMap is true).
	 * @param user the current user (may be null).
	 * @param language The current user language (or default language).
	 * @return false if the process should be stopped.
	 * @throws ResourceException throw this exception to return a specific error message to the client.
	 */
	public default boolean testUndeletion(MetaDataEntity entity, BeanMap item, IConnectionUserBean user, Language language) throws ResourceException {
		return true;
	}

	/**
	 * Called just after the item undeletion.
	 * 
	 * @param entity The corresponding entity.
	 * @param item the old values of the object into database (The Deleted flag of this BeanMap is false).
	 * @param user the current user (may be null).
	 * @param language The current user language (or default language).
	 * @throws ResourceException throw this exception to return a specific error message to the client.
	 */
	public void postUndeletion(MetaDataEntity entity, BeanMap item, IConnectionUserBean user, Language language) throws ResourceException;
}
