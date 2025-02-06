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

import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * This OSGi service is a synchronized listener called each time an entity is selected through its own resource.
 * 
 * <p>
 * Note that this service is <b>not</b> called when an entity is used through a reference line.  
 * 
 * @author ARCAD Software
 */
public interface IMetaDataSelectionListener {

	/**
	 * The OSGi Service ID.
	 */
	public static final String clazz = IMetaDataSelectionListener.class.getName();
	
	/**
	 * This property define the listened entity type.
	 */
	public static final String PROP_TYPE = "type"; //$NON-NLS-1$
	

	/**
	 * Called just after the selection.
	 * 
	 * @param entity The corresponding entity.
	 * @param selectedItems The actual list of selected items, this list is not empty but may content only one element.
	 * @param user The current user (may be null).
	 * @param language The current user language (or default language).
	 * @throws ResourceException Throw this exception to return a specific error message to the client.
	 */
	public void onSelection(MetaDataEntity entity, BeanMapList selectedItems, IConnectionUserBean user, Language language) throws ResourceException;
}
