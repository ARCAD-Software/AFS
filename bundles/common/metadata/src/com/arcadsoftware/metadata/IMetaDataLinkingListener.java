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
package com.arcadsoftware.metadata;

import org.restlet.data.Language;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public interface IMetaDataLinkingListener {

	/**
	 * The OSGi Service ID.
	 */
	public static final String clazz = IMetaDataLinkingListener.class.getName();
	
	/**
	 * This property define the listened entity type.
	 */
	public static final String PROP_TYPE = "type"; //$NON-NLS-1$
	
	/**
	 * This property define the listened entity link.
	 */
	public static final String PROP_LINK = "link"; //$NON-NLS-1$
	
	/**
	 * Called before two element are linked with each other.
	 * 
	 * <p>
	 * Use this method to test, change the link operation.
	 * 
	 * <p>Default implementation must return true.
	 * 
	 * @param link The corresponding entity link.
	 * @param sourceItem the source item to be linked.
	 * @param destItem the destination item to be linked.
	 * @param user the current user (may be null).
	 * @param language The current user language (or default language).
	 * @return false if the process should be stopped.
	 * @throws ResourceException throw this exception to return a specific error message to the client.
	 */
	public boolean testLink(MetaDataLink link, BeanMap sourceItem, BeanMap destItem, IConnectionUserBean user, Language language) throws ResourceException;

	/**
	 * Called before two element are unlinked with each other.
	 * 
	 * <p>
	 * Use this method to test, change the unlink operation.
	 * 
	 * <p>Default implementation must return true.
	 * 
	 * @param link The corresponding entity link.
	 * @param sourceItem the source item to be linked.
	 * @param destItem the destination item to be linked.
	 * @param attributes the modified attributes list.
	 * @param user the current user (may be null).
	 * @param language The current user language (or default language).
	 * @throws ResourceException throw this exception to return a specific error message to the client.
	 */
	public boolean testUnlink(MetaDataLink link, BeanMap sourceItem, BeanMap destItem, IConnectionUserBean user, Language language) throws ResourceException;

}
