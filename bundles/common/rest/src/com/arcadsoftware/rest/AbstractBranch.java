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
package com.arcadsoftware.rest;

import java.util.Dictionary;
import java.util.Hashtable;

/**
 * Abstract implementation of a REST Branch.
 * 
 * This is an helper class you can use the statics properties method and clazz constant
 * to register the service when the extends this class.
 * 
 * This Abstract class also implement the access to the Restlet Application.
 * 
 * Note: If needed this Restlet Application can give you access to the
 *  bundle context of the restful bundle.
 */
public abstract class AbstractBranch implements IBranch {

	/**
	 * Helper method to create needed properties for this service.
	 * 
	 * @param uri
	 * @return
	 */
	public static final Dictionary<String, ?> properties(String uri) {
		Hashtable<String, Object> properties = new Hashtable<String, Object>();
		properties.put(URI,uri);
		return properties;
	}

	/**
	 * Helper property to register Branches services.
	 */
	public static final String clazz = IBranch.class.getName();
	
	private OSGiApplication application;

	/* (non-Javadoc)
	 * @see com.arcadsoftware.core.restful.branch#getApplication()
	 */
	public OSGiApplication getApplication() {
		return application;
	}

	/* (non-Javadoc)
	 * @see com.arcadsoftware.core.restful.branch#setApplication(org.restlet.Application)
	 */
	public void setApplication(OSGiApplication application) {
		this.application = application;
	}

}
