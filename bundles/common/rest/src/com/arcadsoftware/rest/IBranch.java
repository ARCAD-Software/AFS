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
package com.arcadsoftware.rest;

import org.restlet.Context;
import org.restlet.routing.Router;

/**
 * Define a new REST "branch" defined by its URI 
 * (the "uri" must be a property of the OSGi service).
 * 
 * Theses branches are tracked and "attached to some router that feet the requested URI.
 * 
 * Any kind of resources can be attached through theses branches.
 * 
 * TODO Make the branches able to be attached to multiple router.
 */
public interface IBranch {

	/**
	 * Root Branch name, this is the name of the Branch 
	 * created by the Restlet Application as the root of the
	 * resource tree.
	 * 
	 * <p>
	 * Note: The branch name is not necessarily the same as the URL path.
	 */
	public static final String ROOTBRANCH = "/"; //$NON-NLS-1$

	/**
	 * The name of the branch witch require user identification.
	 *   
	 * <p>
	 * Note: The branch name is not necessarily the same as the URL path.
	 */
	public static final String SECUREDBRANCH = "/secure"; //$NON-NLS-1$
	
	/**
	 * The URI property that must be defined by any branch when it is registered.
	 */
	public static final String URI = "uri"; //$NON-NLS-1$
	
	/**
	 * Set the current application.
	 *  
	 * This application is defined before the <code>attach</code> and <code>detach</code> methods are called.
	 * 
	 * This application is an OSGi Application witch has a bundle context that can be used
	 * by descendants
	 * 
	 * @param application
	 */
	public void setApplication(OSGiApplication application);
	
	/**
	 * Return the current application where this branch is going to be attached.
	 * This application should not be used to attache resources.
	 * 
	 * This application is an OSGi Application witch has a bundle context that can be used
	 * by descendants

	 * @return The current application
	 */
	public OSGiApplication getApplication();
	
	/**
	 * Process to any declaration of Restlet, Resources or Guards.
	 * Until the detach method is called the Router and the Context will be maintained.
	 * 
	 * @param context
	 * @param router
	 * @return an handle which will be passed back to this branch when it will be detached. 
	 */
	public Object attach(Context context, Router router);
	
	/**
	 * Remove any declaration previously attached. 
	 * 
	 * @param context
	 * @param router
	 * @param reference
	 */
	public void detach(Context context, Router router, Object reference);
	
}
