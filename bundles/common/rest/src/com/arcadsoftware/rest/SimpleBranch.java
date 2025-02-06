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
 * This class is a simple implementation of a Branch that can attach 
 * a list of resources to the parent router and automatically detach this
 * resources when asked for.
 * 
 */
public abstract class SimpleBranch extends AbstractBranch {
	
	/* (non-Javadoc)
	 * @see com.arcadsoftware.restful.IBranch#attach(org.restlet.Context, org.restlet.Router)
	 */
	public final Object attach(Context context, Router router) {
		return createAttachedResources(context,router);
	}

	/**
	 * You must create a new RouteList each time this method is called.
	 * This RouteList will be used when the parent branch will be disposed.
	 * 
	 * You can attach as many resources as you want to the router as the following example show :
	 * 
	 * <blockquote>
	 * <code>rouleList.add(router.attach("/somerouteto/myresource",MyReource.class));</code>
	 * </blockquote>
	 * 
	 * @param context
	 * @param router
	 * @return
	 */
	protected abstract RouteList createAttachedResources(Context context, Router router);

	/* (non-Javadoc)
	 * @see com.arcadsoftware.restful.IBranch#detach(org.restlet.Context, org.restlet.Router, java.lang.Object)
	 */
	public void detach(Context context, Router router, Object reference) {
		if (reference != null) {
			((RouteList)reference).detachAll(router);
		}
	}

}
