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

import org.restlet.Context;
import org.restlet.routing.TemplateRoute;
import org.restlet.routing.Router;
import org.restlet.resource.ServerResource;

/**
 * This basic implementation of a REST URI branch and a simple an unique resource 
 * and optionally a default resource to the branched router.
 *  
 */
public final class SimpleResourceBranch extends AbstractBranch {

	private class SimpleRefs {
		private TemplateRoute route = null;
		private TemplateRoute defaultRoute = null;
	}
	
	private String uriPattern;
	private Class<? extends ServerResource> targetClass;
	private Class<? extends ServerResource> defaultTargetClass;

	public SimpleResourceBranch(String uriPattern,Class<? extends ServerResource> targetClass) {
		super();
		this.uriPattern = uriPattern;
		this.targetClass = targetClass;
		defaultTargetClass = null;
	}

	public SimpleResourceBranch(String uriPattern,Class<? extends ServerResource> targetClass, Class<? extends ServerResource> defaultTargetClass) {
		this(uriPattern,targetClass);
		this.defaultTargetClass = defaultTargetClass;
	}
	
	/* (non-Javadoc)
	 * @see com.arcadsoftware.core.restful.branch#attach(org.restlet.Context, org.restlet.Router)
	 */
	public Object attach(Context context, Router router) {
		SimpleRefs result = new SimpleRefs();
		if (uriPattern != null) {
			result.route = router.attach(uriPattern, targetClass);
			if (defaultTargetClass != null) {
				result.defaultRoute = router.attachDefault(defaultTargetClass);
			}
		} else {
			result.defaultRoute = router.attachDefault(targetClass);
		}
		return result;
	}

	/* 
	 * (non-Javadoc)
	 * @see com.arcadsoftware.core.restful.IBranch#detach(org.restlet.Context, org.restlet.Router, java.lang.Object)
	 */
	public void detach(Context context, Router router, Object reference) {
		if (reference instanceof SimpleRefs) {
			if (((SimpleRefs)reference).route != null) {
				router.detach(((SimpleRefs)reference).route.getNext());
			}
			if (((SimpleRefs)reference).defaultRoute != null) {
				router.detach(((SimpleRefs)reference).defaultRoute.getNext());
			}
		}
	}

}
