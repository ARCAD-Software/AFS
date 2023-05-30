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
package com.arcadsoftware.connection.internal;

import org.restlet.Context;
import org.restlet.routing.Route;
import org.restlet.routing.Router;
import org.restlet.routing.Template;

import com.arcadsoftware.rest.AbstractBranch;
import com.arcadsoftware.rest.BranchTracker;
import com.arcadsoftware.rest.RouteList;

/*
 * This branch propose a secure connection scheme to any bundle that need one.
 * 
 * Once secured the request allow to access to user rights through an attribute named "connected.user". 
 */
public class SecureBranch extends AbstractBranch {

	private Activator activator;
	private BranchTracker tracker;
	private int count;
	
	public SecureBranch(Activator activator) {
		super();
		this.activator = activator;
		count = 0;
	}

	public Object attach(Context context, Router router) {
		// Initialization of the tracker associated to authenticated resources.
		if (tracker == null) {
			tracker = new BranchTracker(getApplication(), SECUREDBRANCH);
			tracker.setRouter(new Router());
			tracker.getRouter().setDefaultMatchingMode(Template.MODE_STARTS_WITH);
			tracker.getRouter().setRoutingMode(Router.MODE_BEST_MATCH);
			tracker.getRouter().attach("/currentuser", CurrentUserResource.class); //$NON-NLS-1$
			tracker.open();
		}
		CachedAuthentificator guard = new CachedAuthentificator(context, activator, tracker.getRouter());
		guard.setNext(tracker.getRouter());
		Route route = router.attach("/authservices", AuthServicesResource.class); //$NON-NLS-1$
		router.attachDefault(guard);
		count++;
		return new RouteList(guard, route);
	}

	public void detach(Context context, Router router, Object reference) {
		if (reference instanceof RouteList) {
			((RouteList) reference).detachAll(router);
		} else if (reference instanceof CachedAuthentificator) {
			router.detach((CachedAuthentificator) reference);
		}
		count--;
		// reset of the tracker.
		if (count == 0) {
			tracker.close();
			tracker = null;
		}
	}

}
