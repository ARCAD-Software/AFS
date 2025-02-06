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
package com.arcadsoftware.metadata.rest.internal;

import org.restlet.Context;
import org.restlet.routing.Router;
import org.restlet.routing.Template;

import com.arcadsoftware.rest.BranchTracker;
import com.arcadsoftware.rest.RouteList;
import com.arcadsoftware.rest.SimpleBranch;

public class Branch extends SimpleBranch {

	private int count = 0;
	private BranchTracker mdtracker;
	private BranchTracker dltracker;
	private BranchTracker ditracker;
	
	@Override
	protected RouteList createAttachedResources(Context context, Router router) {
		synchronized (this) {
			if (count == 0) {
				mdtracker = new BranchTracker(getApplication(), "/metadata"); //$NON-NLS-1$
				mdtracker.setRouter(new Router());
				mdtracker.getRouter().setDefaultMatchingMode(Template.MODE_STARTS_WITH);
				mdtracker.getRouter().setRoutingMode(Router.MODE_BEST_MATCH);
				mdtracker.getRouter().attachDefault(EntityItemResource.class);
				mdtracker.open();
				dltracker = new BranchTracker(getApplication(), "/data/list"); //$NON-NLS-1$
				dltracker.setRouter(new Router());
				dltracker.getRouter().setDefaultMatchingMode(Template.MODE_STARTS_WITH);
				dltracker.getRouter().setRoutingMode(Router.MODE_BEST_MATCH);
				dltracker.getRouter().attachDefault(MetaDataSimpleListItemResource.class);
				dltracker.open();
				ditracker = new BranchTracker(getApplication(), "/data/type"); //$NON-NLS-1$
				ditracker.setRouter(new Router());
				ditracker.getRouter().setDefaultMatchingMode(Template.MODE_STARTS_WITH);
				ditracker.getRouter().setRoutingMode(Router.MODE_BEST_MATCH);
				ditracker.getRouter().attachDefault(MetaDataItemResource.class);
				ditracker.open();
			}
			count++;
		}
		RouteList rl = new RouteList();
		rl.add(router.attach("/metadata", EntityParentResource.class)); //$NON-NLS-1$
		rl.add(router.attach("/metadata/{type}", mdtracker.getRouter())); //$NON-NLS-1$
		rl.add(router.attach("/data/list", MetaDataSimpleListsResource.class)); //$NON-NLS-1$
		rl.add(router.attach("/data/list/{type}", MetaDataSimpleListParentResource.class)); //$NON-NLS-1$
		rl.add(router.attach("/data/list/{type}/{id}", dltracker.getRouter())); //$NON-NLS-1$
		rl.add(router.attach("/data/{type}", MetaDataParentResource.class)); //$NON-NLS-1$
		rl.add(router.attach("/data/{type}/{id}", ditracker.getRouter())); //$NON-NLS-1$
		return rl;
	}

	@Override
	public void detach(Context context, Router router, Object reference) {
		super.detach(context, router, reference);
		synchronized (this) {
			count--;
			if (count == 0) {
				mdtracker.close();
				mdtracker = null;
				ditracker.close();
				ditracker = null;
				dltracker.close();
				dltracker = null;
			}
		}
	}
	
}
