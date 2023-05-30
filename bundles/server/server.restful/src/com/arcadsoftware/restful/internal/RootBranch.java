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
package com.arcadsoftware.restful.internal;

import org.restlet.Context;
import org.restlet.routing.TemplateRoute;
import org.restlet.routing.Router;

import com.arcadsoftware.rest.AbstractBranch;

/**
 * Base "branch" root of all branches of REST resources.
 */
public class RootBranch extends AbstractBranch {

	private class RootRefs {
		private AboutRestlet aboutRestlet;
		private TemplateRoute faviconRoute;
		private TemplateRoute aboutCSSRoute;
		private TemplateRoute serviceStatusCSSRoute;
	}
	
	private final Activator activator;
	
	public RootBranch(Activator activator) {
		super();
		this.activator = activator;
	}
	
	@Override
	public Object attach(Context context, Router router) {
		// Build the About Restlet
		RootRefs reference = new RootRefs();
		reference.aboutRestlet = new AboutRestlet(context, activator);
		router.attach("/about", reference.aboutRestlet); //$NON-NLS-1$
		reference.faviconRoute = router.attach("/favicon.ico", FaviconResource.class); //$NON-NLS-1$
		reference.serviceStatusCSSRoute = router.attach("/css/service/status.css", ServiceStatusCSSResource.class); //$NON-NLS-1$
		return reference;
	}

	@Override
	public void detach(Context context, Router router, Object reference) {
		if (reference instanceof RootRefs) {
			if (((RootRefs) reference).aboutRestlet != null) {
				router.detach(((RootRefs)reference).aboutRestlet);
			}
			if (((RootRefs) reference).faviconRoute != null) {
				router.getRoutes().remove(((RootRefs)reference).faviconRoute);
			}
			if (((RootRefs) reference).aboutCSSRoute != null) {
				router.getRoutes().remove(((RootRefs)reference).aboutCSSRoute);
			}
			if (((RootRefs) reference).serviceStatusCSSRoute != null) {
				router.getRoutes().remove(((RootRefs)reference).serviceStatusCSSRoute);
			}
		}
	}

}