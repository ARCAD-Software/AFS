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
package com.arcadsoftware.metadata.binary;

import org.osgi.framework.BundleContext;
import org.restlet.Context;
import org.restlet.routing.Router;

import com.arcadsoftware.osgi.AbstractActivator;
import com.arcadsoftware.rest.RouteList;
import com.arcadsoftware.rest.SimpleBranch;

public class Activator extends AbstractActivator {

	private static BundleContext context;
	
	public static BundleContext getBundleContext() {
		return context;
	}
	
	@Override
	public void start(BundleContext bundleContext) throws Exception {
		context = bundleContext;
		super.start(bundleContext);
		registerService(SimpleBranch.clazz, new SimpleBranch() {
			
			protected RouteList createAttachedResources(Context context, Router router) {
				RouteList routes = new RouteList(router.attach("/bin", BinaryRedirectionResource.class)); //$NON-NLS-1$
				routes.add(router.attach("/bin/{binkey}", BinaryRedirectionResource.class)); //$NON-NLS-1$
				return routes;
			}
			
		}, SimpleBranch.properties("/data/type")); //$NON-NLS-1$
	}

}
