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
package com.arcadsoftware.metadata.binary;

import org.osgi.framework.BundleContext;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventHandler;
import org.restlet.Context;
import org.restlet.routing.Router;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataEventHandler;
import com.arcadsoftware.osgi.AbstractActivator;
import com.arcadsoftware.osgi.IBinariesTranferService;
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
		// Register Web-service branch.
		registerService(SimpleBranch.clazz, new SimpleBranch() {
			protected RouteList createAttachedResources(Context context, Router router) {
				RouteList routes = new RouteList(router.attach("/bin", BinaryRedirectionResource.class)); //$NON-NLS-1$
				routes.add(router.attach("/bin/{binkey}", BinaryRedirectionResource.class)); //$NON-NLS-1$
				return routes;
			}
		}, SimpleBranch.properties("/data/type")); //$NON-NLS-1$
		// Register deletion listener.
		registerService(EventHandler.class, new MetaDataEventHandler(this) {
			@Override
			public void handleEvent(Event event) {
				final MetaDataEntity entity = getEntity(event);
				final BeanMap item = getBeanMap(event);
				if ((entity != null) && (item != null)) {
				final String category = entity.getMetadata().getString("binary"); //$NON-NLS-1$
					if ((category != null) && !category.isEmpty()) {
						Object o = event.getProperty(MetaDataEventHandler.EVENT_PROP_HARDDELETE);
						if ((o instanceof Boolean) && ((Boolean) o)) {
							IBinariesTranferService bin = Activator.this.getService(IBinariesTranferService.class);
							if (bin != null) {
								for (String c: category.split(" ")) { //$NON-NLS-1$
									if (bin.removeFile(c, item.getId())) {
										debug(String.format("Attached file to %s/%d deleted in category: %s.", entity.getType(), item.getId(), c));
									}
								}
							}
						}
					}
				}
			}
		}, MetaDataEventHandler.getProperties(MetaDataEventHandler.TOPIC_BEANMAP_DELETED));
	}

}
