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
package com.arcadsoftware.metadata;

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.util.tracker.ServiceTracker;
import org.osgi.util.tracker.ServiceTrackerCustomizer;

import com.arcadsoftware.metadata.internal.MapperServiceTrackerCustomizer;

public class MapperServiceTracker extends ServiceTracker<IMapperService, IMapperService> {
		
	public MapperServiceTracker(BundleContext context) {
		super(context, IMapperService.class, null);
		open();
	}

	public MapperServiceTracker(BundleContext context, ServiceTrackerCustomizer<IMapperService, IMapperService> customizer) {
		super(context, IMapperService.class, customizer);
		open();
	}

	public MapperServiceTracker(BundleContext context, String domainName) {
		super(context, IMapperService.class, new MapperServiceTrackerCustomizer(context, domainName));
		open();
	}

	public IMapperService getMapper() {
		return getService();
	}

	public IMapperService getMapper(String domainName) {
		ServiceReference<IMapperService>[] refs = getServiceReferences();
		if (refs == null) {
			return null;
		}
		for(ServiceReference<IMapperService> ref : refs) {
			try {
				if (domainName.equalsIgnoreCase((String)ref.getProperty(IMapperService.PROP_DOMAINNAME))) {
					return getService(ref);
				}
			} catch (RuntimeException e) {}
		}
		return null;
	}

	public ServiceReference<IMapperService> getServiceReference(String domainName) {
		ServiceReference<IMapperService>[] refs = getServiceReferences();
		if (refs == null) {
			return null;
		}
		for(ServiceReference<IMapperService> ref : refs) {
			try {
				if (domainName.equalsIgnoreCase((String) ref.getProperty(IMapperService.PROP_DOMAINNAME))) {
					return ref;
				}
			} catch (RuntimeException e) {}
		}
		return null;
	}
	
}
