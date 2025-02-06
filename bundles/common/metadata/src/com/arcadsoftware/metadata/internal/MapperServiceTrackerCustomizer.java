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
package com.arcadsoftware.metadata.internal;

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.util.tracker.ServiceTrackerCustomizer;

import com.arcadsoftware.metadata.IMapperService;

public class MapperServiceTrackerCustomizer implements ServiceTrackerCustomizer<IMapperService, IMapperService> {

	private final BundleContext context;
	private final String domainName;
	
	public MapperServiceTrackerCustomizer(BundleContext context, String domainName) {
		super();
		this.context = context;
		this.domainName = domainName;
	}

	public IMapperService addingService(ServiceReference<IMapperService> reference) {
		try {
			if (domainName.equalsIgnoreCase((String) reference.getProperty(IMapperService.PROP_DOMAINNAME))) {
				return context.getService(reference);
			}
		} catch (RuntimeException e) {}
		return null;
	}
	
	public void modifiedService(ServiceReference<IMapperService> reference, IMapperService service) {}
	
	public void removedService(ServiceReference<IMapperService> reference, IMapperService service) {}
}
