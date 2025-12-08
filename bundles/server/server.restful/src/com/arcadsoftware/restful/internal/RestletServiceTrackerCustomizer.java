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
package com.arcadsoftware.restful.internal;

import org.osgi.framework.ServiceReference;
import org.osgi.util.tracker.ServiceTrackerCustomizer;
import org.restlet.Restlet;

import com.arcadsoftware.rest.IBranch;
import com.arcadsoftware.rest.OSGiApplication;

public class RestletServiceTrackerCustomizer implements ServiceTrackerCustomizer<Restlet, Restlet> {

	private OSGiApplication application;
	private Activator activator;
	
	public RestletServiceTrackerCustomizer(OSGiApplication application, Activator activator) {
		super();
		this.application = application;
		this.activator = activator;
	}

	public Restlet addingService(ServiceReference<Restlet> reference) {
		Object path = reference.getProperty(IBranch.URI);
		if (path instanceof String) {
			Restlet service = activator.getContext().getService(reference);
			if (service != null) {
				application.getRouter().attach((String) path, service);
				if (service.getContext() == null) {
					service.setContext(application.getContext());
				}
				return (Restlet) service;
			}
		}
		return null;
	}

	public void modifiedService(ServiceReference<Restlet> reference, Restlet service) {
		removedService(reference, service);
		addingService(reference);
	}

	public void removedService(ServiceReference<Restlet> reference, Restlet service) {
		if (service instanceof Restlet) {
			application.getRouter().detach((Restlet) service);
		}
	}

}
