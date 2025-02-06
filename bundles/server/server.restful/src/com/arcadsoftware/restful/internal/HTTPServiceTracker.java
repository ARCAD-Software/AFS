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

import java.util.Dictionary;
import java.util.Properties;

import org.osgi.framework.ServiceReference;
import org.osgi.service.http.HttpService;
import org.osgi.util.tracker.ServiceTracker;

/**
 * Enregistre le connecteur servlet de Restlet à tous les services HTTP fournient par le framework OSGi. 
 */
public class HTTPServiceTracker extends ServiceTracker<HttpService, HttpService> {

	private static final String ALIAS = "/"; //$NON-NLS-1$
	
	private Activator activator;
	private boolean mustAddServlet = false;
	
	public HTTPServiceTracker(Activator activator) {
		super(activator.getContext(), HttpService.class, null);
		this.activator = activator;
		open();
	}

	@Override
	public HttpService addingService(ServiceReference<HttpService> reference) {
		HttpService o = super.addingService(reference);
		if ((o != null) && mustAddServlet) {
			try {
				((HttpService)o).registerServlet(ALIAS, new OSGiServerServlet(activator), getProperties(), null);
			} catch (Exception e) {
				activator.error(Messages.getString("HTTPServiceTracker.Error"), e); //$NON-NLS-1$
			}
		}
		return o;
	}

	@SuppressWarnings("rawtypes")
	private Dictionary getProperties() {
		Properties props = new Properties();
		props.put("org.restlet.autoWire", "false"); //$NON-NLS-1$ //$NON-NLS-2$
		props.put("org.restlet.attribute.application", "com.arcadsoftware.restful.application"); //$NON-NLS-1$ //$NON-NLS-2$
		return props;
	}

	@Override
	public void removedService(ServiceReference<HttpService> reference, HttpService service) {
		if ((service != null) && mustAddServlet) {
			((HttpService)service).unregister(ALIAS);
		}
		super.removedService(reference, service);
	}
	
	public void addServlet() {
		try {
			mustAddServlet = true;
			Object[] services = getServices();
			if (services != null) {
				for(Object o :services) {
					if (o instanceof HttpService) {
						try {
							((HttpService) o).registerServlet(ALIAS, new OSGiServerServlet(activator), getProperties(), null);
						} catch (Exception e) {
							activator.error(Messages.getString("HTTPServiceTracker.Error"), e); //$NON-NLS-1$
						}
					}
				}
			}
		} catch (Throwable e) {
			activator.error(e.getLocalizedMessage(), e);
		}
	}
	
	public void removeservlet() {
		try {
			Object[] services = getServices();
			if (services != null) {
				for(Object o :services) {
					if ((o instanceof HttpService) && mustAddServlet) {
						((HttpService)o).unregister(ALIAS);
					}
				}
			}
		} finally {
			mustAddServlet = false;
		}
	}
}
