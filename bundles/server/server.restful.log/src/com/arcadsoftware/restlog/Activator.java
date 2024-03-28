/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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
package com.arcadsoftware.restlog;

import java.util.Dictionary;
import java.util.Stack;

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;
import org.osgi.service.log.LogEntry;
import org.osgi.service.log.LogListener;
import org.osgi.service.log.LogReaderService;
import org.osgi.service.log.LogService;
import org.osgi.util.tracker.ServiceTracker;
import org.osgi.util.tracker.ServiceTrackerCustomizer;
import org.restlet.Context;
import org.restlet.routing.Router;

import com.arcadsoftware.osgi.AbstractConfiguredActivator;
import com.arcadsoftware.rest.IBranch;
import com.arcadsoftware.rest.MultiLanguageMessages;
import com.arcadsoftware.rest.RouteList;
import com.arcadsoftware.rest.SimpleBranch;

/**
 * This sample class track the LogReader and expose it to the web
 * as the RESTlet web-service.
 * 
 * This pattern ensure that if the restlet is accessible then le logReader is accessible to.
 * Another model is possible by simply create a Restlet and track the service when the
 * Restlet is called, and if no service is available at this time return an HTTP error code (503). 
 * 
 */
public class Activator extends AbstractConfiguredActivator implements ServiceTrackerCustomizer<LogReaderService, LogReaderService>, LogListener {

	public static final MultiLanguageMessages MESSAGES = new MultiLanguageMessages(
			Activator.class.getPackage().getName() + ".clientmessages", Activator.class.getClassLoader()); //$NON-NLS-1$

	private static final String PROP_STACKLIMIT = "limit";
	private static final String PROP_LOWESTLEVEL = "level";
	
	private ServiceRegistration<IBranch> branchRegistration = null;
	private ServiceTracker<LogReaderService, LogReaderService> tracker;
	private final Stack<LogEntry> lastLog = new Stack<LogEntry>();
	private int limit = 200;
	private int level = LogService.LOG_INFO;

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		// Create a service tracker to track LogReader service.
		tracker = new ServiceTracker<LogReaderService, LogReaderService>(context, LogReaderService.class, this); //$NON-NLS-1$
		// open tracking (since now the restlet can be registered)
		tracker.open();
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		// Stop tracking
		if (tracker != null) {
			Object[] services = tracker.getServices();
			if (services != null) {
				for (Object service: services) {
					if (service instanceof LogReaderService) {
						((LogReaderService) service).removeLogListener(this);
					}
				}
			}
			tracker.close();
			tracker = null;
		}
		super.stop(context);
	}

	@Override
	public void updatedConfiguration(Dictionary<String, Object> properties) {
		if (properties != null) {
			limit = parseIntegerParameter(properties.get(PROP_STACKLIMIT), 100);
			level = parseIntegerParameter(properties.get(PROP_LOWESTLEVEL), LogService.LOG_WARNING);
		}
	}

	@Override
	public LogReaderService addingService(ServiceReference<LogReaderService> reference) {
		// A new LogReaderService has been tracked.
		LogReaderService service = null;
		synchronized (this) {
			// Return the service (any LogReaderService are tracked)
			service = getContext().getService(reference);
			// if the Restlet is not registered.
			// Many LogReaderService should be tracker but only one Restlet is registered. 
			if (branchRegistration == null) {
				branchRegistration = registerService(IBranch.class, new SimpleBranch() {
							@Override
							protected RouteList createAttachedResources(Context context, Router router) {
								return new RouteList(
										router.attach("/admin/log.zip", LogZipResource.class), //$NON-NLS-1$
										router.attach("/admin/log/", new LogReaderRestlet(context, Activator.this)), //$NON-NLS-1$
										router.attach("/admin/log/{level}", new LogReaderLevelRestlet(context, Activator.this)) //$NON-NLS-1$
										);
							}
						}, SimpleBranch.properties(SimpleBranch.SECUREDBRANCH));
				// Only register one listener !
				service.addLogListener(this);
			}
		}
		return service;
	}

	@Override
	public void modifiedService(ServiceReference<LogReaderService> reference, LogReaderService service) {
		// We have nothing to do here since the Restlet, and Branches are stateless.
	}

	@Override
	public void removedService(ServiceReference<LogReaderService> reference, LogReaderService service) {
		// if the LogReaderService is removed we must unregister our Restlet.
		synchronized (this) {
			// ensure that no more service are tracked.
			if ((tracker.getServiceReference() == null) &&
					(branchRegistration != null)) {
				unregister(branchRegistration);
				branchRegistration = null;
			}
		}
	}

	@Override
	public void logged(LogEntry entry) {
		if ((entry.getLevel() <= level) &&
				(lastLog.isEmpty() || (entry != lastLog.lastElement()))) {
			lastLog.push(entry);
			if (lastLog.size() > limit) {
				lastLog.remove(0);
			}
		}
	}

	public LogEntry[] getLog() {
		final LogEntry[] result = new LogEntry[limit + 1];
		lastLog.copyInto(result);
		return result;
	}

	public int getLevel() {
		return level;
	}
}
