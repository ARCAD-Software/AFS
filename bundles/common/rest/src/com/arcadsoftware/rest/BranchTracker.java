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
package com.arcadsoftware.rest;

import java.util.Collection;
import java.util.HashMap;

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.service.log.Logger;
import org.osgi.service.log.LoggerFactory;
import org.osgi.util.tracker.ServiceTracker;
import org.restlet.routing.Router;
import org.restlet.routing.Template;

/**
 * This service tracker define a REST branch extension point. For a given URI
 * it create a Router and track any branches that want to attach some resources
 * to this URI.
 * 
 * You can use this tracker to declare sub-branches extension point into your
 * branches. To do so you must create and open the tracker during the <code>attach</code>
 * of your Branch, and close and clear (set to null) the tracker into the <code>detach</code>
 * method.
 * 
 * TODO support multi-routed branches. 
 */
public class BranchTracker extends ServiceTracker<IBranch, IBranch> {

	private static final String ROOTURI = IBranch.ROOTBRANCH;
	
	private final HashMap<IBranch, Object> references = new HashMap<IBranch, Object>();
	private final OSGiApplication application;
	private final String uri;
	private Router router;
	private boolean detachRouter;
	private boolean opened;

	/**
	 * Create the Branch tracker. This service tracker will create a Router
	 * if theURI is not the root ("/") and attach it to this URI.
	 * 
	 * You can not create multiples tracker for same URI's.
	 * The tracker for root URI is already created by the plugin 
	 * <code>com.arcadsoftware.resful</code>.  
	 * 
	 * @param application attached Application.
	 * @param branchURI the URI where branch will be able to attach.
	 */
	public BranchTracker(OSGiApplication application, String branchURI) {
		super(application.getBundleContext(), IBranch.class.getName(), null);
		// check if the uri is the Root one, and replace it with its valid name.
		if ((branchURI == null) ||
				(branchURI.length() == 0) ||
				branchURI.equals("\\")) { //$NON-NLS-1$
			uri = ROOTURI;
		} else {
			uri = branchURI;
		}
		this.application = application;
		// Root URI automatically use the Application Root router.
		if (ROOTURI.equals(uri)) {
			router = application.getRouter();
			detachRouter = false;
		}
	}

	@Override
	public synchronized void close() {
		super.close();
		opened = false;
		// Detach the router.
		if (detachRouter && (router != null)) {
			application.getRouter().detach(router);
			router = null;
		}
	}

	@Override
	public synchronized void open(boolean trackAllServices) {
		// We absolutely need a rooter now.
		opened = true;
		if (router == null) {
			if (ROOTURI.equals(uri)) {
				router = application.getRouter();
				detachRouter = false;
			} else {
				router = new Router(application.getContext());
				router.setDefaultMatchingMode(Template.MODE_STARTS_WITH);
				router.setRoutingMode(Router.MODE_BEST_MATCH);
				application.getRouter().attach(uri, router);
				detachRouter = true;
			}
		}
		super.open(trackAllServices);
	}

	/**
	 * Get the URI used bay the tracked branches. 
	 * @return
	 */
	public String getUri() {
		return uri;
	}
	
	@Override
	public IBranch addingService(ServiceReference<IBranch> reference) {
		String uriPattern = (String) reference.getProperty(IBranch.URI);
		if ((uriPattern == null) || (!uriPattern.equals(uri))) {
			return null;
		}
		IBranch provider = super.addingService(reference);
		try {
			synchronized (this) {
				provider.setApplication(application);
				Object ref = provider.attach(application.getContext(), router);
				if (ref != null) {
					references.put(provider, ref);
				}
			}
		} catch (Exception e) {
			logError(e);
		}
		return provider;
	}

	private void logError(Exception e) {
		if ((application != null) && (application.getActivator() != null)) {
			application.getActivator().error("Unable to attach a Branch: " + e.getLocalizedMessage(), e);
		} else {
			try {
				BundleContext context = application.getBundleContext();
				if (context != null) {
					Collection<ServiceReference<LoggerFactory>> srs = context.getServiceReferences(LoggerFactory.class, null);
					if ((srs != null) && (srs.size() >= 1)) {
						for (ServiceReference<LoggerFactory> sr: srs) {
							LoggerFactory lf = context.getService(sr);
							if (lf != null) {
								Logger l = lf.getLogger(BranchTracker.class);
								l.error("Unable to attach a Branch: " + e.getLocalizedMessage(), e);
							}
						}
					}
				}
			} catch (Exception z) {}
		}
	}

	@Override
	public void removedService(ServiceReference<IBranch> reference, IBranch service) {
		super.removedService(reference, service);
		synchronized (this) {
			service.detach(application.getContext(), router, references.get(service));
			service.setApplication(null); // Remove references...
			references.remove(service);
		}
	}

	/**
	 * Return the Application linked to this Tracker.
	 * 
	 * @return
	 */
	public OSGiApplication getApplication() {
		return application;
	}

	/**
	 * Return the Router managed by this Branch Tracker.
	 * @return
	 */
	public synchronized Router getRouter() {
		return router;
	}

	/**
	 * Set a new router to this branch tracker. 
	 * If the tracker is opened then it is closed when affecting the new router.
	 * You must reopen it as needed.
	 * 
	 * This Router should be attached to the application by the provider.
	 * 
	 * @param router
	 */
	public synchronized void setRouter(Router router) {
		if (opened) {
			close();
		}
		if ((this.router != null) && detachRouter) {
			application.getRouter().detach(this.router);
		}
		detachRouter = (router != null);
		this.router = router;
	}
}
