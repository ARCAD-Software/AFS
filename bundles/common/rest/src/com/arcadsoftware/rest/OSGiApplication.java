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

import org.osgi.framework.BundleContext;
import org.restlet.Application;
import org.restlet.Restlet;
import org.restlet.data.CharacterSet;
import org.restlet.routing.Router;
import org.restlet.routing.Template;

import com.arcadsoftware.osgi.ILoggedPlugin;
import com.arcadsoftware.rest.internal.InactiveSiteRestlet;
import com.arcadsoftware.rest.internal.SecurityPatchService;

/**
 * Declare a Restlet Application linked to the BundleContext of the bundle that created it.
 * 
 * <p>
 * The bundle that create this application is responsible from starting and stoping it.
 * 
 */
public class OSGiApplication extends Application {

	private volatile Router router;
	private volatile Router root_router;
	private volatile Router inactive_router;
	private final ILoggedPlugin activator;
	private final BundleContext context;
	private boolean active;
	private String inactiveRedirectURI;
	private String version;
	private boolean https;
	private String email;
	private String license;
	private String licenseURL;
	private String termsOfService;
	private String webSite;
	
	/**
	 * This Restlet Application can not be instantiated from IOC or Servlet bridges because it is binded to the bundle
	 * life cycle.
	 * 
	 * @param activator
	 *            interface to the logging facilities.
	 * @param context
	 *            the bundle context that created this application.
	 */
	public OSGiApplication(ILoggedPlugin activator, BundleContext context) {
		super();
		this.context = context;
		this.activator = activator;
		setActive(true);
		// Add extended security headers patch.
		getServices().set(new SecurityPatchService(this));
		// Default Character Set is always UTF-8...
		getMetadataService().setDefaultCharacterSet(CharacterSet.UTF_8);
	}

	@Override
	public Restlet createInboundRoot() {
		if (root_router == null) {
			root_router = new Router(getContext());
			// Both active and inactive routers must be created here (asap).
			router = new Router(getContext());
			router.setDefaultMatchingMode(Template.MODE_STARTS_WITH);
			router.setRoutingMode(Router.MODE_BEST_MATCH);
			inactive_router = new Router(getContext());
			inactive_router.setDefaultMatchingMode(Template.MODE_STARTS_WITH);
			inactive_router.setRoutingMode(Router.MODE_BEST_MATCH);
			inactive_router.attachDefault(new InactiveSiteRestlet(getContext()));
			activeChange();
		}
		return root_router;
	}

	/*
	 * Toggle the application activity.
	 */
	private void activeChange() {
		if (root_router != null) {
			if (active) {
				if (inactive_router != null) {
					root_router.detach(inactive_router);
				}
				if (router == null) {
					router = new Router(getContext());
					router.setDefaultMatchingMode(Template.MODE_STARTS_WITH);
					router.setRoutingMode(Router.MODE_BEST_MATCH);
				}
				root_router.attach(router);
			} else {
				if (router != null) {
					root_router.detach(router);
				}
				if (inactive_router == null) {
					inactive_router = new Router(getContext());
					inactive_router.setDefaultMatchingMode(Template.MODE_STARTS_WITH);
					inactive_router.setRoutingMode(Router.MODE_BEST_MATCH);
					inactive_router.attachDefault(new InactiveSiteRestlet(getContext()));
				}
				root_router.attach(inactive_router);
			}
		}
	}

	/**
	 * This router is the functional router used to attach the service of this application. This router is available
	 * only when the application is active.
	 * 
	 * @return the root router.
	 */
	public Router getRouter() {
		return router;
	}

	/**
	 * This router is used when the application is switch into an inactive mode.
	 * @return
	 */
	public Router getInactiveRouter() {
		return inactive_router;
	}

	/**
	 * @return the bundle context of this application.
	 */
	public BundleContext getBundleContext() {
		return context;
	}

	/**
	 * Define if the REST application is active or not. if inactive no resource are accessible. Any request get a 503 Error state. 
	 * @param active
	 *            the new active state
	 */
	public void setActive(boolean active) {
		this.active = active;
		activeChange();
	}

	/**
	 * Define if the REST application is active or not. if inactive no resource are accessible. Any request get a 503 Error state. 
	 * @return the active
	 */
	public boolean isActive() {
		return active;
	}

	/**
	 * Define the URI used to redirect the user when the application is put into inactive mode (503 state).
	 * 
	 * @param inactiveRedirectURI
	 * @see #isActive()
	 */
	public void setInactiveRedirectURI(String inactiveRedirectURI) {
		this.inactiveRedirectURI = inactiveRedirectURI;
	}

	/**
	 * Get the URI used to redirect the user when the application is put into inactive mode (503 state).
	 * 
	 * @return
	 * @see #isActive()
	 */
	public String getInactiveRedirectURI() {
		return inactiveRedirectURI;
	}

	/**
	 * Get interface to the logger of this framework.
	 * 
	 * @return
	 */
	public ILoggedPlugin getActivator() {
		return activator;
	}
	
	/**
	 * Return the application version.
	 * 
	 * @return
	 */
	public String getVersion() {
		return version;
	}
	
	/**
	 * Define the application version.
	 * 
	 * @param version
	 */
	public void setVersion(String version) {
		this.version = version;
	}

	/**
	 * This property is set to true if the HTTPS server is accessible trough the default HTTPS port number (443).
	 * 
	 * @return
	 */
	public boolean isHttps() {
		return https;
	}

	/**
	 * This property is set to true if the HTTPS server is accessible trough the default HTTPS port number (443).
	 * 
	 * @param https
	 */
	public void setHttps(boolean https) {
		this.https = https;
	}

	/**
	 * Define the contact Email associated to this application.
	 * 
	 * @return
	 */
	public String getEmail() {
		return email;
	}

	/**
	 * Set the contact email associated to this application.
	 * 
	 * @param email
	 */
	public void setEmail(String email) {
		this.email = email;
	}

	/**
	 * Get the license reference associated to this application.
	 * 
	 * @return
	 */
	public String getLicense() {
		return license;
	}

	/**
	 * Set the License title.
	 * 
	 * @param license
	 */
	public void setLicense(String license) {
		this.license = license;
	}

	/**
	 * Define the terms of service of this application.
	 * 
	 * @return
	 */
	public String getTermsOfService() {
		return termsOfService;
	}

	/**
	 * Set the terms of service.
	 * 
	 * @param termsOfService
	 */
	public void setTermsOfService(String termsOfService) {
		this.termsOfService = termsOfService;
	}

	/**
	 * Return the application owner's web-site.
	 * 
	 * @return
	 */
	public String getWebSite() {
		return webSite;
	}

	/**
	 * Set the application owner's web-site.
	 * @param webSite
	 */
	public void setWebSite(String webSite) {
		this.webSite = webSite;
	}

	public String getLicenseURL() {
		return licenseURL;
	}

	public void setLicenseURL(String licenseURL) {
		this.licenseURL = licenseURL;
	}
	
}
