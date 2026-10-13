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
package com.arcadsoftware.osgi;

import java.util.Dictionary;

import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;

import com.arcadsoftware.osgi.internal.BundleManagedFactoryService;

/**
 * This class implement an auto configuration process for bundles.
 * 
 * <p>
 * Note that the initial configuration loading may be done during the <code>super.start(BundleContext)</code> if the
 * Configuration Admin service is already initialized. Then it is not recomended to perform initialization in the start method,
 * after the super call !
 * 
 * @see AbstractActivator
 */
public abstract class AbstractConfiguredFactoryActivator extends AbstractActivator {

	/**
	 * OSGi Managed service class name.
	 */
	public static final String MANAGEDSERVICECLASSNAME = "org.osgi.service.cm.ManagedServiceFactory"; //$NON-NLS-1$;

	/**
	 * Configuration reserver property used to publish properties values through the /config web-services.
	 */
	public static final String PROP_PUBLIC_PROPS = "public.system.parameters"; //$NON-NLS-1$
	
	@Override
	public void start(BundleContext bundleContext) throws Exception {
		super.start(bundleContext);
		// Create a managed service
		registerService(MANAGEDSERVICECLASSNAME, new BundleManagedFactoryService(this), Constants.SERVICE_PID, getConfigurationID());
	}

	@Override
	public void stop(BundleContext bundleContext) throws Exception {
		super.stop(bundleContext);
	}
	
	/**
	 * Get the Configured service ID. This is the bundle ID by default.
	 * This ID is used to create a configured Service that store the bundle configuration parameters.
	 * 
	 * <p>
	 * It should not change as long as the Bundle is started.
	 * 
	 * @return a non null String.
	 */
	protected String getConfigurationID() {
		return getContext().getBundle().getSymbolicName();
	}
	
	public String getName() {
		return getConfigurationID() + " Factory";
	}
	
	/**
	 * This method is called each time the bundle configuration properties has been changed.
	 * This should be the result of the <code>initializeConfiguration</code> method or any
	 * other bundle.
	 * @param pid 
	 * 
	 * @param properties the current bundle properties can be <code>null</code>?
	 */
	public void updatedConfiguration(String pid, Dictionary<String, Object> properties) {
		// Just do nothing.
	}
	
	public void deleted(String pid) {
		// Just do nothing.
	}
}
