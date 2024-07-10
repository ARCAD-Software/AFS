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
package com.arcadsoftware.osgi;

import java.util.Dictionary;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;

import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.util.tracker.ServiceTracker;

import com.arcadsoftware.osgi.internal.BundleManagedService;
import com.arcadsoftware.osgi.internal.ConfigurationTracker;
import com.arcadsoftware.osgi.internal.Messages;

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
public abstract class AbstractConfiguredActivator extends AbstractActivator {

	/**
	 * OSGi Managed service class name.
	 */
	public static final String MANAGEDSERVICECLASSNAME = "org.osgi.service.cm.ManagedService"; //$NON-NLS-1$;

	/**
	 * Configuration reserver property used to publish properties values through the /config web-services.
	 */
	public static final String PROP_PUBLIC_PROPS = "public.system.parameters"; //$NON-NLS-1$
	
	private ConfigurationTracker configTracker;
	
	@Override
	public void start(BundleContext bundleContext) throws Exception {
		super.start(bundleContext);
		// Create a managed service
		registerService(MANAGEDSERVICECLASSNAME, new BundleManagedService(this), Constants.SERVICE_PID, getConfigurationID());
		// Define a tracker to initialize configuration. 
		configTracker = new ConfigurationTracker(this);
		configTracker.open();
	}

	@Override
	public void stop(BundleContext bundleContext) throws Exception {
		if (configTracker != null) {
			configTracker.close();
			configTracker = null;
		}
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
	
	/**
	 * <p>Return the service tracker used to track the configuration manager service.
	 * 
	 * <p>You should not use this tracker, all the needed operation are implemented through 
	 * the method of the <code>AbstractConfigurationActivator</code>.
	 * 
	 * @return A specialized ServiceTracker.
	 */
	protected ServiceTracker<ConfigurationAdmin, ConfigurationAdmin> getConfigurationTracker() {
		return configTracker;
	}

	/**
	 * <p>This method is called when the bundle start or when the Configuration Administration 
	 * Service start (if it is not started when the bundle does).
	 * 
	 * <p>The implementation Activator should add or update properties according with the
	 * initial requirements. It should not force already existing properties because they 
	 * should have been updated by other bundles or previous executions.
	 * 
	 * <p>In any initialization is required (if default values can make it) then you should not
	 * override this method. This method is particularly useful to test if old version properties
	 * need to be transformed into new version ones.
	 * 
	 * @param properties the stored configuration properties.
	 * @return True if the properties have to be updated in the configuration management.
	 * @see #updatedConfiguration(Dictionary)
	 */
	public boolean initializeConfiguration(Dictionary<String, Object> properties) {
		// Just do nothing.
		return false;
	}

	/**
	 * This method is called each time the bundle configuration properties has been changed.
	 * This should be the result of the <code>initializeConfiguration</code> method or any
	 * other bundle.
	 * 
	 * @param properties the current bundle properties can be <code>null</code>?
	 */
	public void updatedConfiguration(Dictionary<String, Object> properties) {
		// Just do nothing.
	}

	/**
	 * <p>This method allow the bundle to set himself its properties. Theses properties
	 * should not be default values (defaults values do not need to be stocked by the
	 * Configuration Admin service).
	 * 
	 * <p>This method does not allow to remove properties, to do so override the 
	 * <code>initializeConfiguration</code> method.
	 * 
	 * <p>If no Configuration Admin Service is available then the <code>updatedConfiguration</code>
	 * is called directly (within the current thread), otherwise this update will be done
	 * by the OSGi service.   
	 * 
	 * @param properties new properties or properties to be updated.
	 * @return true if the updated is going to be done by the OSGi Configuration Admin service
	 * (If so the update will be done in a different thread). 
	 */
	public final boolean updateConfiguration(Dictionary<String, Object> properties) {
		return (configTracker != null) &&
			configTracker.update(properties);
	}

	/**
	 * <p>Finalize the configuration properties of this bundle. This method is called just
	 * before the Configuration Admin service shutdown or this bundle stop.
	 * 
	 * <p>In both cases you can store some properties at this time, the <code>updatedConfiguration</code>
	 * method will not be called.
	 * 
	 * @param properties new properties or properties to be updated.
	 * @return true if the properties are updated and need to be propagated to the configuration store. 
	 */
	public boolean finalizeConfiguration(Dictionary<String, Object> properties) {
		return false;
	}
	
	/**
	 * <p>Get the current configuration properties. This value can be null if the Configuration Admin
	 * service is not currently available.</p>
	 * 
	 * <p>These values can be used to update the configuration. But you should take care of 
	 * possibly concurrent updates. It is not efficient to update not changed values. 
	 * 
	 * @return the current configuration properties, null if they are not available.
	 */
	public Dictionary<String, Object> getCurrentConfiguration() {
		if (configTracker == null) {
			return null;
		}
		Dictionary<String,Object> result = configTracker.getCurrentConfiguration();
		if (result == null) {
			result = new Hashtable<String,Object>();
			result.put(Constants.SERVICE_PID, getConfigurationID());
		}
		return result;
	}
	
	/**
	 * Try to parse an String configuration parameter. Support any object implementing 
	 * the Object.toString() method. 
	 *  
	 * @param param
	 * @param defaultValue
	 * @return
	 */
	public String parseStringParameter(Object param, String defaultValue) {
		if (param == null) {
			return defaultValue; 
		}
		String v = param.toString();
		if (v.trim().length() == 0) {
			return defaultValue;
		}
		return v;
	}
	
	/**
	 * Try to parse an integer configuration parameter. Support Integer and Strings
	 * representations of this value.
	 *  
	 * @param param
	 * @param defaultValue
	 * @return
	 */
	public int parseIntegerParameter(Object param, int defaultValue) {
		if (param == null) {
			return defaultValue; 
		}
		if (param instanceof Integer) {
			return (Integer)param;
		}
		try {
			return Integer.parseInt(param.toString());
		} catch (NumberFormatException e) {
			debug(Messages.getString("osgi.NotAnInteger") + param); //$NON-NLS-1$
			return defaultValue;
		}
	}
	
	/**
	 * Try to parse an float number (double) configuration parameter. Support Float, Integer and Strings
	 * representations of this value.
	 *  
	 * @param param
	 * @param defaultValue
	 * @return
	 */
	public double parseDoubleParameter(Object param, double defaultValue) {
		if (param == null) {
			return defaultValue; 
		}
		if (param instanceof Double) {
			return (Double) param;
		}
		if (param instanceof Float) {
			return (Double) param;
		}
		if (param instanceof Integer) {
			return (Double) param;
		}
		try {
			return Double.parseDouble(param.toString());
		} catch (NumberFormatException e) {
			debug(Messages.getString("osgi.NotAnInteger") + param); //$NON-NLS-1$
			return defaultValue;
		}
	}

	/**
	 * Try to parse a boolean parameter.
	 * 
	 * @param param The boolean object to parse.
	 * @return the Object boolean value or false if it is null.
	 */
	public boolean parseBooleanParameter(Object param) {
		return parseBooleanParameter(param, false);
	}
	
	/**
	 * Try to parse a boolean parameter.
	 * 
	 * @param param
	 * @param defaultValue The returned value if <code>param</code> is null. 
	 * @return
	 */
	public boolean parseBooleanParameter(Object param, boolean defaultValue) {
		if (param == null) {
			return defaultValue;
		}
		if (param instanceof Boolean) {
			return (Boolean) param;
		}
		if (param instanceof Integer) {
			return (Integer) param != 0;
		}
		if (defaultValue) {
			return !(param.toString().equalsIgnoreCase("false") || param.toString().equalsIgnoreCase("no")); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return param.toString().equalsIgnoreCase("true") || param.toString().equalsIgnoreCase("yes"); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	/**
	 * Return the map of properties corresponding to the given prefix.
	 * 
	 * <p>The prefix is removed from the map keys.
	 * @param properties
	 * @param prefix the String prefix. Test is case sensitive.
	 * @return a Map, this map can be empty but never null.
	 */
	public Map<String, ?> getMapProperties(Dictionary<String, Object> properties,String prefix) {
		HashMap<String, Object> result = new HashMap<String, Object>(properties.size());
		Enumeration<?> keys = properties.keys();
		int pl = prefix.length();
		while (keys.hasMoreElements()) {
			Object k = keys.nextElement();
			if (k != null) {
				String ks = k.toString();
				if (ks.startsWith(prefix)) {
					result.put(ks.substring(pl), properties.get(k));
				}
			}
		}
		return result;
	}
}
