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
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;

import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.ServiceReference;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.cm.ManagedServiceFactory;
import org.osgi.util.tracker.ServiceTracker;

import com.arcadsoftware.osgi.internal.BundleManagedFactoryService;
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
		// TODO Auto-generated method stub
	}
	
//	/**
//	 * Try to parse an String configuration parameter. Support any object implementing 
//	 * the Object.toString() method. 
//	 *  
//	 * @param param
//	 * @param defaultValue
//	 * @return
//	 */
//	public String parseStringParameter(Object param, String defaultValue) {
//		if (param == null) {
//			return defaultValue; 
//		}
//		String v = param.toString();
//		if (v.trim().length() == 0) {
//			return defaultValue;
//		}
//		return v;
//	}
//	
//	/**
//	 * Try to parse an integer configuration parameter. Support Integer and Strings
//	 * representations of this value.
//	 *  
//	 * @param param
//	 * @param defaultValue
//	 * @return
//	 */
//	public int parseIntegerParameter(Object param, int defaultValue) {
//		if (param == null) {
//			return defaultValue; 
//		}
//		if (param instanceof Integer) {
//			return (Integer)param;
//		}
//		try {
//			return Integer.parseInt(param.toString());
//		} catch (NumberFormatException e) {
//			debug(Messages.getString("osgi.NotAnInteger") + param); //$NON-NLS-1$
//			return defaultValue;
//		}
//	}
//	
//	/**
//	 * Try to parse an long configuration parameter. Support Long and Strings
//	 * representations of this value.
//	 *  
//	 * @param param
//	 * @param defaultValue
//	 * @return
//	 */
//	public long parseLongParameter(Object param, long defaultValue) {
//		if (param == null) {
//			return defaultValue; 
//		}
//		if (param instanceof Long) {
//			return (Long) param;
//		}
//		try {
//			return Long.parseLong(param.toString());
//		} catch (NumberFormatException e) {
//			debug(Messages.getString("osgi.NotAnInteger") + param); //$NON-NLS-1$
//			return defaultValue;
//		}
//	}
//	
//	/**
//	 * Try to parse an float number (double) configuration parameter. Support Float, Integer and Strings
//	 * representations of this value.
//	 *  
//	 * @param param
//	 * @param defaultValue
//	 * @return
//	 */
//	public double parseDoubleParameter(Object param, double defaultValue) {
//		if (param == null) {
//			return defaultValue; 
//		}
//		if (param instanceof Double) {
//			return (Double) param;
//		}
//		if (param instanceof Float) {
//			return (Double) param;
//		}
//		if (param instanceof Integer) {
//			return (Double) param;
//		}
//		try {
//			return Double.parseDouble(param.toString());
//		} catch (NumberFormatException e) {
//			debug(Messages.getString("osgi.NotADouble") + param); //$NON-NLS-1$
//			return defaultValue;
//		}
//	}
//
//	/**
//	 * Try to parse a boolean parameter.
//	 * 
//	 * @param param The boolean object to parse.
//	 * @return the Object boolean value or false if it is null.
//	 */
//	public boolean parseBooleanParameter(Object param) {
//		return parseBooleanParameter(param, false);
//	}
//	
//	/**
//	 * Try to parse a boolean parameter.
//	 * 
//	 * @param param
//	 * @param defaultValue The returned value if <code>param</code> is null. 
//	 * @return
//	 */
//	public boolean parseBooleanParameter(Object param, boolean defaultValue) {
//		if (param == null) {
//			return defaultValue;
//		}
//		if (param instanceof Boolean) {
//			return (Boolean) param;
//		}
//		if (param instanceof Integer) {
//			return (Integer) param != 0;
//		}
//		if (defaultValue) {
//			return !(param.toString().equalsIgnoreCase("false") || param.toString().equalsIgnoreCase("no")); //$NON-NLS-1$ //$NON-NLS-2$
//		}
//		return param.toString().equalsIgnoreCase("true") || param.toString().equalsIgnoreCase("yes"); //$NON-NLS-1$ //$NON-NLS-2$
//	}
//	
//	/**
//	 * Return the map of properties corresponding to the given prefix.
//	 * 
//	 * <p>The prefix is removed from the map keys.
//	 * @param properties
//	 * @param prefix the String prefix. Test is case sensitive.
//	 * @return a Map, this map can be empty but never null.
//	 */
//	public Map<String, ?> getMapProperties(Dictionary<String, Object> properties,String prefix) {
//		HashMap<String, Object> result = new HashMap<String, Object>(properties.size());
//		Enumeration<?> keys = properties.keys();
//		int pl = prefix.length();
//		while (keys.hasMoreElements()) {
//			Object k = keys.nextElement();
//			if (k != null) {
//				String ks = k.toString();
//				if (ks.startsWith(prefix)) {
//					result.put(ks.substring(pl), properties.get(k));
//				}
//			}
//		}
//		return result;
//	}

}
