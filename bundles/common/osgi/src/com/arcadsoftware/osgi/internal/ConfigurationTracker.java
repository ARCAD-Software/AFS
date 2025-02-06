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
package com.arcadsoftware.osgi.internal;

import java.io.IOException;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.Hashtable;

import org.osgi.framework.ServiceReference;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.util.tracker.ServiceTracker;

import com.arcadsoftware.osgi.AbstractConfiguredActivator;

/**
 * Tracker used to perform basic configuration update operations.
 * 
 * @see AbstractConfiguredActivator
 */
public class ConfigurationTracker extends ServiceTracker<ConfigurationAdmin, ConfigurationAdmin> {

	private static final String OSGI_ERROR_UPDATING_PROPERTIES = "osgi.ErrorUpdatingProperties";
	private static final String OSGI_ERROR_RETRIEVING_PROPERTIES = "osgi.ErrorRetrivingProperties";
	private AbstractConfiguredActivator activator;
	
	/**
	 * @param abstractConfiguredActivator
	 */
	public ConfigurationTracker(AbstractConfiguredActivator activator) {
		super(activator.getContext(), ConfigurationAdmin.class.getName(), null);
		this.activator = activator;
	}

	@Override
	public ConfigurationAdmin addingService(ServiceReference<ConfigurationAdmin> reference) {
		ConfigurationAdmin configAdmin = super.addingService(reference);
		if (configAdmin == null) {
			return null;
		}
		Configuration c = null;
		try {
			c = configAdmin.getConfiguration(context.getBundle().getSymbolicName(), null);
		} catch (IOException e) {
			activator.error(Messages.getString(OSGI_ERROR_RETRIEVING_PROPERTIES), e); //$NON-NLS-1$
			return configAdmin;
		}
		if (c != null) {
			Dictionary<String, Object> properties = c.getProperties();
			if (properties == null) {
				properties = new Hashtable<String, Object>();
			}
			boolean changed = false;
			synchronized (activator) {
				changed = activator.initializeConfiguration(properties);
			}
			if (changed) {
				try {
					c.update(properties);
				} catch (IOException e) {
					activator.error(Messages.getString(OSGI_ERROR_UPDATING_PROPERTIES), e); //$NON-NLS-1$
				}
			}
		}
		return configAdmin;
	}

	@Override
	public void removedService(ServiceReference<ConfigurationAdmin> reference, ConfigurationAdmin service) {
		if (service != null) {
			Configuration c = null;
			try {
				c = service.getConfiguration(context.getBundle().getSymbolicName(), null);
			} catch (IOException e) {
				activator.error(Messages.getString(OSGI_ERROR_RETRIEVING_PROPERTIES), e); //$NON-NLS-1$
			}
			if (c != null) {
				Dictionary<String, Object> properties = c.getProperties();
				if (properties == null) {
					properties = new Hashtable<String, Object>();
				}
				boolean changed = false;
				synchronized (activator) {
					changed = activator.finalizeConfiguration(properties);
				}
				if (changed) {
					super.removedService(reference, service);
					try {
						c.update(properties);
					} catch (IOException e) {
						activator.error(Messages.getString(OSGI_ERROR_UPDATING_PROPERTIES), e); //$NON-NLS-1$
					}
					return;
				}
			}
		}
		super.removedService(reference, service);
	}

	/**
	 * Update the properties of this bunble.
	 * 
	 * <p>If there is no configuration Admin service available, this method return
	 * false and call the updateConfiguration method of the Activator.
	 * 
	 * @param properties
	 * @return
	 */
	public boolean update(Dictionary<String,Object> properties) {
		if (properties.isEmpty()) {
			return false;
		}
		ConfigurationAdmin configAdmin = getService();
		if (configAdmin == null) {
			activator.updatedConfiguration(properties);
			return false;
		}
		Configuration c = null;
		try {
			c = configAdmin.getConfiguration(context.getBundle().getSymbolicName(), null);
		} catch (IOException e) {
			activator.error(Messages.getString(OSGI_ERROR_RETRIEVING_PROPERTIES), e); //$NON-NLS-1$
			return false;
		}
		if (c != null) {
			Dictionary<String, Object> oldProperties = c.getProperties();
			if (oldProperties == null) {
				oldProperties = new Hashtable<String, Object>();
			}
			Enumeration<?> keys = properties.keys();
			while (keys.hasMoreElements()) {
				Object key = keys.nextElement();
				oldProperties.put(key.toString(), properties.get(key));
			}
			try {
				c.update(oldProperties);
			} catch (IOException e) {
				activator.error(Messages.getString(OSGI_ERROR_UPDATING_PROPERTIES), e); //$NON-NLS-1$
			}
		}
		return true;
	}
	
	/**
	 * @return The currently available configuration properties, can be null.
	 */
	public Dictionary<String,Object> getCurrentConfiguration() {
		ConfigurationAdmin configAdmin = getService();
		if (configAdmin == null) {
			return null;
		}		
		Configuration c = null;
		try {
			c = configAdmin.getConfiguration(context.getBundle().getSymbolicName(), null);
		} catch (IOException e) {
			activator.error(Messages.getString(OSGI_ERROR_RETRIEVING_PROPERTIES), e); //$NON-NLS-1$
			return null;
		}
		if (c == null) {
			return null;
		}
		return c.getProperties();
	}
}
