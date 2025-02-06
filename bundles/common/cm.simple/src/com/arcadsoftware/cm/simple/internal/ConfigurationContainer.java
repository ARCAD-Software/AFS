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
package com.arcadsoftware.cm.simple.internal;

import java.util.Dictionary;
import java.util.Enumeration;
import java.util.Hashtable;

import org.osgi.framework.Constants;
import org.osgi.framework.ServiceReference;
import org.osgi.service.cm.ConfigurationAdmin;

public class ConfigurationContainer {

	private volatile boolean deleted;
	private String location;
	private volatile int changeCount;
	private final String pid;
	private final String factoryPid;
	private final Hashtable<String, Object> properties;
	private final ConfigurationAdminFactory factory;
	private volatile boolean readOnly;
	
	public ConfigurationContainer(ConfigurationAdminFactory factory, String pid, String factoryPid) {
		super();
		this.factory = factory;
		this.pid = pid;
		this.factoryPid = factoryPid;
		this.properties = new Hashtable<String, Object>();
		properties.put(Constants.SERVICE_PID, pid);
		if (factoryPid != null) {
			properties.put(ConfigurationAdmin.SERVICE_FACTORYPID, factoryPid);
		}
	}
	
	public boolean changed(Dictionary<String, ?> newProperties) {
		if (newProperties == null) {
			return isNotEmpty(properties);
		}
		Enumeration<String> enu = newProperties.keys();
		while (enu.hasMoreElements()) {
			String key = enu.nextElement();
			if (ConfigurationStorage.isNotIgnored(key)) {
				Object old_val = properties.get(key);
				Object new_val = newProperties.get(key);
				if ((old_val == null) || !new_val.equals(old_val)) {
					return true;
				}
			}
		}		
		enu = properties.keys();
		while (enu.hasMoreElements()) {
			String key = enu.nextElement();
			if (ConfigurationStorage.isNotIgnored(key)) {
				if (newProperties.get(key) == null) {
					return true;
				}
			}
		}
		return false;
	}

	private boolean isNotEmpty(Dictionary<String, Object> properties) {
		Enumeration<String> enu = properties.keys();
		while (enu.hasMoreElements()) {
			if (ConfigurationStorage.isNotIgnored(enu.nextElement())) {
				return true;
			}
		}
		return false;
	}

	public String getPid() {
		return pid;
	}

	public void update(ServiceReference<ConfigurationAdmin> reference, Dictionary<String, ?> properties) {
		if (!isReadOnly() && isNotDeleted()) {
			if ((properties != null) && changed(properties)) {
				this.properties.clear();
				Enumeration<String> keys = properties.keys();
				while (keys.hasMoreElements()) {
					String k = keys.nextElement();
					this.properties.put(k, properties.get(k));
				}
				synchronized (this) {
					if (deleted) {
						return;
					}
					factory.getStorage().update(this);
				}
			}
			synchronized (this) {
				changeCount++;
			}
			if (properties != null) {
				factory.configurationUpdate(reference, this);
			} else { // do not send event to listeners !
				factory.configurationUpdate(null, this);
			}
		}
	}

	public void delete(ServiceReference<ConfigurationAdmin> reference) {
		if (setDelete()) {
			factory.configurationDeletion(reference, this);
			factory.getStorage().remove(pid);
		}
	}

	private synchronized boolean isNotDeleted() {
		return !deleted;
	}
	
	private synchronized boolean setDelete() {
		if (!deleted) {
			deleted = true;
			return true;
		}
		return false;
	}
	
	public String getFactoryPid() {
		return factoryPid;
	}

	public String setLocation(ServiceReference<ConfigurationAdmin> reference, String location) {
		String oldLocation = this.location;
		this.location = location;
		// Setting a null location to something should not trigger updates ! (null location are accessible to every ones). 
		if ((reference != null) && isNotDeleted() && (oldLocation != null) && !oldLocation.equals(location)) {
			factory.configurationUpdateLocation(reference, this, oldLocation);
		}
		return oldLocation;
	}

	public String getLocation() {
		return location;
	}

	public long getChangeCount() {
		return changeCount;
	}

	protected Hashtable<String, Object> getTable() {
		return properties;
	}

	public Dictionary<String, Object> getProperties() {
		return new Hashtable<String, Object>(properties);
	}

	public synchronized boolean isReadOnly() {
		return readOnly;
	}

	public synchronized void setReadOnly(boolean readOnly) {
		this.readOnly = readOnly;
	}

}
