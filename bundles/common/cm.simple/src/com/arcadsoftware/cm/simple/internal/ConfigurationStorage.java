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
package com.arcadsoftware.cm.simple.internal;

import java.io.File;
import java.io.IOException;
import java.security.PrivilegedAction;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map.Entry;

import org.osgi.framework.Filter;
import org.osgi.service.cm.ConfigurationPermission;

/**
 * Manage Configuration properties stored in a single file.
 * 
 * @author ARCAD Software
 */
public class ConfigurationStorage extends AbstractConfigurationStorage {
	
	private final ConfigurationAdminFactory factory;
	private final File store;
	private final HashMap<String, ConfigurationContainer> configurations;
	private final HashMap<String, Integer> factoryCount;
	private final boolean readonly;
	private final boolean delayed;
	
	public ConfigurationStorage(File store, ConfigurationAdminFactory factory) {
		super(factory.getActivator());
		this.factory = factory;
		this.store = store;
		configurations = new HashMap<String, ConfigurationContainer>();
		String ro = factory.getActivator().getContext().getProperty("cm.state"); //$NON-NLS-1$
		readonly = (ro != null) && ("readonly".equalsIgnoreCase(ro) || "ro".equalsIgnoreCase(ro)); //$NON-NLS-1$ //$NON-NLS-2$
		delayed = (ro != null) && ("delayed".equalsIgnoreCase(ro) || "delay".equals(ro)); //$NON-NLS-1$ //$NON-NLS-2$
		factoryCount  =new HashMap<String, Integer>();
	}
	
	public synchronized String newPid(String factoryPid) {
		Integer i = factoryCount.get(factoryPid);
		if (i == null) {
			i = new Integer(1);
		} else {
			i = new Integer(i.intValue() + 1);
		}
		factoryCount.put(factoryPid, i);
		return factoryPid + '.' + i;
	}
	
	public synchronized ConfigurationContainer getConfiguration(String pid) {
		return configurations.get(pid);
	}
	
	public synchronized ConfigurationContainer[] getFactoryConfigurations(String factoryPid) {
		ArrayList<ConfigurationContainer> result = new ArrayList<ConfigurationContainer>(configurations.size());
		if (factoryPid != null) {
			for (ConfigurationContainer c: configurations.values()) {
				if (factoryPid.equals(c.getFactoryPid())) {
					result.add(c);
				}
			}
		}
		return result.toArray(new ConfigurationContainer[result.size()]);
	}

	public synchronized ConfigurationContainer[] listConfigurations(Filter filter, String location) {
		ArrayList<ConfigurationContainer> list = new ArrayList<ConfigurationContainer>(configurations.size());
		for (ConfigurationContainer c: configurations.values()) {
			if (((filter == null) || filter.match(c.getTable())) &&
				((location == null) || location.equals(c.getLocation()) || checkPermission(c.getLocation()))) {
				list.add(c);
			}
		}
		return list.toArray(new ConfigurationContainer[list.size()]);
	}

	private boolean checkPermission(String location) {
		if (location != null) {
			SecurityManager sm = System.getSecurityManager();
			if (sm != null) {
				try {
					sm.checkPermission(new ConfigurationPermission(location, ConfigurationPermission.CONFIGURE));
				} catch (SecurityException e) {
					return false;
				}
			}
		}
		return true;
	}

	public boolean update(final ConfigurationContainer c) {
		synchronized (this) {
			configurations.put(c.getPid(), c);
		}
		if (!readonly && !delayed) {
			return Activator.doPrivileged(new PrivilegedAction<Boolean>() {
				@Override
				public Boolean run() {
					try {
						ConfigurationStorage.this.save();
						return true;
					} catch (IOException e) {
						ConfigurationStorage.this.factory.getActivator().error(e);
					}
					return false;
				}
			});
		}
		return false;
	}
	
	public boolean remove(String pid) {
		synchronized (this) {
			configurations.remove(pid);
		}
		if (!readonly && !delayed) {
			return Activator.doPrivileged(new PrivilegedAction<Boolean>() {
				@Override
				public Boolean run() {
					try {
						ConfigurationStorage.this.save();
						return true;
					} catch (IOException e) {
						ConfigurationStorage.this.factory.getActivator().error(e);
					}
					return false;
				}
			});
		}
		return false;
	}
	
	/**
	 * This method is assumed to be call before the storage is used, from the Thread that created this object.
	 * It does not require to be synchronized.
	 * 
	 * @throws IOException
	 */
	public void load() throws IOException {
		load(store, true, true, true);
	}

	@Override
	protected synchronized Hashtable<String, Object> addConfiguration(String factoryPid, String pid) {
		ConfigurationContainer c = configurations.get(pid);
		if (c != null) {
			return c.getTable();
		}
		c = new ConfigurationContainer(factory, pid, factoryPid);
		configurations.put(pid, c);
		return c.getTable();
	}

	@Override
	protected void logError(String message, Throwable e) {
		factory.getActivator().error(message, e);
	}	

	@Override
	protected Integer getFactoryCount(String fpid) {
		return factoryCount.get(fpid);
	}

	@Override
	protected void setFactoryCount(String fpid, Integer value) {
		factoryCount.put(fpid, value);
	}

	@Override
	protected String getFactoryPid(String pid) {
		ConfigurationContainer c = configurations.get(pid);
		if (c != null) {
			return c.getFactoryPid();
		}
		return null;
	}

	public synchronized void save() throws IOException {
		if (readonly) {
			return;
		}
		final HashMap<String, Hashtable<String, Object>> config = new HashMap<String, Hashtable<String,Object>>(configurations.size());
		for(Entry<String, ConfigurationContainer> e: configurations.entrySet()) {
			config.put(e.getKey(), e.getValue().getTable());
		}
		save(store, config, true, true, true);
	}

	public boolean isDelayed() {
		return delayed;
	}

	public int count() {
		return configurations.size();
	}
}