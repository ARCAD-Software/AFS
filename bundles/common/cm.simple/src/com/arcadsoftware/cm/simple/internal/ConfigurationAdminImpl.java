/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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

import java.io.IOException;
import java.security.Permission;

import org.osgi.framework.Bundle;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.cm.ConfigurationPermission;

public class ConfigurationAdminImpl implements ConfigurationAdmin {

	static private final Permission PREMISSION_ALLCFG = new ConfigurationPermission("*", ConfigurationPermission.CONFIGURE); //$NON-NLS-1$
	
	private final Bundle bundle;
	private final ConfigurationAdminFactory factory;
	private final ServiceRegistration<ConfigurationAdmin> registration;
	
	public ConfigurationAdminImpl(Bundle bundle, ServiceRegistration<ConfigurationAdmin> registration, ConfigurationAdminFactory factory) {
		super();
		this.bundle = bundle;
		this.factory = factory;
		this.registration = registration;
	}
	
	private void checkConfigurePermission(String location) throws SecurityException {
		SecurityManager sm = System.getSecurityManager();
		if (sm != null) {
			String bl = bundle.getLocation();
			if ((bl == null) || !bl.equals(location)) {
				if (location == null) {
					sm.checkPermission(PREMISSION_ALLCFG);
				} else {
					sm.checkPermission(new ConfigurationPermission(location, ConfigurationPermission.CONFIGURE));
				}
			}
		}
	}

	@Override
	public Configuration createFactoryConfiguration(String factoryPid) throws IOException {
		return createFactoryConfiguration(factoryPid, bundle.getLocation());
	}

	@Override
	public Configuration createFactoryConfiguration(String factoryPid, String location) throws IOException {
		if ((factoryPid == null) || factoryPid.isEmpty()) {
			return null;
		}
		checkConfigurePermission(location);
		ConfigurationContainer c = new ConfigurationContainer(factory, factory.getStorage().newPid(factoryPid), factoryPid);
		c.setLocation(null, location);
		return new ConfigurationWrapper(registration.getReference(), c);
	}

	@Override
	public Configuration getConfiguration(String pid, String location) throws IOException {
		if ((pid == null) || pid.isEmpty()) {
			return null;
		}
		checkConfigurePermission(location);
		ConfigurationContainer c = factory.getStorage().getConfiguration(pid);
		if (c == null) {
			c = new ConfigurationContainer(factory, pid, null);
		}
		c.setLocation(null, location);
		return new ConfigurationWrapper(registration.getReference(), c);
	}

	@Override
	public Configuration getConfiguration(String pid) throws IOException {
		return getConfiguration(pid, bundle.getLocation());
	}

	@Override
	public Configuration[] listConfigurations(String filter) throws IOException, InvalidSyntaxException {
		String location = null;
		try {
			checkConfigurePermission(null);
		} catch (SecurityException e) {
			location = bundle.getLocation();
		}
		ConfigurationContainer[] result;
		if (filter == null) {
			result = factory.getStorage().listConfigurations(null, location);
		} else {
			result = factory.getStorage().listConfigurations(FrameworkUtil.createFilter(filter), location);
		}
		ServiceReference<ConfigurationAdmin> ref = registration.getReference();
		Configuration[] w = new Configuration[result.length];
		for(int i = 0; i < w.length; i++) {
			w[i] = new ConfigurationWrapper(ref, result[i]);
		}
		return w;
	}

	//Override
	public Configuration getFactoryConfiguration(String factoryPid, String name, String location) throws IOException {
		if ((factoryPid == null) || factoryPid.isEmpty()) {
			return null;
		}
		if (location != null) {
			checkConfigurePermission(location);
		}
		String pid = factoryPid + '~' + name;
		ConfigurationContainer c = factory.getStorage().getConfiguration(pid);
		if (c == null) {
			c = new ConfigurationContainer(factory, pid, factoryPid);
		}
		c.setLocation(null, location);
		return new ConfigurationWrapper(registration.getReference(), c);
	}

	//Override
	public Configuration getFactoryConfiguration(String factoryPid, String name) throws IOException {
		return getFactoryConfiguration(factoryPid, name, null);
	}
}
