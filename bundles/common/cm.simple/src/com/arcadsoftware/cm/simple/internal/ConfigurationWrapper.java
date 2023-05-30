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
import java.util.Dictionary;

import org.osgi.framework.ServiceReference;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;

/**
 * The ConfigurationWrapper are returned by ConfigurationAdminImpl services. They
 * rely to the ConfigurationContainer for internal operations.
 * 
 * <p>
 * The ConfigurationWrapper is not supposed to live longer than the ConfigurationAdminImpl object that create it.
 * 
 * @author ARCAD Software
 */
public class ConfigurationWrapper implements Configuration {

	private final ServiceReference<ConfigurationAdmin> reference;
	private final ConfigurationContainer conf;

	public ConfigurationWrapper(ServiceReference<ConfigurationAdmin> reference, ConfigurationContainer conf) {
		super();
		this.reference = reference;
		this.conf = conf;
	}
	
	@Override
	public String getPid() {
		return conf.getPid();
	}

	@Override
	public Dictionary<String, Object> getProperties() {
		return conf.getProperties();
	}

	@Override
	public void update(Dictionary<String, ?> properties) throws IOException {
		conf.update(reference, properties);
	}

	@Override
	public void delete() throws IOException {
		conf.delete(reference);
	}

	@Override
	public String getFactoryPid() {
		return conf.getFactoryPid();
	}

	@Override
	public void update() throws IOException {
		conf.update(reference, null);
	}

	@Override
	public void setBundleLocation(String location) {
		conf.setLocation(reference, location);
	}

	@Override
	public String getBundleLocation() {
		return conf.getLocation();
	}

	@Override
	public long getChangeCount() {
		return conf.getChangeCount();
	}

	//@Override
	public Dictionary<String, Object> getProcessedProperties(ServiceReference<?> reference) {
		return conf.getProperties();
	}

	//@Override
	public boolean updateIfDifferent(Dictionary<String, ?> properties) throws IOException {
		if (conf.changed(properties)) {
			conf.update(reference, properties);
			return true;
		}
		return false;
	}
	
	/* Ready for next version of OSGi ! 
	   (Do not remove this code !)
	 
	@Override
	public void addAttributes(ConfigurationAttribute... attrs) throws IOException {
		for (ConfigurationAttribute a: attrs) {
			if (a == ConfigurationAttribute.READ_ONLY) {
				conf.setReadOnly(true);
			} else {
				throw new IOException("The given Configuration attribute can not be persisted.");
			}
		}
	}

	@Override
	public Set<ConfigurationAttribute> getAttributes() {
		HashSet<ConfigurationAttribute> result = new HashSet<Configuration.ConfigurationAttribute>();
		if (conf.isReadOnly()) {
			result.add(ConfigurationAttribute.READ_ONLY);
		}
		return result;
	}

	@Override
	public void removeAttributes(ConfigurationAttribute... attrs) throws IOException {
		for (ConfigurationAttribute a: attrs) {
			if (a == ConfigurationAttribute.READ_ONLY) {
				conf.setReadOnly(false);
			} else {
				throw new IOException("The given Configuration attribute can not be persisted.");
			}
		}
	}/**/
}