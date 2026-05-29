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

import java.util.Dictionary;
import java.util.HashMap;
import java.util.Map;

import org.osgi.framework.ServiceRegistration;
import org.osgi.service.cm.ConfigurationException;
import org.osgi.service.cm.ManagedService;
import org.osgi.service.cm.ManagedServiceFactory;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;

import com.arcadsoftware.osgi.AbstractConfiguredActivator;
import com.arcadsoftware.osgi.AbstractConfiguredFactoryActivator;

/*
 *
 * @see AbstractConfiguredActivator
 */
public class BundleManagedFactoryService implements ManagedServiceFactory {

	public static final String CONFIGURATIONUPDATED_TOPIC = "com/arcadsoftware/configuration/updated"; //$NON-NLS-1$ 
	
	private AbstractConfiguredFactoryActivator activator;
	
	//private final Map<String, ServiceRegistration<?>> registrations = new HashMap<String, ServiceRegistration<?>>();
	
	/**
	 * @param abstractConfiguredActivator
	 */
	public BundleManagedFactoryService(AbstractConfiguredFactoryActivator activator) {
		super();
		this.activator = activator;
	}

	@Override
	public String getName() {
		return activator.getName();
	}

	@SuppressWarnings("unchecked")
	@Override
	public void updated(String pid, Dictionary<String, ?> properties) throws ConfigurationException {
		deleted(pid);
		
		synchronized (activator) {
			activator.updatedConfiguration(pid, (Dictionary<String, Object>) properties);
		}
	}

	@Override
	public void deleted(String pid) {
		
		synchronized (activator) {
			activator.deleted(pid);
		}
		
		/*ServiceRegistration<?> registration = registrations.remove(pid);

        if (registration != null) {
        	activator.unregister(registration);
        }*/
	}

}
