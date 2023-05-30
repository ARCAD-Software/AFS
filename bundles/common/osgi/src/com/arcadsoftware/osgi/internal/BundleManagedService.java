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
package com.arcadsoftware.osgi.internal;

import java.util.Dictionary;
import java.util.HashMap;
import java.util.Map;

import org.osgi.service.cm.ConfigurationException;
import org.osgi.service.cm.ManagedService;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;

import com.arcadsoftware.osgi.AbstractConfiguredActivator;

/*
 *
 * @see AbstractConfiguredActivator
 */
public class BundleManagedService implements ManagedService {

	public static final String CONFIGURATIONUPDATED_TOPIC = "com/arcadsoftware/configuration/updated"; //$NON-NLS-1$ 
	
	private AbstractConfiguredActivator activator;
	
	/**
	 * @param abstractConfiguredActivator
	 */
	public BundleManagedService(AbstractConfiguredActivator activator) {
		super();
		this.activator = activator;
	}

	@SuppressWarnings("unchecked")
	@Override
	public void updated(Dictionary<String, ?> properties) throws ConfigurationException {
		synchronized (activator) {
			try {
				activator.updatedConfiguration((Dictionary<String, Object>) properties);
				EventAdmin ea = (EventAdmin) activator.getService(EventAdmin.class.getName());
				if (ea != null) {
					final Map<String,String> prop = new HashMap<>();
					prop.put("pid", activator.getContext().getBundle().getSymbolicName()); //$NON-NLS-1$
					ea.postEvent(new Event(CONFIGURATIONUPDATED_TOPIC, prop ));
				}
			} catch (Exception e) {
				activator.error(Messages.getString("osgi.ErrorUpdatingProperties"), e); //$NON-NLS-1$
				throw new ConfigurationException(Messages.getString("osgi.ConfigurationPropertyErrorFrom"),Messages.getString("AbstractManagedService.ConfigurationErrorReasons"),e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
	}

}
