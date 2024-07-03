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
package com.arcadsoftware.afs.framework.application.internal;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import com.arcadsoftware.afs.framework.application.IConfigurationManager;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	// The shared instance
	private static Activator plugin;

	private volatile IConfigurationManager manager;
	private ResourceBundle resourceBundle;

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static Activator getInstance() {
		return plugin;
	}

	@Override
	public void start(BundleContext context) throws Exception {
		plugin = this;
		super.start(context);
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		plugin = null;
	}

	public synchronized IConfigurationManager getConfigurationManager() {
		if (manager == null) {
			final IExtensionRegistry reg = Platform.getExtensionRegistry();
			for (final IConfigurationElement element : reg
					.getConfigurationElementsFor(IConfigurationManager.CONFIGURATION_MANAGER_EXTENSION)) {
				try {
					manager = (IConfigurationManager) element.createExecutableExtension("class"); //$NON-NLS-1$
				} catch (final Exception e) {
					getLog().log(new Status(IStatus.ERROR, getBundle().getSymbolicName(),
							"Unable to Initialize the ConfiguratorManager extension named: " + element.getName(), e));
				}
			}
		}
		return manager;
	}

	public synchronized void setConfigurationManager(IConfigurationManager manager) {
		this.manager = manager;
	}

	/**
	 * Returns the plugin's resource bundle,
	 */
	public ResourceBundle getResourceBundle() {
		return resourceBundle;
	}

	/**
	 * Return the key associated value into the downloaded properties file.
	 *
	 * @param key
	 *            the key value.
	 * @return key is the corresponding string is not found.
	 */

	public static String resString(String key) {
		final ResourceBundle bundle = getInstance().getResourceBundle();
		try {
			final String value = bundle.getString(key);
			if (value.startsWith("!")) { //$NON-NLS-1$
				return resString(value.substring(1));
			} else { // $NON-NLS-1$
				return value;
			}
		} catch (final MissingResourceException e) {
			return key;
		}
	}

	public void log(String message) {
		getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(), message));
	}

	public void log(String message, Throwable e) {
		getLog().log(new Status(IStatus.ERROR, getBundle().getSymbolicName(), message, e));
	}

	public void log(Throwable e) {
		getLog().log(new Status(IStatus.ERROR, getBundle().getSymbolicName(), e.getLocalizedMessage(), e));
	}

}
