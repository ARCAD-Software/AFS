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
package com.arcadsoftware.afs.framework.ui.internal;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.osgi.framework.BundleContext;
import org.osgi.util.tracker.ServiceTracker;

import com.arcadsoftware.afs.framework.services.IDynamicHelpService;
import com.arcadsoftware.afs.framework.ui.help.DynamicHelp;
import com.arcadsoftware.afs.framework.ui.plugins.AbstractAFSUIPlugin;
import com.arcadsoftware.afs.framework.ui.plugins.ILocalizationProvider;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractAFSUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "com.arcadsoftware.afs.framework.ui"; //$NON-NLS-1$
	// The shared instance
	private static Activator plugin;
	private static ILocalizationProvider messageProvider;

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}
	
	public static void setMessageProvider(ILocalizationProvider msgProvider) {
		messageProvider = msgProvider;
	}
	
	public static String resString(String key) {
		if (messageProvider==null)
			return plugin.getResourceString(key);
		else {
			String value = messageProvider.getResourceString(key);
			if ((value==null) || value.equalsIgnoreCase("") || value.equalsIgnoreCase(key)) {
				return plugin.getResourceString(key);
			}
		}
		return key;
	}

	private ServiceTracker<?,?> helpTracker;

	public void start(BundleContext context) throws Exception {
		plugin = this;
		super.start(context);
		helpTracker = new ServiceTracker<>(context, IDynamicHelpService.class.getName(), null);
		helpTracker.open();
		try {
			DynamicHelp.init(this);
		} catch (Exception e) {
			log(e);
		}
	}

	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		if (helpTracker != null) {
			helpTracker.close();
			helpTracker = null;
		}
		plugin = null;
	}

	@Override
	public String getResourceBundleName() {
		return "com.arcadsoftware.afs.framework.ui.internal.resources";
	}
	
	@Override
	protected void fillImageRegistry() {
		putImageInRegistry("DLG_ARCAD", getIconPath() + "arcad-16.png"); //$NON-NLS-1$ //$NON-NLS-2$		
	}

	@Override
	protected ResourceBundle loadResourceBundle(String bundleName, Locale local) throws MissingResourceException {
		return ResourceBundle.getBundle(bundleName, Locale.getDefault());		
	}

	@Override
	protected String getApplicationTitle() {
		return "ARCAD-Foundation Services";
	}

	public IDynamicHelpService getDynamicHelpService() {
		if (helpTracker != null) {
			Object s = helpTracker.getService();
			if (s instanceof IDynamicHelpService) {
				return (IDynamicHelpService) s;
			}
		}
		return null;
	}
}