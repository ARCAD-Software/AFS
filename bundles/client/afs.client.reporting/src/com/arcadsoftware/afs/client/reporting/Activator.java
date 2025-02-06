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
package com.arcadsoftware.afs.client.reporting;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.osgi.framework.BundleContext;

import com.arcadsoftware.afs.framework.ui.plugins.AbstractAFSUIPlugin;
import com.arcadsoftware.afs.framework.ui.plugins.ILocalizationProvider;

public class Activator extends AbstractAFSUIPlugin implements ILocalizationProvider {

	public static final String PLUGIN_ID = "com.arcadsoftware.afs.client.reporting"; //$NON-NLS-1$
	public static final String BUNDLE_ID = PLUGIN_ID + ".resources"; //$NON-NLS-1$
	public static final String TITLE = ""; //$NON-NLS-1$

	public static final String PROXYURL = "/proxy/birt/"; //$NON-NLS-1$
	public static final String XMLLIST_URL = "/proxy/birt/report/"; //$NON-NLS-1$

	public static final String REPORTEDITOR_ID = "com.arcadsoftware.afs.client.reporting.ui.editors.reportpreview"; //$NON-NLS-1$
	// The shared instance
	private static Activator plugin;

	/**
	 * The constructor
	 */
	public Activator() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	@Override
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

	/**
	 * Returns the plugin's resource bundle,
	 */
	@Override
	public ResourceBundle getResourceBundle() {
		return resourceBundle;
	}

	public static String resString(String key) {
		return plugin.getResourceString(key);
	}

	@Override
	public String getResourceBundleName() {
		return BUNDLE_ID;
	}

	@Override
	protected void fillImageRegistry() {
	}

	@Override
	protected ResourceBundle loadResourceBundle(String bundleName, Locale local)
			throws MissingResourceException {
		return ResourceBundle.getBundle(bundleName, Locale.getDefault());
	}

	@Override
	protected String getApplicationTitle() {
		return TITLE;
	}
}
