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
package com.arcadsoftware.afs.client.ssh.internal;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.osgi.framework.BundleContext;

import com.arcadsoftware.afs.framework.ui.plugins.AbstractAFSUIPlugin;
import com.arcadsoftware.afs.framework.ui.plugins.ILocalizationProvider;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractAFSUIPlugin implements ILocalizationProvider {

	// The plug-in ID
	public static final String PLUGIN_ID = "com.arcadsoftware.afs.client.ssh"; //$NON-NLS-1$
	public static final String BUNDLE_ID = PLUGIN_ID + ".resources"; //$NON-NLS-1$
	public static final String TITLE = ""; //$NON-NLS-1$

	// The shared instance
	private static Activator plugin;

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

	public static String resString(final String key, final Object... params) {
		return String.format(plugin.getResourceString(key), params);
	}

	/**
	 * The constructor
	 */
	public Activator() {
	}

	@Override
	protected void fillImageRegistry() {
	}

	@Override
	protected String getApplicationTitle() {
		return TITLE;
	}

	@Override
	public String getResourceBundleName() {
		return BUNDLE_ID;
	}

	@Override
	protected ResourceBundle loadResourceBundle(final String bundleName, final Locale local)
			throws MissingResourceException {
		return ResourceBundle.getBundle(bundleName, Locale.getDefault());
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.
	 * BundleContext)
	 */
	@Override
	public void start(final BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		// getDirectories();
		// updateProject();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	@Override
	public void stop(final BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

}
