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
package com.arcadsoftware.afs.client.server.admin.common;

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
	public static final String PLUGIN_ID = "com.arcadsoftware.afs.client.server.admin.common"; //$NON-NLS-1$
	public static final int LOGLVL_FATAL = 3;

	private static Activator instance;

	public static Activator getInstance() {
		return instance;
	}

	@Override
	public void start(BundleContext context) throws Exception {
		instance = this;
		super.start(context);
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		instance = null;
	}

	@Override
	protected void fillImageRegistry() {
	}

	public void log(int errorlevel, String message) {

	}

	public void log(int errorlevel, String message, Throwable e) {

	}

	@Override
	public String getResourceBundleName() {
		return "com.arcadsoftware.afs.client.server.admin.common.resources"; //$NON-NLS-1$
	}

	public static String resString(String key) {
		return instance.getResourceString(key);
	}

	@Override
	protected ResourceBundle loadResourceBundle(String bundleName, Locale local)
			throws MissingResourceException {
		return ResourceBundle.getBundle(bundleName, Locale.getDefault());
	}

	@Override
	protected String getApplicationTitle() {
		return "ARCAD-Server Setting Admin. Console"; //$NON-NLS-1$
	}

}
