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
package com.arcadsoftware.client.editors.swtwidgets.internal;

import org.osgi.framework.BundleContext;

import com.arcadsoftware.afs.framework.ui.plugins.LoggedUIPlugin;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends LoggedUIPlugin {

	private static Activator instance;

	/**
	 * Returns the shared instance
	 * 
	 * @return the shared instance
	 */
	public static Activator getInstance() {
		return instance;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.runtime.Plugins#start(org.osgi.framework.BundleContext)
	 */
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		instance = this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.runtime.Plugin#stop(org.osgi.framework.BundleContext)
	 */
	@Override
	public void stop(BundleContext context) throws Exception {
		instance = null;
		super.stop(context);
	}

	/**
	 * Returns the string from the plugin's resource bundle, or 'key' if not found.
	 * 
	 * @param key
	 *            The resource key.
	 * @return The string from the plugin's resource bundle, or 'key' if not found.
	 */
	public static String getResourceString(String key) {
		return getInstance().resString(key);
	}

	
	public void error(String message, Throwable e) {
		// TODO Auto-generated method stub
		
	}
}