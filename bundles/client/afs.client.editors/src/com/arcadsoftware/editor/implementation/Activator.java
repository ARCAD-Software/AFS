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
package com.arcadsoftware.editor.implementation;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.jface.resource.ImageDescriptor;
import org.osgi.framework.BundleContext;

import com.arcadsoftware.afs.framework.ui.plugins.LoggedUIPlugin;

public class Activator extends LoggedUIPlugin {

	static private Activator instance;

	private boolean isSingleton;

	/**
	 * @return the instance
	 */
	public static Activator getInstance() {
		return instance;
	}

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		try {
			resourceBundle = ResourceBundle.getBundle("com.arcadsoftware.editor.swt.messages", Locale.getDefault()); //$NON-NLS-1$
		} catch (final MissingResourceException x) {
			log(x);
		}
		synchronized (this) {
			if (instance == null) {
				instance = this;
				isSingleton = true;
			}
		}
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		synchronized (this) {
			if (isSingleton) {
				instance = null;
				isSingleton = false;
			}
		}
		super.stop(context);
	}

	/**
	 * Returns an image descriptor for the image file at the given plug-in relative path
	 *
	 * @param path
	 *            the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(String path) {
		return imageDescriptorFromPlugin(instance.getBundle().getSymbolicName(), path);
	}

}
