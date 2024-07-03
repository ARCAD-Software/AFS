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
package com.arcadsoftware.afs.framework.ui.plugins;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.arcadsoftware.afs.framework.ui.internal.Activator;
import com.arcadsoftware.osgi.ILoggedPlugin;

/**
 * Abstract implementation of a plugin Activator with logging facility based on the Eclipse Log interface.
 */
public abstract class LoggedUIPlugin extends AbstractUIPlugin implements ILoggedPlugin {

	protected static final String ICON_PATH = "icons/";

	protected ResourceBundle resourceBundle;

	@Override
	public void log(String message) {
		getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(), message));
	}

	@Override
	public void log(String message, Throwable e) {
		getLog().log(new Status(IStatus.ERROR, getBundle().getSymbolicName(), message, e));
	}

	@Override
	public void log(Throwable e) {
		getLog().log(new Status(IStatus.ERROR, getBundle().getSymbolicName(), e.getLocalizedMessage(), e));
	}

	@Override
	public void warn(String message) {
		getLog().log(new Status(IStatus.WARNING, getBundle().getSymbolicName(), message));
	}

	@Override
	public void warn(String message, Throwable e) {
		getLog().log(new Status(IStatus.WARNING, getBundle().getSymbolicName(), message, e));
	}

	@Override
	public void debug(String message) {
		if (isDebugging()) {
			getLog().log(
					new Status(IStatus.INFO, getBundle().getSymbolicName(),
							Activator.resString("Log.debug") + message)); //$NON-NLS-1$
		}
	}

	@Override
	public void debug(String message, Throwable e) {
		if (isDebugging()) {
			getLog().log(
					new Status(IStatus.INFO, getBundle().getSymbolicName(),
							Activator.resString("Log.debug") + message, e)); //$NON-NLS-1$
		}
	}

	@Override
	public void debug(Throwable e) {
		if (isDebugging()) {
			getLog().log(
					new Status(IStatus.INFO, getBundle().getSymbolicName(),
							Activator.resString("Log.debug") + e.getLocalizedMessage(), e)); //$NON-NLS-1$
		}
	}

	/**
	 * Returns the plugin's resource bundle,
	 *
	 * @return resource bundle
	 */
	public ResourceBundle getResourceBundle() {
		return resourceBundle;
	}

	/**
	 * Returns the string from the plugin's resource bundle, or 'key' if not found.
	 */
	public String resString(String key) {
		final ResourceBundle bundle = resourceBundle;
		if (bundle == null) {
			return key;
		}
		try {
			final String value = bundle.getString(key);
			if (value.startsWith("!")) { //$NON-NLS-1$
				final String newKey = value.substring(1, value.length());
				return resString(newKey);
			}
			return bundle.getString(key);
		} catch (final MissingResourceException e) {
			return key;
		}
	}

	/**
	 * Convenience method for getting the current shell.
	 *
	 * @return the shell
	 */
	public static Shell getShell() {
		return PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
	}

	protected ImageDescriptor putImageInRegistry(String id, String fileName) {
		final ImageDescriptor fid = getPluginImage(fileName);
		getImageRegistry().put(id, fid);
		return fid;
	}

	public ImageDescriptor getPluginImage(String fileName) {
		try {
			return ImageDescriptor.createFromURL(new URL(getBundle().getEntry("/"), fileName)); //$NON-NLS-1$
		} catch (final MalformedURLException e) {
			return null;
		}
	}

	public Image getImage(String key) {
		if ((key == null) || (key.length() == 0)) {
			return null;
		}

		Image image = null;
		try {
			image = getImageRegistry().get(key);
		} catch (final Throwable t) {
			return null;
		}
		return image;
	}

	public void manageMessage(Exception e) {

	}

}
