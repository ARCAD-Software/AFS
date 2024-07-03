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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;

import com.arcadsoftware.afs.framework.ui.internal.Activator;
import com.arcadsoftware.osgi.ILoggedPlugin;

/**
 * Abstract plugin class used to provide some helpers methods and object to plugins activators.
 */
public abstract class LoggedPlugin extends Plugin implements ILoggedPlugin {

	/**
	 * Log an simple information.
	 * 
	 * @param message
	 *            to be logged
	 */
	@Override
	public void log(String message) {
		getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(), message));
	}

	/**
	 * Log an error.
	 *
	 * @param message
	 *            the error specific message
	 * @param e
	 *            the exception
	 */
	@Override
	public void log(String message, Throwable e) {
		getLog().log(new Status(IStatus.ERROR, getBundle().getSymbolicName(), message, e));
	}

	/**
	 * Log an error.
	 *
	 * @param e
	 */
	@Override
	public void log(Throwable e) {
		getLog().log(new Status(IStatus.ERROR, getBundle().getSymbolicName(), e.getLocalizedMessage(), e));
	}

	/**
	 * Log a warning message.
	 *
	 * @param message
	 */
	@Override
	public void warn(String message) {
		getLog().log(new Status(IStatus.WARNING, getBundle().getSymbolicName(), message));
	}

	/**
	 * Log a warning error.
	 *
	 * @param message
	 * @param e
	 *            the exception
	 */
	@Override
	public void warn(String message, Throwable e) {
		getLog().log(new Status(IStatus.WARNING, getBundle().getSymbolicName(), message, e));
	}

	/**
	 * If the plugin is in debug mode then log the message
	 *
	 * @param message
	 */
	@Override
	public void debug(String message) {
		if (isDebugging()) {
			getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(),
					Activator.resString("Log.debug") + message)); //$NON-NLS-1$
		}
	}

	/**
	 * If the plugin is in debug mode then log the exception
	 *
	 * @param message
	 * @param e
	 */
	@Override
	public void debug(String message, Throwable e) {
		if (isDebugging()) {
			getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(),
					Activator.resString("Log.debug") + message, e)); //$NON-NLS-1$
		}
	}

	/**
	 * If the plugin is in debug mode then log the exception
	 *
	 * @param e
	 */
	@Override
	public void debug(Throwable e) {
		if (isDebugging()) {
			getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(),
					Activator.resString("Log.debug") + e.getLocalizedMessage(), e)); //$NON-NLS-1$
		}
	}
}
