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
package com.arcadsoftware.afs.client.core.internal;

import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.osgi.framework.BundleContext;

import com.arcadsoftware.afs.client.core.AFSRightManager;
import com.arcadsoftware.afs.client.core.IRightManagerExtension;
import com.arcadsoftware.osgi.ILoggedPlugin;

/**
 * The activator class controls the plug-in life cycle
 */
public class BaseActivator extends Plugin implements ILoggedPlugin {

	public static String format(String message, Object... objects) {
		if (message == null) {
			return ""; //$NON-NLS-1$
		}
		StringBuilder sb = new StringBuilder();
		int i = message.indexOf('{');
		int p = 0;
		int x = 0;
		while (i >= 0) {
			if ((i < message.length() - 1) && (message.charAt(i + 1) == '}')) {
				if ((i > 0) && (message.charAt(i - 1) == '\\')) {
					if ((i > 1) && (message.charAt(i - 2) == '\\')) {
						// Just remove le second '\' character and process to the placeholder replacement.
						sb.append(message.substring(p, i - 1));
						sb.append(format(x++, objects));
						p = i + 2;
					}
				}
			}
			i = message.indexOf('{', i + 1);
		}
		if (p < message.length()) {
			sb.append(message.substring(p));
		}
		return sb.toString();
	}
	
	private static Object format(int i, Object[] objects) {
		if ((i < objects.length)) {
			Object o = objects[i];
			if (o == null) {
				return "null"; //$NON-NLS-1$
			}
			Class<?> c = o.getClass();
			if (c.isArray()) {
				if (c == byte[].class) {
					return Arrays.toString((byte[]) o);
				}
				if (c == short[].class) {
					return Arrays.toString((short[]) o);
				}
				if (c == int[].class) {
					return Arrays.toString((int[]) o);
				}
				if (c == long[].class) {
					return Arrays.toString((long[]) o);
				}
				if (c == float[].class) {
					return Arrays.toString((float[]) o);
				}
				if (c == double[].class) {
					return Arrays.toString((double[]) o);
				} 
				if (c == boolean[].class) {
					return Arrays.toString((boolean[]) o);
				} 
				if (c == char[].class) {
					return Arrays.toString((char[]) o);
				}
				return Arrays.deepToString((Object[]) o);
			}
			return o.toString();
		}
		return null;
	}

	// The plug-in ID
	public static final String TITLE = ""; //$NON-NLS-1$

	private static final String LOGDEBUG_RESOURCE = "Log.debug";

	// The shared instance
	private static BaseActivator plugin;

	private ILoggedPlugin logger;

	/**
	 * The constructor
	 */
	public BaseActivator() {
		logger = this;
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
	public static BaseActivator getDefault() {
		return plugin;
	}

	public void missingRight(List<Integer> expected) {
		final IRightManagerExtension manager = AFSRightManager.getRightManager();
		if (manager != null) {
			manager.missingRight(expected);
		}
	}

	public static ILoggedPlugin getLogger() {
		return getDefault().logger;
	}

	public static void setLogger(ILoggedPlugin logger) {
		getDefault().logger = logger;
	}

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
			getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(),
					Resources.resString(LOGDEBUG_RESOURCE) + message));
		}
	}

	@Override
	public void debug(String message, Throwable e) {
		if (isDebugging()) {
			getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(),
					Resources.resString(LOGDEBUG_RESOURCE) + message, e));
		}
	}

	@Override
	public void debug(Throwable e) {
		if (isDebugging()) {
			getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(),
					Resources.resString(LOGDEBUG_RESOURCE) + e.getLocalizedMessage(), e));
		}
	}

	@Override
	public void error(String message, Throwable e) {
		getLog().log(new Status(IStatus.ERROR, getBundle().getSymbolicName(), message, e));
	}

	public void error(String message) {
		getLog().log(new Status(IStatus.ERROR, getBundle().getSymbolicName(), message));
	}

	@Override
	public void info(String message) {
		getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(), message));
	}

	@Override
	public void info(String message, Throwable e) {
		getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(), message, e));
	}

	@Override
	public void info(Throwable e) {
		getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(), e.getLocalizedMessage(), e));
	}
	
	@Override
	public void info(String message, Object... objects) {
		Throwable e = null;
		if ((objects.length > 0) && (objects[objects.length - 1] instanceof Throwable)) {
			e = ((Throwable) objects[objects.length - 1]);
		}
		getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(), format(message, objects), e));
	}

	@Override
	public void error(Throwable e) {
		getLog().log(new Status(IStatus.ERROR, getBundle().getSymbolicName(), e.getLocalizedMessage(), e));
	}

	@Override
	public void error(String message, Object... objects) {
		Throwable e = null;
		if ((objects.length > 0) && (objects[objects.length - 1] instanceof Throwable)) {
			e = ((Throwable) objects[objects.length - 1]);
		}
		getLog().log(new Status(IStatus.ERROR, getBundle().getSymbolicName(), format(message, objects), e));
	}

	@Override
	public void warn(Throwable e) {
		getLog().log(new Status(IStatus.WARNING, getBundle().getSymbolicName(), e.getLocalizedMessage(), e));
	}

	@Override
	public void warn(String message, Object... objects) {
		Throwable e = null;
		if ((objects.length > 0) && (objects[objects.length - 1] instanceof Throwable)) {
			e = ((Throwable) objects[objects.length - 1]);
		}
		getLog().log(new Status(IStatus.WARNING, getBundle().getSymbolicName(), format(message, objects), e));
	}

	@Override
	public void debug(String message, Object... objects) {
		if (isDebugging()) {
			Throwable e = null;
			if ((objects.length > 0) && (objects[objects.length - 1] instanceof Throwable)) {
				e = ((Throwable) objects[objects.length - 1]);
			}
			getLog().log(new Status(IStatus.INFO, getBundle().getSymbolicName(), Resources.resString(LOGDEBUG_RESOURCE) + format(message, objects), e)); //$NON-NLS-1$
		}
	}

	@Override
	public void trace(String message) {}

	@Override
	public void trace(String message, Throwable e) {}

	@Override
	public void trace(Throwable e) {}

	@Override
	public void trace(String message, Object... objects) {}

	@Override
	public void audit(String message, Throwable e) {
		error(message, e);
	}

	@Override
	public void audit(String message) {
		error(message);
	}

	@Override
	public void audit(Throwable e) {
		error(e);
	}

	@Override
	public void audit(String message, Object... objects) {
		error(message, objects);
	}

}
