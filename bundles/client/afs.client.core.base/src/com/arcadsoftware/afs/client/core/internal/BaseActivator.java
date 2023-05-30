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
package com.arcadsoftware.afs.client.core.internal;

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
public class BaseActivator extends Plugin  implements ILoggedPlugin{

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
		IRightManagerExtension manager = AFSRightManager.getRightManager();
		if (manager!=null) {
			manager.missingRight(expected);
		}
	}
	
	public static ILoggedPlugin getLogger() {
		return getDefault().logger;
	}
	
	public static void setLogger(ILoggedPlugin logger) {
		getDefault().logger = logger;
	}
		
	public void log(String message) {
		getLog().log(new Status(IStatus.INFO,getBundle().getSymbolicName(), message));
	}
	
	public void log(String message,Throwable e) {
		getLog().log(new Status(IStatus.ERROR,getBundle().getSymbolicName(), message, e));
	}

	public void log(Throwable e) {
		getLog().log(new Status(IStatus.ERROR,getBundle().getSymbolicName(), e.getLocalizedMessage(), e));
	}

	public void warn(String message) {
		getLog().log(new Status(IStatus.WARNING,getBundle().getSymbolicName(), message));
	}
	
	public void warn(String message,Throwable e) {
		getLog().log(new Status(IStatus.WARNING,getBundle().getSymbolicName(), message, e));
	}

	public void debug(String message) {
		if (isDebugging()) {
			getLog().log(new Status(IStatus.INFO,getBundle().getSymbolicName(),Resources.resString(LOGDEBUG_RESOURCE) + message)); //$NON-NLS-1$
		}
	}

	public void debug(String message, Throwable e) {
		if (isDebugging()) {
			getLog().log(new Status(IStatus.INFO,getBundle().getSymbolicName(), Resources.resString(LOGDEBUG_RESOURCE) + message, e)); //$NON-NLS-1$
		}
	}

	public void debug(Throwable e) {
		if (isDebugging()) {
			getLog().log(new Status(IStatus.INFO,getBundle().getSymbolicName(), Resources.resString(LOGDEBUG_RESOURCE) + e.getLocalizedMessage(), e)); //$NON-NLS-1$
		}
	}
	
	public void error(String message, Throwable e) {
		getLog().log(new Status(IStatus.ERROR,getBundle().getSymbolicName(), message, e));
		
	}
	
	public void error(String message) {
		getLog().log(new Status(IStatus.ERROR,getBundle().getSymbolicName(), message));
		
	}
	
}
