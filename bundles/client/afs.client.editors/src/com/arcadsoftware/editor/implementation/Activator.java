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

	static private Activator instance = null;

	private boolean isSingleton = false;
	// private ServiceTracker scriptTracker;

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
		// TODO RAP
		// scriptTracker = new ServiceTracker(context, IScriptManager.class.getName(), null);
		// scriptTracker.open();
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		// scriptTracker.close();
		// scriptTracker = null;
		synchronized (this) {
			if (isSingleton) {
				instance = null;
				isSingleton = false;
			}
		}
		super.stop(context);
	}

	// public IScriptEngine openScriptEngine() {
	// if (scriptTracker == null) {
	// return null;
	// }
	// Object o = scriptTracker.getService();
	// if (o instanceof IScriptManager) {
	// return ((IScriptManager) o).open(IEngineProvider.LANGUAGE_JAVASCRIPT);
	// }
	// return null;
	// }

	// public void closeStriptEngine(IScriptEngine engine) {
	// if (scriptTracker == null) {
	// return;
	// }
	// // WARNING: If there is many Scriptmanager then we are not sure
	// // that the closing manager is the one that open this engine !
	// Object o = scriptTracker.getService();
	// if (o instanceof IScriptManager) {
	// ((IScriptManager) o).close(engine);
	// }
	// }

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

	@Override
	public void error(String message, Throwable e) {
		// TODO Auto-generated method stub

	}

}
