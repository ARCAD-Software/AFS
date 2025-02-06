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
package com.arcadsoftware.afs.framework.help.internal;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;

import com.arcadsoftware.afs.framework.help.dynamicHelp.DynamicHelpService;
import com.arcadsoftware.afs.framework.services.IDynamicHelpService;
import com.arcadsoftware.afs.framework.ui.plugins.AbstractAFSUIPlugin;

public class Activator extends AbstractAFSUIPlugin {

	private ServiceRegistration<?> dhsr;

	@Override
	public void start(BundleContext context) throws Exception {
		dhsr = getBundle().getBundleContext().registerService(IDynamicHelpService.clazz, new DynamicHelpService(this), null);
		super.start(context);
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		if (dhsr != null) {
			dhsr.unregister();
			dhsr = null;
		}
	}

	@Override
	public String getResourceBundleName() {
		return "com.arcadsoftware.afs.framework.help.resources"; //$NON-NLS-1$;
	}

	@Override
	protected ResourceBundle loadResourceBundle(String bundleName, Locale local)
			throws MissingResourceException {
		return ResourceBundle.getBundle(bundleName, Locale.getDefault());
	}

	@Override
	protected void fillImageRegistry() {}

	@Override
	protected String getApplicationTitle() {
		return ""; //$NON-NLS-1$
	}
	
	public boolean isRegisteredDynamicHelp() {
		return dhsr != null;
	}

}
