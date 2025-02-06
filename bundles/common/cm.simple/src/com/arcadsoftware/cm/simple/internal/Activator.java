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
package com.arcadsoftware.cm.simple.internal;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;

import org.osgi.framework.BundleContext;

import com.arcadsoftware.osgi.AbstractActivator;

public class Activator extends AbstractActivator {

	private volatile ConfigurationAdminFactory manager;
	
	@Override
	public void start(BundleContext bundleContext) throws Exception {
		super.start(bundleContext);
		manager = new ConfigurationAdminFactory(this, getStoreFile());
		manager.open();
	}

	@Override
	public void stop(BundleContext bundleContext) throws Exception {
		super.stop(bundleContext);
		manager.close();
	}

	private File getStoreFile() {
		String loc = getContext().getProperty("osgi.cm.storefile"); //$NON-NLS-1$
		File l = null;
		if ((loc != null) && !loc.isEmpty()) {
			l = new File(loc);
		} else {
			loc = getContext().getProperty("osgi.configuration.area"); //$NON-NLS-1$
			if ((loc == null) || loc.isEmpty()) {
				l = new File("./configuration"); //$NON-NLS-1$
			} else if (loc.startsWith("file:")) { //$NON-NLS-1$
				try {
					l = new File(new URI(loc));
				} catch (URISyntaxException e) {
					l = new File("./configuration"); //$NON-NLS-1$
				}
			} else {
				l = new File(loc);
			}
			if (!l.isDirectory()) {
				l = new File(System.getProperty("user.dir") + "/.configuration"); //$NON-NLS-1$ //$NON-NLS-2$
				l.mkdirs();
			}
			l = new File(l, "osgi.cm.ini"); //$NON-NLS-1$
		}
		// Logger is not started when this bundle start...
		debug("Configuration store file: " + l.getAbsolutePath());
		return l;
	}
}