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
package com.arcadsoftware.metadata.registry.internal;

import java.io.File;
import java.util.Dictionary;

import org.osgi.framework.BundleContext;

import com.arcadsoftware.metadata.registry.XmlRegistry;
import com.arcadsoftware.osgi.AbstractFileRepositoryActivator;
import com.arcadsoftware.osgi.FileSystemTracker;

public class Activator extends AbstractFileRepositoryActivator {

	public static final String INTERNAL = "Internal:"; //$NON-NLS-1$
	public static final String EXTERNAL = "External:"; //$NON-NLS-1$
	
	private RegistryBundleListener rbl;
	private XmlRegistry registry;

	@Override
	public void start(BundleContext bundleContext) throws Exception {
		super.start(bundleContext);
		registry = new XmlRegistry(this);
		registry.setContext(bundleContext);
		rbl = new RegistryBundleListener(this,registry);
		// Register a Listener to hook Bundle start event.
		bundleContext.addBundleListener(rbl);
		// Register the Registry service for programmatically adding of Entities
		registerService(XmlRegistry.clazz, registry, XmlRegistry.PROP_REGISTRYNAME, "XML Entities Registry"); //$NON-NLS-1$
	}

	@Override
	public void stop(BundleContext bundleContext) throws Exception {
		super.stop(bundleContext);
		bundleContext.removeBundleListener(rbl);
	}

	@Override
	public boolean initializeConfiguration(Dictionary<String, Object> properties) {
		if (properties != null) {
			if (properties.get(PROP_WATCHPERIOD) == null) {
				properties.put(PROP_WATCHPERIOD, 120);
			}
		}
		return false;
	}

	@Override
	protected String getDefaultDirName() {
		return "files/entities"; //$NON-NLS-1$
	}

	@Override
	protected String getFileExtension() {
		return ".xml"; //$NON-NLS-1$
	}

	@Override
	public boolean addFile(FileSystemTracker tracker, String name, File file) {
		if (registry == null) {
			return false;
		}
		registry.loadContainer(EXTERNAL + name, file);
		return true;
	}

	@Override
	public void updateFile(FileSystemTracker tracker, String name, File file) {
		registry.updateContainer(EXTERNAL + name, file);
	}

	@Override
	public void removeFile(FileSystemTracker tracker, String name, File file) {
		registry.unloadContainer(EXTERNAL + name);
	}

}
