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
import java.net.URL;
import java.util.Dictionary;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleEvent;
import org.osgi.framework.BundleListener;

import com.arcadsoftware.metadata.registry.XmlRegistry;

public class RegistryBundleListener implements BundleListener {

	// FIXME the directive avoiding the process of the following bundle is in fact "DynamicImport-Package: *" a more acurate filter should test the presence of this directive in the Manifest of the bundle instead of its symbolic name !
	private static final String GROOVY = "groovy"; //$NON-NLS-1$
	private static final String ORG_CODEHAUS_GROOVY = "org.codehaus.groovy"; //$NON-NLS-1$
	private static final String JAKARTAACTIVATION = "com.sun.activation.jakarta.activation"; //$NON-NLS-1$
	private static final String DEFAULTENTITYFILENAME = "/META-INF/entities.xml"; //$NON-NLS-1$
	private static final String AUXILIARY_DEFAULTENTITYFILENAME = "/META-INF/Entities.xml"; //$NON-NLS-1$
	private static final String ENITIESHEADER = "Arcad-Entities"; //$NON-NLS-1$
	private static final String EQUINOX_COMMON = "org.eclipse.equinox.common"; //$NON-NLS-1$
	
	private Activator activator;
	private XmlRegistry registry;
	
	public RegistryBundleListener(Activator activator, XmlRegistry registry) {
		super();
		this.activator = activator;
		this.registry = registry;
		if (isCommonStarted()) {
			proceedDelayedBundleScan();
		}
	}

	private void proceedDelayedBundleScan() {
		if (activator.getContext() != null) {
			for (Bundle bundle: activator.getContext().getBundles()) {
				if ((bundle.getState() == Bundle.ACTIVE) && // 
						(!JAKARTAACTIVATION.equals(bundle.getSymbolicName())) && //
						(!ORG_CODEHAUS_GROOVY.equals(bundle.getSymbolicName())) && //
						(!GROOVY.equals(bundle.getSymbolicName()))) {
					addBundle(bundle);
				}
			}
		}
	}

	private boolean isCommonStarted() {
		if (activator != null) {
			BundleContext cx = activator.getContext();
			if (cx != null) {
				Bundle[] bls = cx.getBundles();
				if (bls != null) {
					for (Bundle b: bls) {
						if ((b != null) && EQUINOX_COMMON.equals(b.getSymbolicName()) && (b.getState() == Bundle.ACTIVE)) {
							return true;
						}
					}
				}
			}
		}
		return false;
	}

	@Override
	public void bundleChanged(BundleEvent event) {
		Bundle bundle = event.getBundle();
		switch (event.getType()) {
		case BundleEvent.STARTED:
			// Suppression du bundle de Groovy qui a accès aux resources des autres bundles...
			if ((!ORG_CODEHAUS_GROOVY.equals(bundle.getSymbolicName())) && //
					(!JAKARTAACTIVATION.equals(bundle.getSymbolicName())) && //
					(!GROOVY.equals(bundle.getSymbolicName()))) {
				if (EQUINOX_COMMON.equals(event.getBundle().getSymbolicName())) {
					//Thread.sleep(100);
					proceedDelayedBundleScan();
				} else if (isCommonStarted()) {
					addBundle(bundle);
				}
			}
			break;
		case BundleEvent.STOPPED:
			if (isCommonStarted()) {
				removeBundle(bundle);
			}
			break;
		}
	}

	public void addBundle(Bundle bundle) {
		File file = getEntitiesFile(bundle);
		if ((file != null) && file.isFile()) {
			registry.loadContainer(Activator.INTERNAL + bundle.getSymbolicName() + ':' + bundle.getVersion().toString(), file);
		}
	}

	public void removeBundle(Bundle bundle) {
		File file;
		file = getEntitiesFile(bundle);
		if ((file != null) && file.isFile()) {
			registry.unloadContainer(Activator.INTERNAL + bundle.getSymbolicName() + ':' + bundle.getVersion().toString());
		}
	}

	protected File getEntitiesFile(Bundle bundle) {
		String filename = DEFAULTENTITYFILENAME;
		Dictionary<?, ?> headers = bundle.getHeaders();
		if (headers != null) {
			Object o = headers.get(ENITIESHEADER);
			if ((o != null) && (o.toString().length() > 0)) {
				filename = o.toString();
			}
		}
		// Do not use bundle classpath.
		URL url = bundle.getEntry(filename);
		if (url == null) {
			// Use bundle classpath.
			url = bundle.getResource(filename);
			if (url == null) {
				if (filename == DEFAULTENTITYFILENAME) {
					url = bundle.getEntry(AUXILIARY_DEFAULTENTITYFILENAME);
					if (url == null) {
						// Use bundle classpath.
						url = bundle.getResource(AUXILIARY_DEFAULTENTITYFILENAME);
						if (url == null) {
							return null;
						}
					}
				}
			}
		}
		return activator.toFile(url);
	}
}
