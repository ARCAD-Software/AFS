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
package com.arcadsoftware.server.web.internal;

import java.util.Dictionary;

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;

import com.arcadsoftware.osgi.AbstractConfiguredActivator;
import com.arcadsoftware.rest.IBranch;

public class Activator extends AbstractConfiguredActivator {

	private static final String PROP_SECURED = "secure"; //$NON-NLS-1$
	private static final String PROP_WEBROOT = "webroot"; //$NON-NLS-1$
	private static final String PROP_DISABLED = "disabled"; //$NON-NLS-1$
	
	private ServiceRegistration<IBranch> branchweb;
	
	@Override
	public void start(BundleContext bundleContext) throws Exception {
		super.start(bundleContext);
		branchweb = registerService(IBranch.class, new WebBranch(this, null), IBranch.URI, IBranch.ROOTBRANCH);
	}

	@Override
	public void updatedConfiguration(Dictionary<String, Object> properties) {
		if (properties != null) {
			if (branchweb != null) {
				unregister(branchweb);
			}
			if (!parseBooleanParameter(properties.get(PROP_DISABLED))) {
				String webroot = parseStringParameter(properties.get(PROP_WEBROOT), null);
				if (parseBooleanParameter(properties.get(PROP_SECURED))) {
					branchweb = registerService(IBranch.class, new WebBranch(this, webroot), IBranch.URI, IBranch.SECUREDBRANCH);
				} else {
					branchweb = registerService(IBranch.class, new WebBranch(this, webroot), IBranch.URI, IBranch.ROOTBRANCH);
				}
			}
		}
	}
}
