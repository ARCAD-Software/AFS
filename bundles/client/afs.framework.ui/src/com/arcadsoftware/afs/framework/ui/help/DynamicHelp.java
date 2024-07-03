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
package com.arcadsoftware.afs.framework.ui.help;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;

import com.arcadsoftware.afs.framework.services.IDynamicHelpInit;
import com.arcadsoftware.afs.framework.services.IDynamicHelpService;
import com.arcadsoftware.afs.framework.ui.internal.Activator;
import com.arcadsoftware.osgi.ILoggedPlugin;

public final class DynamicHelp {

	public static final String EXTENSION_POINT = "com.arcadsoftware.afs.framework.dynamicHelp"; //$NON-NLS-1$

	public static void init(ILoggedPlugin activator) {
		for (final IConfigurationElement ce : Platform.getExtensionRegistry()
				.getConfigurationElementsFor(EXTENSION_POINT)) {
			try {
				if (Boolean.valueOf(ce.getAttribute("start"))) { //$NON-NLS-1$
					final Object o = ce.createExecutableExtension("class"); //$NON-NLS-1$
					if (o instanceof IDynamicHelpInit) {
						((IDynamicHelpInit) o).init();
					}
				}
			} catch (final Exception e) {
				String cn = "";
				try {
					cn = ce.getContributor().getName();
				} catch (final Exception z) {
				}
				activator.error("Error during IDynamicHelpInit invocation \"" + cn + "\": " + e.getLocalizedMessage(), //$NON-NLS-1$ //$NON-NLS-2$
						e);
			}
		}
	}

	/**
	 * Register Help Id
	 *
	 * @param helpId
	 * @param control
	 */
	public static void registerContextHelpId(String helpId, Object object) {
		if ((helpId != null) && (object != null)) {
			final IDynamicHelpService helpService = Activator.getDefault().getDynamicHelpService();
			if (helpService != null) {
				helpService.register(object, helpId);
			}
		}
	}

	public static void showContextHelpId(String helpId) {
		if (helpId != null) {
			final IDynamicHelpService helpService = Activator.getDefault().getDynamicHelpService();
			if (helpService != null) {
				helpService.showHelp(helpId);
			}
		}
	}

	/**
	 * Connect control to Dynamic Help Id, and refresh help content if already displayed
	 */
	public static void updateContextHelpId(String helpId, Object object) {
		if ((helpId != null) && (object != null)) {
			final IDynamicHelpService helpService = Activator.getDefault().getDynamicHelpService();
			if (helpService != null) {
				helpService.register(object, helpId);
				helpService.showHelp(helpId);
			}
		}
	}
}
