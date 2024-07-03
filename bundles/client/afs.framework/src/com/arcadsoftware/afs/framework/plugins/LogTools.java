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
package com.arcadsoftware.afs.framework.plugins;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.osgi.framework.Bundle;

public class LogTools {

	public enum BundleProperty {
		NAME("Bundle-Name", "Plug-in Name"), //$NON-NLS-1$ //$NON-NLS-2$
		VENDOR("Bundle-Vendor", "Plug-in Vendor"), //$NON-NLS-1$ //$NON-NLS-2$
		VERSION("Bundle-Version", "Version"), //$NON-NLS-1$ //$NON-NLS-2$
		ID("Bundle-SymbolicName", "Plug-in ID"); //$NON-NLS-1$ //$NON-NLS-2$

		private final String key;
		private final String label;

		BundleProperty(String key, String label) {
			this.key = key;
			this.label = label;
		}

		public String getKey() {
			return key;
		}

		public String getLabel() {
			return label;
		}
	}

	protected static BundleProperty[] BUNDLE_PROPERTIES = new BundleProperty[] { BundleProperty.ID, BundleProperty.NAME,
			BundleProperty.VENDOR, BundleProperty.VERSION };

	public static String getBundleSummaryInformation(Bundle bundle, BundleProperty[] keys, boolean showLabel) {
		final StringBuilder info = new StringBuilder();
		for (final BundleProperty bp : keys) {
			final Object o = bundle.getHeaders().get(bp.key);
			if (o != null) {
				final String value = o.toString();
				if (!info.toString().equals("")) { // $NON-NLS-1$
					info.append(" - ");//$NON-NLS-1$
				}
				if (showLabel) {
					info.append(bp.label + ": " + value);//$NON-NLS-1$
				} else {
					info.append(value);
				}
			}
		}
		return info.toString();
	}

	/**
	 * Method createStatus is helper method that creates a Status object.
	 */
	public static Status createStatus(Bundle bundle, int severity, String msg) {
		final Status status = new Status(severity, bundle
				.getSymbolicName(), IStatus.OK, msg, null);
		return status;

	}

}
