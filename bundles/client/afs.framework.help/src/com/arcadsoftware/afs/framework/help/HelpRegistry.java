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
package com.arcadsoftware.afs.framework.help;

import java.util.HashMap;

import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.PlatformUI;
import org.osgi.framework.Bundle;

/**
 * Context Registry
 *
 * @author ARCAD Software
 */
public class HelpRegistry {

	protected static HashMap<String, String> helpContextRegistry;

	private static String getContextId(Bundle bundle, String localKey) {
		return helpContextRegistry.get(bundle.getSymbolicName() + '.' + localKey);
	}

	/**
	 * Register a contextId for a particular Bundle.<br>
	 * This contextId can be retrieve using a key
	 *
	 * @param bundle
	 *            Owner of the contextId
	 * @param localKey
	 *            access key
	 * @param localContextId
	 *            Context Identifier
	 */
	public static void registerContext(Bundle bundle, String localKey, String localContextId) {
		if (getContextId(bundle, localKey) == null) {
			helpContextRegistry.put(bundle.getSymbolicName() + '.' + localKey,
					bundle.getSymbolicName() + '.' + localContextId);
		}
	}

	protected void setHelp(Control c, String contextId) {
		PlatformUI.getWorkbench().getHelpSystem().setHelp(c, contextId);
	}

	protected void setHelp(Control c, Bundle bundle, String localKey) {
		final String contextId = helpContextRegistry.get(bundle.getSymbolicName() + '.' + localKey);
		if (contextId != null) {
			PlatformUI.getWorkbench().getHelpSystem().setHelp(c, contextId);
		}
	}

}
