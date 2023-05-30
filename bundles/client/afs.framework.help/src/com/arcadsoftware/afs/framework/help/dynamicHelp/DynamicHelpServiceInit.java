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
/**
 * This class is used to as an extension point to force start of the plugin.
 */
package com.arcadsoftware.afs.framework.help.dynamicHelp;

import com.arcadsoftware.afs.framework.services.IDynamicHelpInit;

public class DynamicHelpServiceInit implements IDynamicHelpInit {

	@Override
	public void init() {
		// Nothing to do. This is used to force Bundle to be started. Activator takes in charge loading Service
	}
}
