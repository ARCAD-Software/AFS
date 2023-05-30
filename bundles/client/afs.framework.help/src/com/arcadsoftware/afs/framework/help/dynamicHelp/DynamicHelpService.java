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
package com.arcadsoftware.afs.framework.help.dynamicHelp;

import org.eclipse.jface.action.IAction;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.PlatformUI;

import com.arcadsoftware.afs.framework.help.internal.Activator;
import com.arcadsoftware.afs.framework.services.IDynamicHelpService;

public class DynamicHelpService implements IDynamicHelpService {
	
	private final Activator activator;
	
	public DynamicHelpService(Activator activator) {
		super();
		this.activator = activator;
	}
	
	@Override
	public void register(Object object, String helpId) {
		if (helpId != null) {
			try {
				if (object instanceof Control) {
					PlatformUI.getWorkbench().getHelpSystem().setHelp((Control)object, helpId);
				} else if (object instanceof IAction) {
					PlatformUI.getWorkbench().getHelpSystem().setHelp((IAction)object, helpId);
				} else if (object instanceof Menu) {
					PlatformUI.getWorkbench().getHelpSystem().setHelp((Menu)object, helpId);
				} else if (object instanceof MenuItem) {
					PlatformUI.getWorkbench().getHelpSystem().setHelp((MenuItem)object, helpId);
				} else if (object != null) {
					activator.error(object.getClass().getName() + " objects cannot be registered to Dynamic Help");
				} else {
					activator.error("Cannot show help for ID " + helpId);
				}
			} catch (Exception e) {
				activator.error("Cannot show help for ID " + helpId, e);
			}
		}
	}

	@Override
	public void showHelp(String helpId) {
		try {
			if (PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findViewReference("org.eclipse.help.ui.HelpView") != null) { //$NON-NLS-1$
				PlatformUI.getWorkbench().getHelpSystem().displayHelp(helpId);
			}
		} catch (Exception e) {
			activator.error("Could not show help for ID " + helpId, e);
		}		
	}
}
