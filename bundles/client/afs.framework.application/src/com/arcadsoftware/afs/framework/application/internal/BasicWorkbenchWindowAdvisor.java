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
package com.arcadsoftware.afs.framework.application.internal;

import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;

import com.arcadsoftware.afs.framework.application.IConfigurationManager;

public class BasicWorkbenchWindowAdvisor extends WorkbenchWindowAdvisor {

	private IConfigurationManager manager;

	public BasicWorkbenchWindowAdvisor(IWorkbenchWindowConfigurer configurer) {
		super(configurer);
	}

	private IConfigurationManager getManager() {
		if (manager == null) {
			manager = Activator.getInstance().getConfigurationManager();
		}
		return manager;
	}

	@Override
	public void preWindowOpen() {
		if (getManager() != null) {
			final IWorkbenchWindowConfigurer configurer = getWindowConfigurer();
			// Manage visibility of the different elements
			configurer.setInitialSize(getManager().getInitialSize());
			final int ss = getManager().getShellStyle();
			if (ss != 0) {
				configurer.setShellStyle(ss);
			}
			configurer.setShowCoolBar(getManager().isCoolBarVisible());
			configurer.setShowProgressIndicator(getManager().isProgressBarVisible());
			configurer.setShowStatusLine(getManager().isStatusLineVisible());
			// Perspective Bar Configuration
			configurer.setShowPerspectiveBar(getManager().isPerspectiveBarVisible());
			getManager().setPerspectiveBarLocation();
			getManager().setPerspectiveBarStyle();
		}
	}

	@Override
	public void postWindowCreate() {
		if (getManager() != null) {
			getManager().adaptMainShell(getWindowConfigurer().getWindow().getShell());
		}
	}

	@Override
	public ActionBarAdvisor createActionBarAdvisor(IActionBarConfigurer configurer) {
		return new BasicActionBarAdvisor(configurer);
	}

}
