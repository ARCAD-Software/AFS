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

import org.eclipse.ui.application.IWorkbenchConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;

import com.arcadsoftware.afs.framework.application.IConfigurationManager;

public class BasicWorkbenchAdvisor extends WorkbenchAdvisor {

	private IConfigurationManager manager;

	private IConfigurationManager getManager() {
		if (manager == null) {
			manager = Activator.getInstance().getConfigurationManager();
		}
		return manager;
	}

	@Override
	public String getInitialWindowPerspectiveId() {
		if (getManager() != null) {
			return getManager().getDefaultPerspectiveId();
		}
		return null;
	}

	@Override
	public WorkbenchWindowAdvisor createWorkbenchWindowAdvisor(IWorkbenchWindowConfigurer configurer) {
		return new BasicWorkbenchWindowAdvisor(configurer);
	}

	@Override
	public void initialize(IWorkbenchConfigurer configurer) {
		super.initialize(configurer);
		configurer.setSaveAndRestore(true);
		if (getManager() != null) {
			getManager().doAfterInitialization(configurer);
		}
	}

	@Override
	public void preStartup() {
		super.preStartup();
		if (getManager() != null) {
			getManager().preStartup();
		}
	}

	@Override
	public void postStartup() {
		super.postStartup();
		if (getManager() != null) {
			getManager().postStartup();
		}
	}

	@Override
	public void postShutdown() {
		super.postShutdown();
		if (getManager() != null) {
			getManager().postShutdown();
		}
	}

	@Override
	public boolean preShutdown() {
		boolean result = super.preShutdown();
		if (result && (getManager() != null)) {
			return getManager().preShutdown();
		}
		return result;
	}
}
