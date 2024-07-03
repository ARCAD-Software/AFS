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

import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;

public class Application implements IApplication {

	@Override
	public Object start(IApplicationContext context) throws Exception {
		Object result = null;
		final Display display = PlatformUI.createDisplay();
		try {
			int returnCode;
			try {
				final BasicWorkbenchAdvisor ad = new BasicWorkbenchAdvisor();
				returnCode = PlatformUI.createAndRunWorkbench(display, ad);
				if (returnCode == PlatformUI.RETURN_RESTART) {
					result = IApplication.EXIT_RESTART;
				} else {
					result = IApplication.EXIT_OK;
				}
			} catch (final Exception e) {
				Activator.getInstance().log(e);
			}

		} finally {
			display.dispose();
		}
		return result;
	}

	@Override
	public void stop() {
		final IWorkbench workbench = PlatformUI.getWorkbench();
		if (workbench == null) {
			return;
		}
		final Display display = workbench.getDisplay();
		display.syncExec(() -> {
			if (!display.isDisposed()) {
				workbench.close();
			}
		});
	}

}
