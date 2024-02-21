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
package com.arcadsoftware.osgi;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.osgi.framework.console.CommandInterpreter;

public class ConsoleProgressMonitor implements IProgressMonitor {

	private CommandInterpreter ci;
	private boolean cancel;
	
	public ConsoleProgressMonitor(CommandInterpreter ci) {
		super();
		this.ci = ci;
	}
	
	@Override
	public void beginTask(String name, int totalWork) {
		ci.println();
		ci.println(" --- " + name + " ---"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	@Override
	public void done() {
		ci.println();
	}

	@Override
	public void internalWorked(double work) {
	}

	@Override
	public boolean isCanceled() {
		return cancel;
	}

	@Override
	public void setCanceled(boolean value) {
		cancel = value;
	}

	@Override
	public void setTaskName(String name) {
	}

	@Override
	public void subTask(String name) {
		ci.println();
		ci.println("   - " + name + " "); //$NON-NLS-1$ //$NON-NLS-2$
	}

	@Override
	public void worked(int work) {
		ci.print('.');
	}

}
