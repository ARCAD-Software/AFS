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
package com.arcadsoftware.osgi;

/**
 * @deprecated Extends one of the existing implementation like SysOutLogged. 
 */
public abstract class LoggedPluginAdapter implements ILoggedPlugin {
	
	@Override
	public void log(String message) {
		this.log(message, null);
	}

	@Override
	public void log(Throwable e) {
		this.log(e.getLocalizedMessage(), e);
	}	
	
	@Override
	public void warn(String message) {
		this.warn(message, (Throwable) null);
	}

	@Override
	public void debug(String message) {
		this.debug(message, (Throwable) null);
	}

	@Override
	public void debug(Throwable e) {
		this.debug(e.getLocalizedMessage(), e);
	}	
}
