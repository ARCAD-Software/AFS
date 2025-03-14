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
 * Empty and final implementation of the ILoggedPlugin facade.
 * 
 * <p>
 * Usage of this interface should equivalent to pass a <b>null</b> logged, but avoid NPE !
 * 
 * @author ARCAD Software
 */
public final class LoggedToVoid implements ILoggedPlugin {

	public LoggedToVoid() {}

	@Override
	public void log(String message) {}

	@Override
	public void log(String message, Throwable e) {}

	@Override
	public void log(Throwable e) {}

	@Override
	public void error(String message, Throwable e) {}

	@Override
	public void warn(String message) {}

	@Override
	public void warn(String message, Throwable e) {}

	@Override
	public void debug(String message) {}

	@Override
	public void debug(String message, Throwable e) {}

	@Override
	public void debug(Throwable e) {}

	@Override
	public void info(String message) {}

	@Override
	public void info(String message, Throwable e) {}

	@Override
	public void info(Throwable e) {}

	@Override
	public void info(String message, Object... objects) {}

	@Override
	public void error(String message) {}

	@Override
	public void error(Throwable e) {}

	@Override
	public void error(String message, Object... objects) {}

	@Override
	public void warn(Throwable e) {}

	@Override
	public void warn(String message, Object... objects) {}

	@Override
	public void debug(String message, Object... objects) {}

	@Override
	public void trace(String message) {}

	@Override
	public void trace(String message, Throwable e) {}

	@Override
	public void trace(Throwable e) {}

	@Override
	public void trace(String message, Object... objects) {}

	@Override
	public void audit(String message, Throwable e) {}

	@Override
	public void audit(String message) {}

	@Override
	public void audit(Throwable e) {}

	@Override
	public void audit(String message, Object... objects) {}

}
