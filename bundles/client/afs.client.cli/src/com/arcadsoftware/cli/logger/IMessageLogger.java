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
package com.arcadsoftware.cli.logger;

public interface IMessageLogger {
	
	public static final int LOGLVL_VERBOSE = 0;
	public static final int LOGLVL_INFO = 1;
	public static final int LOGLVL_WARNING = 2;
	public static final int LOGLVL_FATAL = 3;

	public void logMessage(String message, int logLevel);
}
