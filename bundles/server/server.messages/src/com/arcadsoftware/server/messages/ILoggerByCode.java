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
package com.arcadsoftware.server.messages;

/**
 * Interface for logging messages identified by a code and optional parameters.
 * <p>
 * This abstraction allows logging with different severity levels (verbose, error, warning, info),
 * where each message is identified by a unique code. This can be used in conjunction with a
 * message catalog for internationalization or centralized message formatting.
 * 
 * @author ARCAD software
 */
public interface ILoggerByCode {
	
	/**
	 * Logs a verbose-level message identified by a code with optional parameters.
	 * <p>
	 * Verbose logs are typically used for debugging or tracing information that is not needed in production.
	 *
	 * @param code the unique identifier for the message
	 * @param vars optional parameters to be substituted into the message
	 */
	public void logVerbose(String code, Object... vars);
	
	/**
	 * Logs an error-level message identified by a code with optional parameters.
	 * <p>
	 * Error logs indicate a failure that should be investigated and possibly trigger alerts.
	 *
	 * @param code the unique identifier for the message
	 * @param vars optional parameters to be substituted into the message
	 */
	public void logError(String code, Object... vars);
	
	/**
	 * Logs a warning-level message identified by a code with optional parameters.
	 * <p>
	 * Warnings indicate potentially harmful situations or unexpected behavior.
	 *
	 * @param code the unique identifier for the message
	 * @param vars optional parameters to be substituted into the message
	 */
	public void logWarning(String code, Object... vars);
	
	/**
	 * Logs an informational message identified by a code with optional parameters.
	 * <p>
	 * Info logs provide general information about application progress or state.
	 *
	 * @param code the unique identifier for the message
	 * @param vars optional parameters to be substituted into the message
	 */
	public void logInfo(String code, Object... vars);	
}