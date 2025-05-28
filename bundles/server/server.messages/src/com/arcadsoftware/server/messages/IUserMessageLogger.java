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
 * Interface for logging user-facing messages at various severity levels.
 * <p>
 * This interface defines methods to log messages encapsulated in {@link UserMessage} objects,
 * allowing structured logging with predefined formatting, internationalization, or message codes.
 */
public interface IUserMessageLogger {
	
	/**
	 * Logs a message with error severity.
	 * <p>
	 * Error messages indicate a failure or critical issue that requires attention.
	 *
	 * @param um the {@link UserMessage} to log
	 */
	public void logError(UserMessage um);
	
	/**
	 * Logs a message with informational severity.
	 * <p>
	 * Info messages are used to report normal application progress or events.
	 *
	 * @param um the {@link UserMessage} to log
	 */
	public void logInfo(UserMessage um);
	
	/**
	 * Logs a message with warning severity.
	 * <p>
	 * Warning messages indicate potential issues or non-critical problems.
	 *
	 * @param um the {@link UserMessage} to log
	 */
	public void logWarning(UserMessage um);
	
	/**
	 * Logs a message with verbose severity.
	 * <p>
	 * Verbose messages are typically used for detailed debugging or diagnostic information.
	 *
	 * @param um the {@link UserMessage} to log
	 */
	public void logVerbose(UserMessage um);
}
