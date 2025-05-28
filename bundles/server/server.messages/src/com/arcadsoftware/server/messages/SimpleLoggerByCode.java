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

import com.arcadsoftware.server.messages.internal.Activator;

/**
 * A simple implementation of the {@link ILoggerByCode} interface that logs messages
 * to the standard output based on message codes and severity levels.
 * <p>
 * This logger retrieves messages from the {@link MessageManager} using a message code
 * and optional parameters, and formats them with a severity prefix. By default, messages
 * are printed to {@code System.out}, but the behavior can be extended by overriding
 * the {@link #additonnalLog(int, UserMessage)} method.
 */
public class SimpleLoggerByCode implements ILoggerByCode {

	protected final int LEVEL_VERBOSE = 0;
	protected final int LEVEL_INFO = 1;
	protected final int LEVEL_WARNING = 2;
	protected final int LEVEL_ERROR = 3;

	/**
	 * Logs a verbose message identified by its code and optional parameters.
	 *
	 * @param code the message code
	 * @param vars optional parameters for message formatting
	 */
	@Override
	public void logVerbose(String code, Object... vars) {
		log(LEVEL_VERBOSE, code, vars);
	}

	/**
	 * Logs an error message identified by its code and optional parameters.
	 *
	 * @param code the message code
	 * @param vars optional parameters for message formatting
	 */
	@Override
	public void logError(String code, Object... vars) {
		log(LEVEL_ERROR, code, vars);
	}

	/**
	 * Logs a warning message identified by its code and optional parameters.
	 *
	 * @param code the message code
	 * @param vars optional parameters for message formatting
	 */
	@Override
	public void logWarning(String code, Object... vars) {
		log(LEVEL_WARNING, code, vars);
	}

	/**
	 * Logs an informational message identified by its code and optional parameters.
	 *
	 * @param code the message code
	 * @param vars optional parameters for message formatting
	 */
	@Override
	public void logInfo(String code, Object... vars) {
		log(LEVEL_INFO, code, vars);
	}

	/**
	 * Core logging method that retrieves and formats the message before printing it.
	 * <p>
	 * The message is retrieved via {@link MessageManager}, formatted with level and text,
	 * printed to {@code System.out}, and passed to {@link #additonnalLog(int, UserMessage)}.
	 *
	 * @param level the severity level of the message
	 * @param code the message code
	 * @param vars optional parameters for message formatting
	 */
	protected void log(int level, String code, Object... vars) {
		UserMessage message = MessageManager.getMessage(code, vars);
		Activator.logInfo(String.format("[Level %1$s] %2$s\n%3$s", //$NON-NLS-1$
				level, message.getTextLevel1(), message.getTextLevel2()), null);
		additonnalLog(level, message);
	}

	/**
	 * Hook method for subclasses to implement additional logging behavior.
	 * <p>
	 * This method is called after the message is printed. Subclasses can override it
	 * to log messages to files, databases, or monitoring systems.
	 *
	 * @param level the severity level of the message
	 * @param message the message to log
	 */
	protected void additonnalLog(int level, UserMessage message) {}
}
