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
package com.arcadsoftware.cli.logger;

public class ServiceLogger implements ISimpleLogger {

	private final LogEntries logEntries;

	public ServiceLogger() {
		logEntries = new LogEntries();
	}

	public LogEntries getLogEntries() {
		return logEntries;
	}

	@Override
	public void logError(String message) {
		logEntries.add(new LogEntry(IMessageLogger.LOGLVL_FATAL, message));
	}

	@Override
	public void logInfo(String message) {
		logEntries.add(new LogEntry(IMessageLogger.LOGLVL_INFO, message));

	}

	@Override
	public void logVerbose(String message) {
		logEntries.add(new LogEntry(IMessageLogger.LOGLVL_VERBOSE, message));

	}

	@Override
	public void logWarning(String message) {
		logEntries.add(new LogEntry(IMessageLogger.LOGLVL_WARNING, message));

	}

}
