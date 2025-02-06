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

import java.util.Date;

public class LogEntry {

	private int level = IMessageLogger.LOGLVL_INFO;
	private Date logDate;
	private String message = ""; //$NON-NLS-1$

	public LogEntry() {

	}

	public LogEntry(int level, String message) {
		this.level = level;
		this.message = message;
		logDate = new Date();
	}

	public int getLevel() {
		return level;
	}

	public String getMessage() {
		return message;
	}

	public String print() {
		return level + " : " + message; //$NON-NLS-1$
	}

	public Date getLogDate() {
		return logDate;
	}

}
