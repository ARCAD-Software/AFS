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
package com.arcadsoftware.cli.model;

import com.arcadsoftware.ae.core.utils.Utils;
import com.arcadsoftware.cli.logger.IMessageLogger;
import com.arcadsoftware.cli.logger.ISimpleLogger;

public class LoggedObject {

	protected ISimpleLogger logger = null;

	public static final String MSG_PROCESS_COUNT = "Nb. processed records :";

	public void setLogger(ISimpleLogger logger) {
		this.logger = logger;
	}

	public ISimpleLogger getLogger() {
		return logger;
	}

	public void log(String message, Throwable e, int logLevel) {
		log(message + "\n" + Utils.stackTrace(e), logLevel); //$NON-NLS-1$
	}

	public void log(Throwable e, int logLevel) {
		log(e.getClass().getName() + ": " + e.getLocalizedMessage() + "\n" + Utils.stackTrace(e), logLevel);//$NON-NLS-1$
	}

	public void log(String message, int logLevel) {
		if (logger != null) {
			switch (logLevel) {
			case IMessageLogger.LOGLVL_FATAL:
				logger.logError(message);
				break;
			case IMessageLogger.LOGLVL_INFO:
				logger.logInfo(message);
				break;
			case IMessageLogger.LOGLVL_WARNING:
				logger.logWarning(message);
				break;
			case IMessageLogger.LOGLVL_VERBOSE:
				logger.logVerbose(message);
				break;
			default:
				break;
			}
		}
	}
}
