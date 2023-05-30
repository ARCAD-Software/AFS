/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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
package com.arcadsoftware.cleanup.logger;

public class SystemOutLogger implements ICleanupLogger {

	@Override
	public void log(int level, String message) {
		if (level == LOG_ERROR) {
			System.out.println(message);
		} else {
			System.err.println(message);
		}
	}

	@Override
	public void logException(String message, Throwable e) {
		log(LOG_ERROR, message);
	}
}
