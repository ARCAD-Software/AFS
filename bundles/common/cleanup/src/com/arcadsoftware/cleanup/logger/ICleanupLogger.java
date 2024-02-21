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
package com.arcadsoftware.cleanup.logger;

public interface ICleanupLogger {
	
	public static final int	LOG_ERROR	= 1;
	public static final int	LOG_WARNING	= 2;
	public static final int	LOG_INFO	= 3;
	public static final int	LOG_DEBUG	= 4;
	
	public void log(int level, String message);
	
	public void logException(String message, Throwable e);
}
