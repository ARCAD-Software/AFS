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
package com.arcadsoftware.server.messages;

/**
 * 
 */
public class SimpleLoggerByCode implements ILoggerByCode {

	protected final int LEVEL_VERBOSE = 0;
	protected final int LEVEL_INFO = 1;
	protected final int LEVEL_WARNING = 2;
	protected final int LEVEL_ERROR = 3;
	
	@Override
	public void logVerbose(String code, Object... vars) {
		log(LEVEL_VERBOSE, code, vars);
	}

	@Override
	public void logError(String code, Object... vars) {
		log(LEVEL_ERROR, code, vars);
	}
	@Override
	public void logWarning(String code, Object... vars) {
		log(LEVEL_WARNING, code, vars);
	}

	@Override
	public void logInfo(String code, Object... vars) {
		log(LEVEL_INFO, code, vars);
	}

	/**
	 * 
	 */
	protected void log(int level, String code, Object...vars) {
		UserMessage message = MessageManager.getMessage(code, vars);
		String stringMessage = String.format("[Level %1$s] %2$s\n%3$s", //$NON-NLS-1$
				level, message.getTextLevel1(), message.getTextLevel2());
		System.out.println(stringMessage);
		additonnalLog(level, message);
	}
	
	/**
	 * 
	 */
	protected void additonnalLog(int level, UserMessage message) {}
}
