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
package com.arcadsoftware.server.messages;

/**
 * 
 */
public class UserMessageLoggedObject {
	
	protected volatile IUserMessageLogger logger;
	
	/**
	 * Define the associated logger.
	 *  
	 * @param logger
	 */
	public void setLogger(IUserMessageLogger logger) {
		this.logger = logger;
	}
	
	/**
	 * Get the current associated logger.
	 * 
	 * @return
	 */
	public IUserMessageLogger getLogger() {
		return logger;
	}
	
	/**
	 * Log a message.
	 * 
	 * @param message
	 * @param logLevel
	 */
	public void log(UserMessage message, int logLevel) {
		if (logger != null) {
			switch (logLevel) {
			case IMSGConstants.MESSAGE_LEVEL_ERROR:	
				logger.logError(message);
				break;
			case IMSGConstants.MESSAGE_LEVEL_INFO:				
				logger.logInfo(message);
				break;		
			case IMSGConstants.MESSAGE_LEVEL_WARNING:				
				logger.logWarning(message);
				break;
			case IMSGConstants.MESSAGE_LEVEL_VERBOSE:
			default:
				logger.logVerbose(message);
				break;				
			}
		}
	}
	
	/**
	 * Hide password data in log.
	 * 
	 * <p>
	 * Default implementation does nothing.
	 * 
	 * @param log
	 */
	public StringBuilder hidePassword(StringBuilder log) {
		return log;
	}
}
