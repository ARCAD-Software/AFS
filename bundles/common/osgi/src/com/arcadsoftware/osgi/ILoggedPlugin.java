package com.arcadsoftware.osgi;

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
public interface ILoggedPlugin {

	/**
	 * System property used to activate a TRACE level in OSGi bundles 
	 * (this trace level can be used to add debug massages in the Log).
	 */
	public static final boolean TRACE = "true".equalsIgnoreCase(System.getProperty("com.arcadsoftware.trace")); //$NON-NLS-1$

	/**
	 * Log an simple information message.
	 *  
	 * @param message to be logged
	 */
	public void log(String message);
	
	/**
	 * Log an error message.
	 * 
	 * @param message the error specific message
	 * @param e the exception associated to this error. Can be null.
	 */
	public void log(String message, Throwable e);
	
	/**
	 * Log an error without any message.
	 * 
	 * <p>You should consider to use the <code>log(message,e)</code> version of this methode
	 * to inform the user about the nature of this error.
	 * 
	 * @param e the exception that caused the error.
	 */
	public void log(Throwable e);
	
	/**
	 * Log an error message.
	 * 
	 * @param message the error specific message
	 * @param e the exception associated to this error. Can be null.
	 */
	public void error(String message, Throwable e);
	
	/**
	 * Log a warning message.
	 * 
	 * @param message
	 */
	public void warn(String message);
	
	/**
	 * Log a warning error.
	 * 
	 * <p>Warning errors are not critical error that do not require any user action and do
	 * not stop the current process.
	 * 
	 * @param message
	 * @param e the exception
	 */
	public void warn(String message,Throwable e);
	
	/**
	 * If the plugin is in debug mode then log the message.
	 * 
	 * @param message
	 */
	public void debug(String message);
	
	/**
	 * If the plugin is in debug mode then log the exception
	 * 
	 * @param message
	 * @param e
	 */
	public void debug(String message, Throwable e);

	/**
	 * If the plugin is in debug mode then log the exception
	 * 
	 * @param e
	 */
	public void debug(Throwable e);
}
