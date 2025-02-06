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
package com.arcadsoftware.cli;

import com.arcadsoftware.osgi.ILoggedPlugin;

/**
 * Interface between the command execution and the output console.
 * 
 * @author ARCAD Software
 */
public interface ICommandLinePrinter extends ILoggedPlugin {
	
	/**
	 * Print using String.format method...
	 * 
	 * @param format
	 * @param text
	 */
	public void println(String format, Object... text);
	
	/**
	 * Print the String representation of the given object and add new line feed..
	 * 
	 * @param text
	 */
	public void println(Object text);
	
	/**
	 * Print using String.format method...
	 * 
	 * @param indentation a positive integer of white spaces tobe added before the given text.
	 * @param text 
	 */
	public void println(int indentation, String text);
	
	/**
	 * Print the String representation of the given object.
	 * 
	 * @param text
	 */
	public void print(Object text);
	
	/**
	 * Print a blank line.
	 * 
	 * @param text
	 */
	public void println();
	
	/**
	 * Wait any user input (a key stroke).
	 * 
	 * @return
	 */
	public int waitUserInput();
	
	/**
	 * Read a text line (until [ENTER]) from the user input.
	 * 
	 * @param text
	 * @return
	 */
	public String read(Object text);
	
	/**
	 * Read a secret text line (until [ENTER]) from the user input.
	 * 
	 * @param text
	 * @return
	 */
	public char[] readSecret(Object text);

	/**
	 * Return true is the framework debug option is activated.
	 * 
	 * @return
	 */
	public boolean isDebug();
	
	/**
	 * Timing logging running from the given start date.
	 * 
	 * @param message
	 * @param t starting time (= System.currentTimeMillis())
	 * @see AbstractLoggerFacade#TIMINGTRACE
	 */
	public void logTiming(String message, long t);
	
	/**
	 * Log a detailed debug message. Only if the system property com.arcadsoftware.trace is true.
	 * 
	 * <p>
	 * For performance reasons the parameter are not not necessarily converted into string. This occurs only 
	 * if the message is effectively sent to the log, if the TRACE property is set to <b>true</b>.
	 * 
	 * @param message The message will be assembled from the given object list, only and only if the TRACE is activated.
	 * @see AbstractLoggerFacade#TIMINGTRACE
	 */
	public void trace(Object... message);

	/**
	 * Log a stack trace only if the "trace" system property is set.
	 * 
	 * @param e
	 */
	public void trace(Throwable e);
}
