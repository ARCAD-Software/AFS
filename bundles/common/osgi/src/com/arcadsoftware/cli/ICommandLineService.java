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

/**
 * This OSGi service is called from the command line when the frameworks is started. 
 * 
 * @author ARCAD Software
 */
public interface ICommandLineService {

	@Deprecated
	public static final String clazz = ICommandLineService.class.getName();
	
	/**
	 * OSGi Service property which identify this command, this keyword is used as an argument is the CLI.
	 */
	public static final String COMMANDNAME = "cli-command-name"; //$NON-NLS-1$
	
	/**
	 * Optional OSGi Service property, this text message is used to document the commands list.
	 * 
	 * It is not recommended to used a multiline comment, But if you does it will be indented.
	 */
	public static final String COMMANDSHORTHELPMESSAGE = "cli-command-message"; //$NON-NLS-1$
	
	/**
	 * 
	 * @param commandName
	 * @param arguments 
	 * @param printer
	 * @return
	 * @throws CommandLineException
	 */
	public int run(String commandName, ICommandLineArguments arguments, ICommandLinePrinter printer) throws CommandLineException;
}
