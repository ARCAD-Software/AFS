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
package com.arcadsoftware.tool.cli;

import com.arcadsoftware.cli.CommandLineException;
import com.arcadsoftware.cli.ICommandLineArguments;
import com.arcadsoftware.cli.ICommandLinePrinter;
import com.arcadsoftware.cli.ICommandLineService;

public class CommandLineService implements ICommandLineService {

	private final Command command;
	
	public CommandLineService(Command command) {
		super();
		this.command = command;
	}

	@Override
	public int run(String commandName, ICommandLineArguments arguments, ICommandLinePrinter printer)
			throws CommandLineException {
		// FIXME Try catch !
		// FIXME move sys.err and sys.out to the printer !
		command.init(arguments.getArguments());
		// FIXME Test if error is > 0
		return command.run();
	}

}
