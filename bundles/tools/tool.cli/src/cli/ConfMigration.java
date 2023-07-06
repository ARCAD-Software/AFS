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
package cli;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import com.arcadsoftware.tool.cli.Command;
import com.arcadsoftware.tool.cli.EquinoxConfigurations;

public class ConfMigration extends Command {

	public static void main(String[] args) {
		new ConfMigration(args).exec();
	}
	
	public ConfMigration() {
		super();
	}
	
	public ConfMigration(String[] args) {
		super(args);
	}

	@Override
	protected String getVersion() {
		return "1.0.0"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandFullName() {
		return "confmigration"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandDescription() {
		return "This command allow to move the configuration files from an Equinox configuration file format to the new OSGi CM simple file formats.";
	}

	@Override
	protected Map<String, String> getArgumentsDescription() {
		HashMap<String, String> result = new HashMap<String, String>();
		result.put("[-cf|-conf <path>]", //$NON-NLS-1$
				"Define the folder which contain the previous configuration, the default value is the same folder as the new configuration.");
		result.put("[-cfg]", //$NON-NLS-1$
				"Force the usage of CFG file to store the migrated configuration.");
		return result;
	}

	@Override
	protected int run() {
		File confFolder = getArgumentValue(new String[] {"-cf", "-conf"}, new File(getHomeDirectory(), "configuration")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		EquinoxConfigurations ec = new EquinoxConfigurations(getConfigurationRoot());
		if (isArgument("-cfg")) { //$NON-NLS-1$
			ec.forceCFG();
		}
		try {
			if (!ec.load(confFolder)) {
				printWarn("No configuration files found.");
			} else {
				try {
					ec.save();
					println("Configuration migrated");
				} catch (IOException e) {
					printError("Unable to record the configuration files: " + e.getLocalizedMessage());
					if (isArgument("-debug")) { //$NON-NLS-1$
						e.printStackTrace();
					}
				}
			}
		} catch (ClassNotFoundException | IOException e) {
			printError("Unable to load the configuration: " + e.getLocalizedMessage());
			if (isArgument("-debug")) { //$NON-NLS-1$
				e.printStackTrace();
			}
		}
		return 0;
	}
}
