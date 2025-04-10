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
import java.io.FileInputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

import com.arcadsoftware.tool.cli.Command;

public class UpdateConfigIni extends Command {

	public static void main(String[] args) {
		System.exit(new UpdateConfigIni(args).exec());
	}

	private HashSet<String> osgikeys = new HashSet<String>();
	
	{
		osgikeys.add("osgi.framework"); //$NON-NLS-1$
		osgikeys.add("osgi.bundles"); //$NON-NLS-1$
		osgikeys.add("eclipse.p2.data.area"); //$NON-NLS-1$
		osgikeys.add("osgi.install.area"); //$NON-NLS-1$
	}
	
	public UpdateConfigIni() {
		super();
	}

	public UpdateConfigIni(String[] args) {
		super(args);
	}

	@Override
	protected int run() {
		Properties oldConfig = new Properties();
		File oci = getArgumentValue(new String[] {"-configini", "-ci"}, new File(getHomeDirectory(), "configuration/config.bak")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		if (!oci.isFile()) {
			try {
				printWarn("The config.ini backup file is not reachable: " + oci.getCanonicalPath());
			} catch (IOException e) {
				printWarn("The config.ini backup file is not reachable: " + oci.getAbsolutePath());
			}
			return ERROR_INVALID_CONFIGINI;
		}
		try (FileInputStream fis = new FileInputStream(oci)) {
			oldConfig.load(fis);
		} catch (IOException e) {
			printError(e.getLocalizedMessage());
			if (isArgument("-debug")) { //$NON-NLS-1$
				e.printStackTrace();
			}
			return ERROR_INVALID_CONFIGINI;
		}
		Map<String, String> config = getConfigIniProperties();
		int c = 0;
		// The -all option import all properties, except the options related to the distribution (the bundles !).
		if (isArgument("-a", "-all")) { //$NON-NLS-1$ //$NON-NLS-2$
			for (Entry<Object, Object> e: oldConfig.entrySet()) {
				if (!osgikeys.contains(e.getKey()) && //
						(isArgument("-o", "-override") || !config.containsKey(e.getKey()))) { //$NON-NLS-1$ //$NON-NLS-2$
					config.put(e.getKey().toString(), e.getValue().toString());
					if (isArgument("-debug")) { //$NON-NLS-1$
						println("The property \"%s\" has been copied.", e.getKey().toString());
					}
					c++;
				}
			}
		} else {
			for(String key: CRYPTOSYSTEMKEYS) {
				Object value = oldConfig.get(key);
				if ((value != null) && //
						(isArgument("-o", "-override") || !config.containsKey(key))) { //$NON-NLS-1$ //$NON-NLS-2$
					config.put(key, value.toString());
					if (isArgument("-debug")) { //$NON-NLS-1$
						println("The property \"%s\" has been copied.", key);
					}
					c++;
				}
			}
		}
		if (c == 0) {
			println("Zero system properties were injected into the config.ini file.");
			return 0;
		}
		println("%d system properties were injected into the new config.ini file.", c);
		if (saveConfigIni()) {
			println("Config.ini file correctly updated.");
			return 0;
		}
		return ERROR_INTERNAL_ERROR;
	}

	@Override
	protected String getVersion() {
		return "1.0.0"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandFullName() {
		return "updatecfgini"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandDescription() {
		return "This command is used during the application update. It allows to inject the system properties declared into the previous config.ini file into the new one. By default only the critical properties are copied, and only if they do not already exist into the new config.ini file.";
	}

	@Override
	protected Map<String, String> getArgumentsDescription() {
		HashMap<String, String> result = new HashMap<String, String>();
		result.put("[-a|-all]", //$NON-NLS-1$
				"Import all properties from the old config.ini file except the bundles path related ones.");
		result.put("[-o|-override]", //$NON-NLS-1$
				"Override the existing properties in the new config.ini by the old values.");
		result.put("[-ci|-configini] <path>", //$NON-NLS-1$
				"Define the path of the previous config.ini backup file. The default value is \"./configuration/config.bak\".");
		return result;
	}

}
