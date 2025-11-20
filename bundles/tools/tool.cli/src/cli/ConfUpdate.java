package cli;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import com.arcadsoftware.cm.simple.Configuration;
import com.arcadsoftware.cm.simple.ConfigurationStoreManager;
import com.arcadsoftware.tool.cli.Command;

public class ConfUpdate extends Command {

	public static void main(String[] args) {
		System.exit(new ConfUpdate(args).exec());
	}

	public ConfUpdate() {
		super();
	}

	public ConfUpdate(String[] args) {
		super(args);
	}

	@Override
	protected String getVersion() {
		return "1.0.0"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandFullName() {
		return "confupdate"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandDescription() {
		return "This command allow to update the current configuration to match the minimal required configuration of the current version of the application.";
	}

	@Override
	protected Map<String, String> getArgumentsDescription() {
		HashMap<String, String> result = new HashMap<String, String>();
		result.put("[-p|-prop <path>]", //$NON-NLS-1$
				"Define the \".ini\" file which contain the required modification of the configuration.");
		result.put("[-b|-backup]", //$NON-NLS-1$
				"Create a copy of the current configuration into an .ini.bak file.");
		result.put("[-c|-consume]", //$NON-NLS-1$
				"Delete the new properties file after integration into the configuration.");
		result.put("[-r|-remove <pid>]", //$NON-NLS-1$
				"Delete the listed PID from the configuration (This parameter may be used repeatedly on tha same call).");
		return result;
	}

	@Override
	protected int run() {
		// Backup of the previous configuration files.
		if (isArgument("-b", "-bup", "-bkup", "-backup")) {//$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ 
			File confFile = getConfigurationRoot();
			if (confFile.isDirectory()) {
				confFile = new File(confFile, "osgi.cm.ini.bak"); //$NON-NLS-1$
			} else {
				confFile = new File(confFile.getParentFile(), confFile.getName() + ".bak"); //$NON-NLS-1$
			}
			println("Create a backup of the current configuration in: " + confFile.getAbsolutePath());
			confFile.getParentFile().mkdirs();
			if (!saveOSGiConfigurationToINI(confFile)) {
				printError("Unable to create a backup file on the configuration.");
				return ERROR_FILESYSTEM_ACCESS;
			}
		}
		// Loading properties file with conversion orders.
		String prop = getArgumentValue(new String[] {"-p", "-prop"}, ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		File pf;
		if ((prop == null) || prop.isEmpty()) {
			pf = new File(getHomeDirectory(), "configuration/update.osgi.cm.ini"); //$NON-NLS-1$
			if (isArgument("-debug")) { //$NON-NLS-1$
				println("Using default property file: " + pf.getAbsolutePath());
			}
		} else {
			pf = new File(prop);
		}
		boolean modif = false;
		if (pf.isFile()) {
			// Apply conversion of the configuration.
			println("Loading new configuration elements...");
			ConfigurationStoreManager newConf = new ConfigurationStoreManager(pf);
			newConf.setUseCFG(false);
			newConf.setUseINI(true);
			newConf.setUseJSON(false);
			try {
				newConf.load();
				if (isArgument("-debug")) { //$NON-NLS-1$
					println("New configuration loaded...");
				}
				for (Configuration conf: newConf.listAllConfigurations()) {
					println("Adding configuration: " + conf.getPid());
					getOSGiConfiguration(conf.getPid()).putAll(conf);
					modif = true;
				}
			} catch (IOException e) {
				printError("Error while loading OSGi configuration: " + e.getLocalizedMessage());
				return ERROR_FILESYSTEM_ACCESS;
			}
		} else if ((prop != null) && !prop.isEmpty()) {
			printError("Unable to found the update properties file: " + pf.getAbsolutePath());
			return ERROR_MISSING_FILE;
		} else if (isArgument("-debug")) { //$NON-NLS-1$
			println("Unable to found the update properties file: " + pf.getAbsolutePath());
		}
		for (String p: getArgumentValues(new String[] {"-r", "-remove"})) {
			removeOSGiConfiguration(p);
			modif = true;
		}
		// Save the new configuration files.
		if (modif) {
			println("Record the updated configuration...");
			if (!saveOSGiConfiguration()) {
				printError("Unable to save the new configuration.");
				return ERROR_INVALID_CONFIGURATION;
			}
		} else {
			println("No modification applied to the current configuration.");
		}
		// Delete the property file.
		if (isArgument("-c", "-consume") && pf.isFile()) { //$NON-NLS-1$ //$NON-NLS-2$
			if (!pf.delete()) {
				printError("Unable to delete the file: " + pf.getAbsolutePath());
				return ERROR_FILESYSTEM_ACCESS;
			}
			println("Update file consumed.");
		}
		println("Configuration correctly updated.");
		return 0;
	}
}
