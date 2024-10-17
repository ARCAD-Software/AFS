package cli;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

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
				"Define the properties file which contain the required modification of the configuration.");
		result.put("[-c|-consume]", //$NON-NLS-1$
				"Delete the properties file after integration into the configuration.");
		return result;
	}

	@Override
	protected int run() {
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
		if (!pf.isFile()) {
			printError("Unable to found the update properties file: " + pf.getAbsolutePath());
			return ERROR_MISSING_FILE;
		}
		// Apply conversion of the configuration.
		
		// Backup of the previous configuration files.
		
		// Save the new configuration files.
		
		// Delete the property file.
		if (isArgument("-c", "-consume") && !pf.delete()) { //$NON-NLS-1$ //$NON-NLS-2$
			printError("Unable to delete the file: " + pf.getAbsolutePath());
		}
		return 0;
	}
}
