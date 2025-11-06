package cli;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.Map;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.crypt.RandomGenerator;
import com.arcadsoftware.tool.cli.Command;

public class MasterKeyToPath extends Command {

	public static void main(String[] args) {
		System.exit(new MasterKeyToPath(args).exec());
	}

	public MasterKeyToPath(String[] args) {
		super(args);
	}

	@Override
	protected int run() {
		println("Loading the Master key definition and initiliazing the target file.");
		Map<String, String> config = getConfigIniProperties();
		String pathInConfig = config.get("com.arcadsoftware.masterkey.path"); //$NON-NLS-1$
		String path = getArgumentValue(new String[] {"-target", "-t"}, (String) null); //$NON-NLS-1$ //$NON-NLS-2$
		File target;
		if (path != null) {
			target = new File(path);
		} else if (pathInConfig != null) {
			target = new File(pathInConfig);
		} else {
			path = "./configuration/masterkey.bin";
			target = new File(path);
			println("The Master Key will be stored into the default file: " + target.getAbsolutePath());
		}
		if (!target.exists()) {
			if (!target.getParentFile().exists() && !target.getParentFile().mkdirs()) {
				printError("Unable to create the target file folders: " + target.getAbsolutePath());
				return ERROR_FILESYSTEM_ACCESS;
			}
			try {
				target.createNewFile();
			} catch (IOException e) {
				printError("Unable to create the target file: " + target.getAbsolutePath());
				if (isArgument("-debug")) { //$NON-NLS-1$
					e.printStackTrace();
				}
				return ERROR_FILESYSTEM_ACCESS;
			}
		}
		String key = getArgumentValue(new String[] {"-key", "-k"}, (String) null); //$NON-NLS-1$ //$NON-NLS-2$
		if (key != null) {
			key =  new String(Crypto.unFog(key));
		} else {
			if (pathInConfig != null) {
				File mkf = new File(pathInConfig);
				if (mkf.isFile()) {
					try {
						key = new String(Crypto.getChars(Files.readAllBytes(mkf.toPath()), StandardCharsets.UTF_8, 0));
					} catch (IOException e) {
						if (isArgument("-debug")) { //$NON-NLS-1$
							e.printStackTrace();
						}
					}
				}
			}
			if (key == null) {
				key = config.get("com.arcadsoftware.masterkey.fog"); //$NON-NLS-1$
				if (key != null) {
					key =  new String(Crypto.unFog(key));
				} else {
					key = config.get("com.arcadsoftware.masterkey"); //$NON-NLS-1$
					if (key == null) {
						printWarn("The Default master key was not initialized a new one has been generated.");
						key = RandomGenerator.randomStringSecure(256);
					}
				}
			}
		}
		if (isArgument("-debug")) { //$NON-NLS-1$
			println("The exported master key is: " + key);
		}
		if (save(target, key)) {
			println("Update the application configuration.");
			config.remove("com.arcadsoftware.masterkey"); //$NON-NLS-1$
			config.remove("com.arcadsoftware.masterkey.fog"); //$NON-NLS-1$
			if (path != null) {
				config.put("com.arcadsoftware.masterkey.path", path); //$NON-NLS-1$
			}
			saveConfigIni();
		}
		println("Done.");
		return 0;
	}

	@Override
	protected String getVersion() {
		return "1.0.0"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandFullName() {
		return "exportmasterkey"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandDescription() {
		return "This command allow to store the master key (used as source material to encrypt all critical data from the application) into an external file, by default the master key is stored in the configurations files.";
	}

	@Override
	protected Map<String, String> getArgumentsDescription() {
		HashMap<String, String> result = new HashMap<String, String>();
		result.put("[-k|-key <key>]", //$NON-NLS-1$
				"The Actual key to export, ignoring the current key in the configuration (Note that option may broke your installation). This key may be fogged.");
		result.put("[-t|-target <path>]", //$NON-NLS-1$
				"The file used to store the master key, this will override the current configuration. This path may be relative to the home directory or absolute.");
		return result;
	}

}
