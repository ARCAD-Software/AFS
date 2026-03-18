package cli;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;

import com.arcadsoftware.tool.cli.Command;

public class MoveServerBinaries extends Command {

	public static void main(String[] args) {
		System.exit(new MoveServerBinaries(args).exec());
	}

	public MoveServerBinaries() {
		super();
	}

	public MoveServerBinaries(String[] args) {
		super(args);
	}

	@Override
	protected int run() {
		println();
		Hashtable<String, Object> conf = getOSGiConfiguration("com.arcadsoftware.server.binaries"); //$NON-NLS-1$
		if (conf == null) {
			printError("No configuration found using default source.");
			return ERROR_INVALID_CONFIGURATION;
		}
		File source = new File((String) conf.getOrDefault("path", "files/bin")); //$NON-NLS-1$ //$NON-NLS-2$
		if (!source.isDirectory()) {
			printError("The source folder does not exists: " + source.getAbsolutePath());
			return ERROR_MISSING_FILE;
		}
		String t = getArgument(0);
		if (t.isBlank() || (t.charAt(0) == '-')) {
			println("The target folder must be set as the first argument of this command.");
			t = read("Type the target folder path. This path can be relaive to the home directory of the application:");
			if (t.isBlank()) {
				return ERROR_MISSING_PARAMETER;
			}
		}
		File target = new File(t);
		if (!target.isDirectory()) {
			if (target.exists()) {
				printError("Target path must be a writable directory.");
				return ERROR_WRONG_PARAMETER;
			}
			if (!isArgument("-c", "-create")) {
				printError("Target folder does not exists.");
				println("The target folder must exist on the file system.");
				return ERROR_WRONG_PARAMETER;
			}
			if (!target.mkdirs()) {
				try {
					printError("Unable to create the target folder: " + target.getCanonicalPath());
				} catch (IOException e) {
					printError("Unable to create the target folder: " + target.getAbsolutePath());
				}
				println("Target path must be a writable directory.");
				return ERROR_WRONG_PARAMETER;
			}
			println("Target folder created.");
		}
		if (target.equals(source)) {
			printError("Target and Source folder are the same.");
			return 0;
		}
		File[] list = source.listFiles();
		if (list == null) {
			printError("Unable to access to the Source folder: " + source.getAbsolutePath());
			return ERROR_FILESYSTEM_ACCESS;
		}
		if (isArgument("-debug")) { //$NON-NLS-1$
			println("Source folder contain " + list.length + " files/subfolders.");
		} 
		long i = moveFiles(list, target, isArgument("-debug", "-p", "-progres", "-progress")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		if (i < 0) {
			i = (-i) -1;
			println(i + " files moved. Anyway, if you can fix the problem, you can try to run this command again.");
			return ERROR_FILESYSTEM_ACCESS;
		}
		if (i == 0) {
			println("No files moved !");
		} else {
			println(i + " files moved.");
		}
		if (!source.delete()) {
			println("Unable to delete the source folder. You can nanually delete it, if it is empty.");
		}
		conf.put("path", t); //$NON-NLS-1$
		if (saveOSGiConfiguration()) {
			println("Binaries files moved and configuraiton updated.");
			return 0;
		}
		return ERROR_INVALID_CONFIGURATION;
	}

	private long moveFiles(File[] list, File target, boolean p) {
		long count = 0l;
		if (list != null) {
			for (File source: list) {
				if (source.isFile()) {
					File t = new File(target, source.getName());
					try {
						Files.move(source.toPath(), t.toPath(), StandardCopyOption.REPLACE_EXISTING);
						if (p) {
							println("File " + source.getAbsolutePath() + " moved.");
						}
						if (count >= 0) {
							count++;
						} else {
							count--;
						}
					} catch (IOException e) {
						printError("Unable to move the source file: " + source.getAbsolutePath());
						if (isArgument("-debug")) { //$NON-NLS-1$
							println(e.getLocalizedMessage());
						}
						if (count >= 0) {
							count = (-count) -1l; 
						}
					}
				} else if (source.isDirectory()) {
					File t = new File(target, source.getName());
					if (t.isDirectory() || t.mkdir()) {
						if (isArgument("-debug")) { //$NON-NLS-1$
							println("Moving Folder " + source.getAbsolutePath());
						}
						long i = moveFiles(source.listFiles(), t, p);
						if (i > 0) {
							if (count >= 0) {
								count += i;
							} else {
								count -= i;
							}
						} else if (i < 0) {
							if (count >= 0) {
								count = i - count;
							} else {
								count -= -i;
							}
						}
						if (!source.delete() && isArgument("-debug")) { //$NON-NLS-1$
							println("Unable to delete directory: " + source.getAbsolutePath());
						}
					} else {
						printError("Unable to create target directory: " + t.getAbsolutePath());
						if (count >= 0) {
							count = (-count) -1l; 
						}
					}
				} else {
					try {
						printError("Unable to move object: " + source.getCanonicalPath());
					} catch (IOException e) {
						printError("Unable to move object: " + source.getAbsolutePath());
					}
				}
			}
		}
		return count;
	}

	@Override
	protected String getVersion() {
		return "1.0.0"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandFullName() {
		return "movefilesbin"; //$NON-NLS-1$ 
	}

	@Override
	protected String getCommandDescription() {
		return "Move the folder used to store the Server Binaries files to a new location (and move the folder content too).";
	}

	@Override
	protected Map<String, String> getArgumentsDescription() {
		HashMap<String, String> result = new HashMap<String, String>();
		result.put("[<path>]", //$NON-NLS-1$
				"Define the new folder to store the server binaries files. This path can be relative to the home directory of the application. If not provided this parameter will be prompted during the execution of this command.");
		result.put("[-c,-create]", //$NON-NLS-1$
				"If the target directory does not exists create it.");
		result.put("[-p,-progress]", //$NON-NLS-1$
				"Print progression information while copying the files.");
		return result;
	}

}
