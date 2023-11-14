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

import java.io.BufferedReader;
import java.io.Console;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Scanner;

import com.arcadsoftware.cm.simple.Configuration;
import com.arcadsoftware.cm.simple.ConfigurationStoreManager;

import java.util.Map.Entry;

public abstract class Command {

	static {
		// Sometimes, the Server setting has a modification of the java path to a c:/ in lowercase.
		// This modification is not reproduisible in tbe .Bat file and must be corrected here.
		if ("true".equalsIgnoreCase(System.getProperty("minkey"))) { //$NON-NLS-1$ //$NON-NLS-1$
			String jh = System.getProperty("java.home"); //$NON-NLS-1$
			if ((jh.length() > 3) && (jh.charAt(1) == ':') && (jh.charAt(2) == '\\')) {
				System.setProperty("java.home", Character.toLowerCase(jh.charAt(0)) + jh.substring(1)); //$NON-NLS-1$
			}
		}
	}

	// Reserved System Exit code range from 32 to 64:
	// 32-39 : Low level error (relative to the Generic classes, configuration and installation).
	// 40-49 : Related to parameters...
	// 50-59 : Critical system situation which may require an human intervention.
	// 60-64 : Tests, automatic cancellation of operation...
	protected static final int ERROR_INTERNAL_ERROR = 32;
	protected static final int ERROR_MISSING_HOMMEDIR = 33;
	protected static final int ERROR_INVALID_CONFIGINI = 34;
	protected static final int ERROR_INVALID_CONFIGURATION = 35;
	protected static final int ERROR_MISSING_PARAMETER = 40;
	protected static final int ERROR_WRONG_PARAMETER = 41;
	protected static final int ERROR_MISSING_FILE = 42;
	protected static final int ERROR_DATABASE_CORRUPTED = 50;
	protected static final int ERROR_FILESYSTEM_ACCESS = 51;
	protected static final int ERROR_CANCELLED = 60;
	protected static final int ERROR_TEST_FAIL = 61;
	
	protected static final String[] CRYPTOSYSTEMKEYS = new String[] { //
			"com.arcadsoftware.salt.min.size", //$NON-NLS-1$
			"com.arcadsoftware.salt.size.variation", //$NON-NLS-1$
			"com.arcadsoftware.iv.min.size", //$NON-NLS-1$
			"com.arcadsoftware.iv.size.variation", //$NON-NLS-1$
			"com.arcadsoftware.hash.min.iterations", //$NON-NLS-1$
			"com.arcadsoftware.hash.iterations.variation", //$NON-NLS-1$
			"com.arcadsoftware.cypher.min.iterations", //$NON-NLS-1$
			"com.arcadsoftware.cypher.iterations.variation", //$NON-NLS-1$
			"com.arcadsoftware.masterkey.fog", //$NON-NLS-1$
			"com.arcadsoftware.masterkey", //$NON-NLS-1$
			"com.arcadsoftware.masterkey.path" //$NON-NLS-1$
	};
	
	private static final String[] MASTERKEYS = new String[] { //
			"com.arcadsoftware.masterkey.fog", //$NON-NLS-1$
			"com.arcadsoftware.masterkey", //$NON-NLS-1$
			"com.arcadsoftware.masterkey.path" //$NON-NLS-1$
	};
	
	private static final boolean ANSI_READY;
	
	static {
		boolean v = false;
		try {
			v = (System.console() != null) && (System.getenv().get("TERM") != null); //$NON-NLS-1$
		} catch (Throwable t) {}
		ANSI_READY = v;
	}
	
	private String[] arguments;
	private File appHomeDir;
	private HashMap<String, String> configini;
	private ConfigurationStoreManager configuration;
	private int error;
	
	public Command() {
		super();
	}
	
	public Command(String[] args) {
		super();
		init(args);
	}
	
	protected void init(String[] args) {
		arguments = loadArguments(args);
		if (isArgument("-v", "-version", "--version")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			print(getCommandFullName());
			print(" version ");
			println(getVersion());
			System.exit(0);
		}
		if (isArgument("-h", "-help", "--help")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			printHelp();
			System.exit(0);
		}
		if (isArgument("-debug")) { //$NON-NLS-1$
			System.setProperty("com.arcadsoftware.masterkey.trace", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			println("System Properties:");
			println("  java.home = " + System.getProperty("java.home")); //$NON-NLS-1$ //$NON-NLS-2$
			println("  os.arch =   " + System.getProperty("os.arch")); //$NON-NLS-1$ //$NON-NLS-2$
			println("  os.name =   " + System.getProperty("os.name")); //$NON-NLS-1$ //$NON-NLS-2$
			println("  user.name = " + System.getProperty("user.name")); //$NON-NLS-1$ //$NON-NLS-2$
			println("  user.home = " + System.getProperty("user.home")); //$NON-NLS-1$ //$NON-NLS-2$
			println("  user.dir =  " + System.getProperty("user.dir")); //$NON-NLS-1$ //$NON-NLS-2$
			println();
		}
		appHomeDir = initHomeDirectory();
		if (appHomeDir == null) {
			System.exit(ERROR_MISSING_HOMMEDIR);
		}
		configini = initConfigIni();
		initMasterKey();
		configuration = loadConfiguration();
	}

	protected void printHelp() {
		print("Usage of ");
		print(getCommandFullName());
		print(" (version ");
		print(getVersion());
		println(") :"); //$NON-NLS-1$
		println(getCommandDescription());
		println();
		Map<String, String> a = getArgumentsDescription();
		print("  "); //$NON-NLS-1$
		print(getCommandFullName());
		int margin = 18;
		int am = getCommandFullName().length() + 2;
		int p = am;
		if (a != null) {
			for (String k: a.keySet()) {
				p += k.length() + 1;
				if (p > 80) {
					println();
					for (int x = 0; x < am; x++) {
						System.out.print(" "); //$NON-NLS-1$
					}
					p = am;
				}
				System.out.print(" "); //$NON-NLS-1$
				System.out.print(k);
				if (k.length() > margin) {
					margin = k.length();
				}
			}
		}
		for (String k: new String[] {"[-v]", "[-h]", "[-homedir <path>]"}) {
			p += k.length() + 1;
			if (p > 80) {
				println();
				for (int x = 0; x < am; x++) {
					System.out.print(" "); //$NON-NLS-1$
				}
				p = am;
			}
			System.out.print(" "); //$NON-NLS-1$
			System.out.print(k);
			if (k.length() > margin) {
				margin = k.length();
			}
		}
		margin++;
		println();
		println("Arguments:");
		if (a != null) {
			for (Entry<String, String> e: a.entrySet()) {
				printarg(e.getKey(), margin, ": ", e.getValue()); //$NON-NLS-1$
			}
		}
		printarg("[-v|-version]", margin, ": ", "Print the command version, ignore other arguments."); //$NON-NLS-1$ //$NON-NLS-2$
		printarg("[-h|-help]", margin, ": ", "Print this message, ignore other arguments."); //$NON-NLS-1$ //$NON-NLS-2$
		printarg("[-homedir <path>]", margin, ": ", "Specify the path to the application home directory if it is not the folder where the command is run."); //$NON-NLS-1$ //$NON-NLS-2$
		println();
		println("Note: All arguments can be set to the command through a file, in that case the only argument that must be passed to the command is the path to this file. this file must contain all required arguments, one argument per line.");
		println();
	}
	
	public final void exec() {
		if (error != 0) {
			System.exit(error);
		}
		try {
			System.exit(run());
		} catch (Throwable e) {
			System.err.println("INTERNAL ERROR [" + e.getClass().getSimpleName() + "]: " + e.getLocalizedMessage());
			if (isArgument("-debug")) { //$NON-NLS-1$
				e.printStackTrace();
			}
			System.exit(ERROR_INTERNAL_ERROR);
		}
	}
	
	private String[] loadArguments(String[] args) {
		if (args.length == 1) {
			File cf = new File(args[0]);
			if (cf.isFile()) {
				ArrayList<String> result = new ArrayList<String>();
				try (FileReader fr = new FileReader(cf)) {
					try (BufferedReader reader = new BufferedReader(fr)) {
						String line;
						while ((line = reader.readLine()) != null) {
							result.add(line);
						}
						return result.toArray(new String[result.size()]);
					}
				} catch (IOException e) {
					println("Error while trying the load argument file: " + e.getLocalizedMessage());
				}
			}
		}
		return args;
	}

	protected File getConfigurationRoot() {
		String loc = configini.get("osgi.cm.storefile"); //$NON-NLS-1$
		File l = null;
		if ((loc != null) && !loc.isEmpty()) {
			l = new File(loc);
		} else {
			l = new File(appHomeDir, "configuration"); //$NON-NLS-1$
			if (!l.isDirectory()) {
				l = new File(System.getProperty("user.dir") + "/configuration"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			l = new File(l, "osgi.cm.ini"); //$NON-NLS-1$
		}
		return l;
	}
	
	protected ConfigurationStoreManager loadConfiguration() {
		ConfigurationStoreManager result = new ConfigurationStoreManager(getConfigurationRoot());
		result.setUseCFG(true);
		result.setUseINI(true);
		result.setUseJSON(true);
		try {
			result.load();
		} catch (IOException e) {
			System.out.println("Error while loading OSGi configuration: " + e.getLocalizedMessage());
			return null;
		}
		return result;
	}

	protected void initMasterKey() {
		boolean warn = true;
		for (String key: MASTERKEYS) {
			if (configini.containsKey(key)) {
				warn = false;
				break;
			}
		}
		if (warn) {
			printWarn("The configuration file does not contain a Master Key definition, This may lead to problems if an encrypted material need to be decripted with this command and the environment variables differ from the ones used to run the application server.");
			println("You should reffer to installation documentation and add the Master Key definition into the config.ini file.");
		}
		for (String key: CRYPTOSYSTEMKEYS) {
			String v = configini.get(key);
			if (v != null) {
				System.setProperty(key, v);
			}
		}
		if (isArgument("-debug")) { //$NON-NLS-1$
			println("The Cryptographic API is initialized.");
		}
	}

	protected HashMap<String, String> initConfigIni() {
		HashMap<String, String> result = new HashMap<String, String>();
		File configini = new File(appHomeDir, "configuration/config.ini"); //$NON-NLS-1$
		if (configini.isFile()) {
			Properties p = new Properties();
			try (FileInputStream fis = new FileInputStream(configini)) {
				p.load(fis);
				Enumeration<Object> e = p.keys();
				while (e.hasMoreElements()) {
					Object k = e.nextElement();
					if (k != null) {
						Object v = p.get(k);
						if (v != null) {
							result.put(k.toString(), v.toString());
						}
					}
				}
			} catch (IOException e) {
				try {
					println("LOADING ERROR: config.ini file is invalid: " + configini.getCanonicalPath());
				} catch (IOException e1) {
					println("LOADING ERROR: config.ini file is invalid: " + configini.getAbsolutePath());
				}
				println("ERROR " + e.getLocalizedMessage());
			}
		} else {
			try {
				printError("ERROR: config.ini not found: " + configini.getCanonicalPath());
			} catch (IOException e) {
				printError("ERROR: config.ini not found: " + configini.getAbsolutePath());
			}
			printError("This command must be run form the Application server home directory.");
		}
		return result;
	}

	public boolean saveConfigIni() {
		if (configini.isEmpty()) {
			return false;
		}
		Properties props = new Properties();
		for (Entry<String, String> e: configini.entrySet()) {
			props.setProperty(e.getKey(), e.getValue());
		}
		File configini = new File(appHomeDir, "configuration/config.ini"); //$NON-NLS-1$
		if (configini.isFile()) {
			configini.delete();
			try (FileOutputStream fos = new FileOutputStream(configini)) {
				props.store(fos, "Configuration File"); //$NON-NLS-1$
				return true;
			} catch (IOException e) {
				try {
					printError("ERROR While recording modification to config.ini file \"" + //
							configini.getCanonicalPath() + "\": " + e.getLocalizedMessage()); //$NON-NLS-1$
				} catch (IOException e1) {
					printError("ERROR While recording modification to config.ini file \"" + //
							configini.getAbsolutePath() + "\": " + e.getLocalizedMessage()); //$NON-NLS-1$
				}
			}
		}		
		return false;
	}

	public boolean saveOSGiConfiguration() {
		if (configuration != null) {
			try {
				configuration.save();
				return true;
			} catch (IOException e) {
				printError("ERROR While recording the OSGi configuration: " + e.getLocalizedMessage());
			}
		}
		return false;
	}
	
	protected File getHomeDirectory() {
		return appHomeDir;
	}
	
	protected Hashtable<String, Object> getOSGiConfiguration(String pid) {
		if (configuration != null) {
			return configuration.addConfiguration(pid);
		}
		return null;
	}
	
	protected List<Configuration> listOSgiConfigurations() {
		return configuration.listAllConfigurations();
	}
	
	protected String getProperty(Map<String, ?> props, String name, String defValue) {
		Object o = props.get(name);
		if (o != null) {
			return o.toString().trim();
		}
		return defValue;
	}

	protected Integer getProperty(Map<String, ?> props, String name, int defValue) {
		Object o = props.get(name);
		if (o instanceof Integer) {
			return (Integer) o;
		}
		if (o != null) {
			try {
				return Integer.valueOf(o.toString());
			} catch (NumberFormatException e) {
			}
		}
		return defValue;
	}

	protected Long getProperty(Map<String, ?> props, String name, long defValue) {
		Object o = props.get(name);
		if (o instanceof Long) {
			return (Long) o;
		}
		if (o != null) {
			try {
				return Long.valueOf(o.toString());
			} catch (NumberFormatException e) {
			}
		}
		return defValue;
	}

	protected Boolean getProperty(Map<String, ?> props, String name, boolean defValue) {
		Object o = props.get(name);
		if (o instanceof Boolean) {
			return (Boolean) o;
		}
		if (o != null) {
			try {
				return Boolean.valueOf(o.toString());
			} catch (NumberFormatException e) {
			}
		}
		return defValue;
	}

	protected File initHomeDirectory() {
		String homedir = System.getProperty("afs.homedir"); //$NON-NLS-1$
		if (homedir == null) {
			homedir = getArgumentValue("-homedir", (String) null); //$NON-NLS-1$
		}
		if (homedir == null) {
			File f = new File("."); //$NON-NLS-1$
			if (f.isDirectory() && new File(f, "configuration").isDirectory() && //$NON-NLS-1$
					new File(f, "plugins").isDirectory()) { //$NON-NLS-1$
				homedir = f.getAbsolutePath();
			} else {
				f = new File(".."); //$NON-NLS-1$
				if (f.isDirectory()) {
					if (new File(f, "configuration").isDirectory() && //$NON-NLS-1$
							new File(f, "plugins").isDirectory()) { //$NON-NLS-1$
						homedir = f.getAbsolutePath();
					}
				}
			}
		}
		if (homedir == null) {
			printError("ERROR Home Directory not found. Use the option -homedir <path> to set it.");
			return null;
		}
		File f = new File(homedir);
		if (!f.isDirectory()) {
			try {
				printError("ERROR Home Directory is incorrect: " + f.getCanonicalPath());
			} catch (IOException e) {
				printError("ERROR Home Directory is incorrect: " + f.getAbsolutePath());
			}
			return null;
		}
		return f;
	}
	
	protected abstract int run();
	
	protected abstract String getVersion();
	
	protected abstract String getCommandFullName();

	protected abstract String getCommandDescription();
	
	protected abstract Map<String, String> getArgumentsDescription();
	
	/**
	 * Retrieve the config.ini content.
	 * 
	 * @return
	 */
	protected Map<String, String> getConfigIniProperties() {
		return configini;
	}
	
	/**
	 * Set all the given properties as System properties.
	 * 
	 * @param properties
	 */
	protected void setSystemProperties(Map<String, String> properties) {
		if (properties != null) {
			for (Entry<String, String> e: properties.entrySet()) {
				System.setProperty(e.getKey(), e.getValue());
			}
		}		
	}

	/**
	 * Set all the given properties that start with the given prefix as System properties.
	 * 
	 * @param properties
	 * @param prefix
	 */
	protected void setSystemProperties(Map<String, String> properties, String prefix) {
		if (properties != null) {
			for(Entry<String, String> e: properties.entrySet()) {
				if (e.getKey().startsWith(prefix)) {
					System.setProperty(e.getKey(), e.getValue());
				}
			}
		}		
	}
	
	protected boolean isArgument(String... arg) {
		for (String a: arguments) {
			for (String x: arg) {
				if (a.equalsIgnoreCase(x)) {
					return true;
				}
			}
		}
		return false;
	}
	
	protected String getArgument(int pos) {
		if ((pos < 0) || (pos >= arguments.length)) {
			return null;
		}
		return arguments[pos];
	}
	
	protected String getArgumentValue(String arg, String defaultValue) {
		for(int i = 0; i < arguments.length; i++) {
			if (arguments[i].equalsIgnoreCase(arg) && (i + 1 < arguments.length)) {
				return arguments[i + 1];
			}
		}
		String ae = arg.toLowerCase() + '=';
		for(String a: arguments) {
			if (a.toLowerCase().startsWith(ae)) {
				return a.substring(ae.length());
			}
		}
		ae = arg.toLowerCase() + ':';
		for(String a: arguments) {
			if (a.toLowerCase().startsWith(ae)) {
				return a.substring(ae.length());
			}
		}
		return defaultValue;
	}

	protected int getArgumentValue(String arg, int defaultValue) {
		String s = getArgumentValue(arg, ""); //$NON-NLS-1$
		if (!s.isEmpty()) {
			try {
				return Integer.parseInt(s);
			} catch (NumberFormatException e) {}
		}
		return defaultValue;
	}

	protected boolean getArgumentValue(String arg, boolean defaultValue) {
		String s = getArgumentValue(arg, (String) null);
		if (s != null) {
			return "true".equalsIgnoreCase(s) || "1".equals(s) || "yes".equalsIgnoreCase(s);
		}
		return defaultValue;
	}

	protected File getArgumentValue(String arg, File defaultValue) {
		String s = getArgumentValue(arg, (String) null);
		if (s != null) {
			return new File(s);
		}
		return defaultValue;
	}

	protected Properties getArgumentValue(String arg, Properties defaultValue) {
		File f = getArgumentValue(arg, (File) null);
		if ((f != null) && f.isFile()) {
			Properties result = new Properties(defaultValue);
			try (FileInputStream fis = new FileInputStream(f)) {
				result.load(fis);
				return result;
			} catch (IOException e) {
				println("LOADING ERROR: " + e.getLocalizedMessage());
			}
		}
		return defaultValue;
	}

	protected char[] getArgumentValue(String arg, char[] defaultValue) {
		String s = getArgumentValue(arg, (String) null);
		if (s != null) {
			return s.toCharArray();
		}
		return defaultValue;
	}

	protected char[] getArgumentValue(String[] arg, char[] defaultValue) {
		String s = getArgumentValue(arg, (String) null);
		if (s != null) {
			return s.toCharArray();
		}
		return defaultValue;
	}

	protected String[] getArgumentValues(String arg, String defaultValue) {
		ArrayList<String> result = new ArrayList<String>(arguments.length);
		String ae = arg.toLowerCase() + '=';
		String ap = arg.toLowerCase() + ':';
		for (int i = 0; i < arguments.length; i++) {
			if (arguments[i].equalsIgnoreCase(arg) && (i + 1 < arguments.length)) {
				result.add(arguments[i + 1]);
			} else {
				String x = arguments[i].toLowerCase();
				if (x.startsWith(ae)) {
					result.add(arguments[i].substring(ae.length()));
				} else if (x.startsWith(ap)) {
					result.add(arguments[i].substring(ae.length()));
				}
			}
		}
		if ((result.size() == 0) && (defaultValue != null)) {
			result.add(defaultValue);
		}
		return result.toArray(new String[result.size()]);
	}

	protected String getArgumentValue(String[] args, String defaultValue) {
		for (int i = 0; i < arguments.length; i++) {
			for(String a: args) {
				if (arguments[i].equalsIgnoreCase(a) && (i + 1 < arguments.length)) {
					return arguments[i + 1];
				}
			}
		}
		for(String a: args) {
			String ae = a.toLowerCase() + '=';
			for(String x: arguments) {
				if (x.toLowerCase().startsWith(ae)) {
					return x.substring(ae.length());
				}
			}
		}
		for(String a: args) {
			String ae = a.toLowerCase() + ':';
			for(String x: arguments) {
				if (x.toLowerCase().startsWith(ae)) {
					return x.substring(ae.length());
				}
			}
		}
		return defaultValue;
	}

	protected String[] getArgumentValues(String[] args) {
		ArrayList<String> result = new ArrayList<String>(arguments.length);
		for(int i = 0; i < arguments.length; i++) {
			for(String a: args) {
				if (arguments[i].equalsIgnoreCase(a) && (i + 1 < arguments.length)) {
					result.add(arguments[i + 1]);
				} else {
					String ae = a.toLowerCase() + '=';
					String x = arguments[i].toLowerCase();
					if (x.startsWith(ae)) {
						result.add(arguments[i].substring(ae.length()));
					} else {
						ae = a.toLowerCase() + ':';
						if (x.startsWith(ae)) {
							result.add(arguments[i].substring(ae.length()));
						}
					}
				}
			}
		}
		return result.toArray(new String[result.size()]);
	}

	protected int getArgumentValue(String[] args, int defaultValue) {
		String s = getArgumentValue(args, ""); //$NON-NLS-1$
		if (!s.isEmpty()) {
			try {
				return Integer.parseInt(s);
			} catch (NumberFormatException e) {}
		}
		return defaultValue;
	}

	protected File getArgumentValue(String[] arg, File defaultValue) {
		String s = getArgumentValue(arg, (String) null);
		if (s != null) {
			return new File(s);
		}
		return defaultValue;
	}

	protected Properties getArgumentValue(String[] arg, Properties defaultValue) {
		File f = getArgumentValue(arg, (File) null);
		if ((f != null) && f.isFile()) {
			Properties result = new Properties(defaultValue);
			try (FileInputStream fis = new FileInputStream(f)) {
				result.load(fis);
				return result;
			} catch (IOException e) {
				println("LOADING ERROR: " + e.getLocalizedMessage());
			}
		}
		return defaultValue;
	}

	protected int waitUserInput() {
		try {
			return System.in.read();
		} catch (IOException e) {
			return 0;
		}
	}
	
	protected void println(String format, Object... args) {
		println(String.format(format, args));
	}

	protected void println(Object text) {
		printww(0, text);
	}

	protected void println() {
		System.out.println();
	}

	protected void printarg(String prefix, int indentation, String infix, String text) {
		int i = 0;
		if (prefix != null) {
			if (ANSI_READY) {
				System.out.print("\033[0;33m" + prefix.toString() + "\033[0m");
			} else {
				System.out.print(prefix);
			}
			i = prefix.length();
		} 
		if (indentation > 25) {
			indentation = 25;
		}
		for(; i < indentation; i++) {
			System.out.print(' ');
		}
		if (infix != null) {
			print(infix);
			indentation += infix.length();
		}
		printww(indentation, text);
	}

	protected void printww(final int marge, final Object text) {
		if (text != null) {
			String s;
			if (text instanceof Object[]) {
				s = Arrays.deepToString((Object[]) text);
			} else if (text instanceof int[]) {
				s = Arrays.toString((int[]) text);
			} else if (text instanceof boolean[]) {
				s = Arrays.toString((boolean[]) text);
			} else if (text instanceof char[]) {
				s = new String((char[]) text);
			} else {
				s = text.toString();
			}
			if (marge + s.length() > 80) {
				int max = 79 - marge;
				if (max <= 0) {
					System.out.println(s);
				} else {
					int i = s.indexOf('\n');
					if ((i < max) && (i >= 0)) {
						max = i;
					} else {
						i = max; 
						while (i > 0) {
							if (Character.isWhitespace(s.charAt(i))) {
								break;
							}
							i--;
						}
						if (i == 0) {
							i = max;
						}
					}
					i++;
					System.out.println(s.substring(0, i));
					for(int x = 0; x < marge; x++) {
						System.out.print(' ');
					}
					printww(marge, s.substring(i));
				}
			} else {
				System.out.println(s);
			}
		}
	}
	
	protected void print(Object text) {
		System.out.print(text);
	}

	protected void printError(Object text) {
		if (text != null) {
			if (ANSI_READY) {
				System.out.println("\033[1;31m" + text.toString() + "\033[0m"); //$NON-NLS-1$ //$NON-NLS-2$
			} else {
				System.err.println(text);
			}
		}
	}

	protected void printWarn(Object text) {
		if (text != null) {
			if (ANSI_READY) {
				System.out.println("\033[0;33m" + text.toString() + "\033[0m"); //$NON-NLS-1$ //$NON-NLS-2$
			} else {
				System.err.println("WARNING: " + text);
			}
		}
	}
	
	public int getErrorStatus() {
		return error;
	}
	
	protected void setErrorStatus(int errorStatus) {
		if ((error == 0) || (errorStatus == 0) || ((error > 0) && (error < errorStatus)) || ((error < 0) && (error > errorStatus))) {
			error = errorStatus;
		}
	}
	
	public boolean isError() {
		return error != 0; 
	}
	
	@SuppressWarnings("resource")
	protected String read(Object text) {
		println(text);
		return new Scanner(System.in).nextLine();
	}

	protected char[] readSecret(Object text) {
		Console c = System.console();
		if (c == null) {
			return read(text).toCharArray();
		}
		return c.readPassword(text.toString());
	}
	
}
