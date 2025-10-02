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
package run;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * This class is a wrapper allowing to upgrade the classpath with missing plugins references.
 * 
 * @author ARCAD Software
 */
public class Exec {

	public static final String H2 = "com.h2database.light_"; //$NON-NLS-1$
	public static final String H2_JTS = "org.locationtech.jts.jts-core_"; //$NON-NLS-1$
	public static final String PGSQL = "org.postgresql.jdbc_"; //$NON-NLS-1$
	public static final String LDAP = "com.unboundid.ldap.sdk_"; //$NON-NLS-1$
	public static final String JT400 = "org.apache.servicemix.bundles.jt400_"; //$NON-NLS-1$

	private static final String[] CP_BASE = new String[] {
			"javax.servlet-api_", //$NON-NLS-1$
			"org.eclipse.osgi_", //$NON-NLS-1$
			"org.eclipse.osgi.service.cm_", //$NON-NLS-1$
			"org.eclipse.osgi.util.function_", //$NON-NLS-1$
			"org.eclipse.osgi.util.promise_", //$NON-NLS-1$
			"org.eclipse.equinox.common_", //$NON-NLS-1$
			"bcprov", //$NON-NLS-1$
			"bcpkix", //$NON-NLS-1$
			"bcutil", //$NON-NLS-1$
			"org.ops4j.pax.logging.pax-logging-api_", //$NON-NLS-1$
			"com.arcadsoftware.crypt_", //$NON-NLS-1$
			"com.arcadsoftware.osgi_", //$NON-NLS-1$
			"com.arcadsoftware.cm.simple_" //$NON-NLS-1$
	};

	private static HashMap<String, String[]> CP_SPE = new HashMap<String, String[]>();
	static {
		CP_SPE.put("cli.ChangeDBPassword", new String[] {
				H2,
				H2_JTS,
				PGSQL,
				JT400
		});
		CP_SPE.put("cli.ConfigAdminUser", new String[] {
				H2,
				H2_JTS,
				PGSQL,
				JT400
		});
		CP_SPE.put("cli.DBH2Backup", new String[] {
				H2_JTS,
				H2
		});
		CP_SPE.put("cli.DBH2Restore", new String[] {
				H2_JTS,
				H2
		});
		CP_SPE.put("cli.DBMigration", new String[] {
				H2,
				H2_JTS,
				PGSQL,
				JT400
		});
		CP_SPE.put("cli.DBUpdate", new String[] {
				H2,
				H2_JTS,
				PGSQL,
				JT400
		});
		CP_SPE.put("cli.TestDB", new String[] {
				H2,
				H2_JTS,
				PGSQL,
				JT400
		});
		CP_SPE.put("cli.TestLDAP", new String[] {
				LDAP
		});
		CP_SPE.put("org.h2.tools.Console", new String[] {
				H2_JTS,
				H2
		});
	}

	static {
		if (System.getProperty("org.ops4j.pax.logging.DefaultServiceLog.level") == null) {
			System.setProperty("org.ops4j.pax.logging.DefaultServiceLog.level", "ERROR");
		}
	}

	public static void main(String[] args) {
		System.exit(exec(args));
	}

	public static int exec(String[] args) {
		if ((args == null) || (args.length == 0)) {
			System.err.println("Missing arguments: The command to run is missing. Check the execution shell script...");
			return 40;
		}
		String classname = args[0];
		String[] arguments = new String[args.length - 1];
		for (int i = 1; i < args.length; i++) {
			arguments[i - 1] = args[i];
		}
		// Test the working directory...
		File homedir = getHomeDirectory(arguments);
		File pluginsDir = new File(homedir, "plugins"); //$NON-NLS-1$
		if (!pluginsDir.isDirectory()) {
			System.err.println("ERROR: Plugins folder not found in home directory.");
			return 33;
		}
		File toolsDir = new File(homedir, "tools"); //$NON-NLS-1$
		if (!toolsDir.isDirectory()) {
			System.err.println("ERROR: Tools folder not found in home directory.");
			return 33;
		}
		// Compute the classpath of the required sub command...
		ArrayList<File> files = new ArrayList<File>();
		File[] plugins = pluginsDir.listFiles();
		for (String p : CP_BASE) {
			File pl = getJar(plugins, p);
			if (pl == null) {
				// TODO ajouter un mécanisme de substitution !
				System.err.println("WARNING: Required plugin jar not found: " + p + "*.jar");
			} else {
				files.add(pl);
			}
		}
		String[] sup = CP_SPE.get(classname);
		if (sup != null) {
			for (String p : sup) {
				File pl = getJar(plugins, p);
				if (pl == null) {
					// TODO ajouter un mécanisme de substitution !
					System.err.println("WARNING: Required plugin jar not found: " + p + "*.jar");
				} else {
					files.add(pl);
				}
			}
		}
		// Add specific jar tools...
		if ("com.arcadsoftware.tool.cli.DBH2BackupVersion14199".equals(classname)) {
			File h2Driver = getJar(toolsDir.listFiles(), "com.h2database.light_1.4."); //$NON-NLS-1$
			if (h2Driver == null) {
				System.err.println(
						"WARNING: The legacy H2 driver version 1.4.x must be present in the tools directory to be able to upgrade the database.");
				return 36;
			}
			files.add(h2Driver);
		}
		// Add a copy of this classes Jar...
		File tools = getJar(toolsDir.listFiles(), "com.arcadsoftware.tool.cli"); //$NON-NLS-1$
		if (tools == null) {
			System.err.println("ERROR: Tools program not found in home directory.");
			return 36;
		}
		files.add(tools);
		// Execute the command...
		ClassLoader origin = Thread.currentThread().getContextClassLoader();
		try (URLClassLoader ccl = getClassLoader(files)) {
			Thread.currentThread().setContextClassLoader(ccl);
			Class<?> c = Class.forName(classname, true, ccl);
			// ccl.loadClass(classname);
			Constructor<?> cc = c.getConstructor(String[].class);
			Object o = cc.newInstance((Object) arguments);
			Method m = c.getMethod("exec");
			Object r = m.invoke(o);
			if (r instanceof Integer) {
				return (Integer) r;
			}
			return 0;
		} catch (Throwable e) {
			String m = e.getLocalizedMessage();
			if (m == null) {
				m = e.getMessage();
				if (m == null) {
					m = '[' + e.getClass().getName() + ']';
					if (e.getCause() != null) {
						m += ' ' + e.getCause().getLocalizedMessage();
					}
				}
			}
			System.err.println("CRITICAL ERROR: " + m);
			if (isArgument(arguments, "-debug")) { //$NON-NLS-1$
				System.out.println("Classpath = " + files.toString());
				while (e != null) {
					System.err.println(e);
					for (StackTraceElement ste : e.getStackTrace()) {
						System.err.println("\tat " + ste);
					}
					e = e.getCause();
					if (e != null) {
						System.err.print("Caused by: ");
					}
				}
			}
			return 32;
		} finally {
			Thread.currentThread().setContextClassLoader(origin);
		}
		// TODO allow to execute multipart commands (with differents classpath...)
	}

	private static File getHomeDirectory(String[] args) {
		String homedir = System.getProperty("afs.homedir"); //$NON-NLS-1$
		if (homedir == null) {
			homedir = getArgumentValue(args, "-homedir", (String) null); //$NON-NLS-1$
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
			System.err.println("ERROR Home Directory not found. Use the option -homedir <path> to set it.");
			System.exit(33);
		}
		File f = new File(homedir);
		if (!f.isDirectory()) {
			System.err.println("ERROR Home Directory is incorrect: " + f.getAbsolutePath());
			System.exit(33);
		}
		return f;
	}

	private static String getArgumentValue(String[] arguments, String arg, String defaultValue) {
		for (int i = 0; i < arguments.length; i++) {
			if (arguments[i].equalsIgnoreCase(arg) && (i + 1 < arguments.length)) {
				return arguments[i + 1];
			}
		}
		String ae = arg.toLowerCase() + '=';
		for (String a : arguments) {
			if (a.toLowerCase().startsWith(ae)) {
				return a.substring(ae.length());
			}
		}
		ae = arg.toLowerCase() + ':';
		for (String a : arguments) {
			if (a.toLowerCase().startsWith(ae)) {
				return a.substring(ae.length());
			}
		}
		return defaultValue;
	}

	private static boolean isArgument(String[] arguments, String arg) {
		for (String a : arguments) {
			if (a.equalsIgnoreCase(arg)) {
				return true;
			}
		}
		return false;
	}

	private static URLClassLoader getClassLoader(List<File> files) {
		URL[] urls = new URL[files.size()];
		for (int i = 0; i < files.size(); i++) {
			try {
				urls[i] = files.get(i).toURI().toURL();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		// Create a new class loader independent from the current one.
		return new URLClassLoader(urls, ClassLoader.getPlatformClassLoader());
	}

	private static File getJar(File[] dir, String prefix) {
		for (File f : dir) {
			String fn = f.getName().toLowerCase();
			// TODO support directory plugins !
			if (fn.startsWith(prefix) && fn.endsWith(".jar")) { //$NON-NLS-1$
				return f;
			}
		}
		return null;
	}

}
