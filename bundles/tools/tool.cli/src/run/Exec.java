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

	private static String[] CP_BASE = new String[] {
			"javax.servlet_", //$NON-NLS-1$
			"org.eclipse.osgi_", //$NON-NLS-1$
			"org.eclipse.osgi.services_", //$NON-NLS-1$
			"org.eclipse.equinox.common_", //$NON-NLS-1$
			"bcprov", //$NON-NLS-1$
			"bcpkix", //$NON-NLS-1$
			"bcutil", //$NON-NLS-1$
			"com.arcadsoftware.crypt_", //$NON-NLS-1$
			"com.arcadsoftware.osgi_", //$NON-NLS-1$
			"com.arcadsoftware.cm.simple_" //$NON-NLS-1$
	};
	
	private static HashMap<String, String[]> CP_SPE = new HashMap<String, String[]>();
	static {
		CP_SPE.put("cli.ChangeDBPassword", new String[] {
				"org.ops4j.pax.logging.pax-logging-api_", //$NON-NLS-1$
				"com.h2database_1.4", //$NON-NLS-1$
				"com.sun.jna_", //$NON-NLS-1$
				"com.sun.jna.platform_", //$NON-NLS-1$
				"waffle_", //$NON-NLS-1$
				"org.postgresql.jdbc_" //$NON-NLS-1$
		});
		CP_SPE.put("cli.ConfigAdminUser", new String[] {
				"org.ops4j.pax.logging.pax-logging-api_", //$NON-NLS-1$
				"com.h2database_1.4", //$NON-NLS-1$
				"com.sun.jna_", //$NON-NLS-1$
				"com.sun.jna.platform_", //$NON-NLS-1$
				"waffle_", //$NON-NLS-1$
				"org.postgresql.jdbc_" //$NON-NLS-1$
		});
		CP_SPE.put("cli.DBH2Backup", new String[] {
				"org.ops4j.pax.logging.pax-logging-api_", //$NON-NLS-1$
				"com.h2database_1.4" //$NON-NLS-1$
		});
		CP_SPE.put("cli.DBH2Restore", new String[] {
				"org.ops4j.pax.logging.pax-logging-api_", //$NON-NLS-1$
				"com.h2database_1.4" //$NON-NLS-1$
		});
		CP_SPE.put("cli.DBMigration", new String[] {
				"org.ops4j.pax.logging.pax-logging-api_", //$NON-NLS-1$
				"com.h2database_1.4", //$NON-NLS-1$
				"com.sun.jna_", //$NON-NLS-1$
				"com.sun.jna.platform_", //$NON-NLS-1$
				"waffle_", //$NON-NLS-1$
				"org.postgresql.jdbc_" //$NON-NLS-1$
		});
		CP_SPE.put("cli.DBUpdate", new String[] {
				"org.ops4j.pax.logging.pax-logging-api_", //$NON-NLS-1$
				"com.h2database_1.4", //$NON-NLS-1$
				"com.sun.jna_", //$NON-NLS-1$
				"com.sun.jna.platform_", //$NON-NLS-1$
				"waffle_", //$NON-NLS-1$
				"org.postgresql.jdbc_" //$NON-NLS-1$
		});
		CP_SPE.put("cli.TestDB", new String[] {
				"org.ops4j.pax.logging.pax-logging-api_", //$NON-NLS-1$
				"com.h2database_1.4", //$NON-NLS-1$
				"com.sun.jna_", //$NON-NLS-1$
				"com.sun.jna.platform_", //$NON-NLS-1$
				"waffle_", //$NON-NLS-1$
				"org.postgresql.jdbc_" //$NON-NLS-1$
		});
		CP_SPE.put("cli.TestLDAP", new String[] {
				"com.unboundid.ldap.sdk_" //$NON-NLS-1$
		});
		CP_SPE.put("org.h2.tools.Console", new String[] {
				"org.ops4j.pax.logging.pax-logging-api_", //$NON-NLS-1$
				"com.h2database_1.4" //$NON-NLS-1$
		});
	}
	
	public static void main(String[] args) {
		if ((args == null) || (args.length == 0)) {
			System.exit(40);
		}
		String classname = args[0];
		String[] arguments = new String[args.length -1];
		for (int i = 1; i < args.length; i++) {
			arguments[i - 1] = args[i];
		}
		// Test the working directory...
		File homedir = getHomeDirectory(arguments);
		File pluginsDir = new File(homedir, "plugins"); //$NON-NLS-1$
		if (!pluginsDir.isDirectory()) {
			System.err.println("ERROR: Plugins folder not found in home directory.");
			System.exit(33);
		}
		File toolsDir = new File(homedir, "tools"); //$NON-NLS-1$
		if (!toolsDir.isDirectory()) {
			System.err.println("ERROR: Tools folder not found in home directory.");
			System.exit(33);
		}
		// Compute the classpath of the required sub command...
		ArrayList<File> files = new ArrayList<File>();
		File[] plugins = pluginsDir.listFiles();
		for(String p: CP_BASE) {
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
			for(String p: sup) {
				File pl = getJar(plugins, p);
				if (pl == null) {
					// TODO ajouter un mécanisme de substitution !
					System.err.println("WARNING: Required plugin jar not found: " + p + "*.jar");
				} else {
					files.add(pl);
				}
			}
		}
		// Add a copy of this classes Jar...
		File tools = getJar(toolsDir.listFiles(), "com.arcadsoftware.tool.cli-"); //$NON-NLS-1$
		if (tools == null) {
			System.err.println("ERROR: Tools program not found in home directory.");
			System.exit(33);
		}
		files.add(tools);
		// Execute the command...
		ClassLoader ccl = getClassLoader(files);
		Thread.currentThread().setContextClassLoader(ccl);  
		try {
			Class<?> c = Class.forName(classname, true , ccl);
			Constructor<?> cc = c.getConstructor(String[].class);
			Object o = cc.newInstance((Object) arguments);
			Method m = c.getMethod("exec");
			m.invoke(o);
		} catch (Exception e) {
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
				e.printStackTrace();
			}
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
		for (String a: arguments) {
			if (a.toLowerCase().startsWith(ae)) {
				return a.substring(ae.length());
			}
		}
		ae = arg.toLowerCase() + ':';
		for (String a: arguments) {
			if (a.toLowerCase().startsWith(ae)) {
				return a.substring(ae.length());
			}
		}
		return defaultValue;
	}
	
	private static boolean isArgument(String[] arguments, String arg) {
		for (String a: arguments) {
			if (a.equalsIgnoreCase(arg)) {
				return true;
			}
		}
		return false;
	}

	private static ClassLoader getClassLoader(List<File> files) {
		URL[] urls = new URL[files.size()];
		for (int i = 0; i < files.size(); i++) {
		    try {
		        urls[i] = files.get(i).toURI().toURL();
		    } catch (Exception e) {
		        e.printStackTrace();
		    }
		}
		// Create a new class loader independent from the current one.
		return new URLClassLoader(urls, null);
	}

	private static File getJar(File[] dir, String prefix) {
		for (File f: dir) {
			String fn = f.getName().toLowerCase();
			// TODO support directory plugins !
			if (fn.startsWith(prefix) && fn.endsWith(".jar")) { //$NON-NLS-1$
				return f;
			}
		}
		return null;
	}
	
}
