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
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystemNotFoundException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.Iterator;
import java.util.stream.Stream;

import run.Exec;

public class Install {

	public static void main(String[] args) {
		String platform = null;
		boolean debug = false;
		String osn = System.getProperty("os.name"); //$NON-NLS-1$
		if (osn != null) {
			osn = osn.toLowerCase();
			if (osn.startsWith("windows")) { //$NON-NLS-1$
				platform = "win"; //$NON-NLS-1$
			} else { // default:
				platform = "unix"; //$NON-NLS-1$
			}
		}
		if (args.length > 0) {
			for (String arg: args) {
				if ("unix".equalsIgnoreCase(arg)) { //$NON-NLS-1$
					platform = "unix"; //$NON-NLS-1$
				} else if ("linux".equalsIgnoreCase(arg)) { //$NON-NLS-1$
					platform = "unix"; //$NON-NLS-1$
				} else if ("os400".equalsIgnoreCase(arg) || "ibmi".equalsIgnoreCase(arg)) { //$NON-NLS-1$ //$NON-NLS-2$
					platform = "unix"; //$NON-NLS-1$
				} else if ("win".equalsIgnoreCase(arg) || "windows".equalsIgnoreCase(arg)) { //$NON-NLS-1$ //$NON-NLS-2$
					platform = "win"; //$NON-NLS-1$
				} else if ("-debug".equalsIgnoreCase(arg)) { //$NON-NLS-1$
					debug = true;
				}
			}
		}
		File plugins = new File(getHomeDirectory(args), "plugins"); //$NON-NLS-1$
		if (!plugins.isDirectory()) {
			if (debug) {
				System.out.println("Unable to found the \"plugins\" directory. Use the option -homedir <path> to set the application home directory.");
			}
			plugins = null;
		}
		File target = new File(Install.class.getProtectionDomain().getCodeSource().getLocation().getPath()).getParentFile();
		if (debug) {
			System.out.println("Target directory is: " + target.getAbsolutePath());
		}
		if (!target.isDirectory()) {
			System.err.println("Unable to get the current installation path.");
			System.exit(-2);
		}
		if (platform != null) {
			if (debug) {
				System.out.println("Platform used: /build/" + platform);
			}
			try {
				extractFiles("/build/common", target, plugins, debug); //$NON-NLS-1$
				extractFiles("/build/" + platform, target, plugins, debug); //$NON-NLS-1$
				System.out.println("Tools Installation completed.");
			} catch (Exception e) {
				System.err.println(e.getLocalizedMessage());
				if (debug) {
					e.printStackTrace();
				}
				System.exit(-3);
			}
		} else {
			System.err.println("Unknown platform: " + osn + "\nPlease add \"unix\" or \"windows\" in the command line arguments.");
			System.exit(-1);
		}
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

	private static void extractFiles(String path, File target, File plugins, boolean debug) throws IOException, URISyntaxException {
		URI uri = Install.class.getResource(path).toURI();
        Path dirPath;
        if (uri.getScheme().equals("jar")) { //$NON-NLS-1$
        	try {
        		dirPath = FileSystems.getFileSystem(uri).getPath(path);
        	} catch (FileSystemNotFoundException e) {
        		dirPath = FileSystems.newFileSystem(uri, Collections.<String, Object> emptyMap()).getPath(path);
        	}
        } else {
        	dirPath = Paths.get(uri);
        }
        try (Stream<Path> walk = Files.walk(dirPath, 1)) {
	        for (Iterator<Path> it = walk.iterator(); it.hasNext();) {
	        	Path p = it.next();
	        	String name = p.getFileName().toString();
	        	if (!p.equals(dirPath) && isInstallable(name, plugins)) {
		        	File tf = new File(target, name);
		        	if (debug) {
		        		System.out.println("Extracting file: " + p.toString() + " to " + tf.getAbsolutePath());
		        	}
		        	if (tf.isFile()) {
			        	if (debug) {
			        		System.out.println("Target file already exist... remove it.");
			        		if (!tf.delete()) {
			        			System.out.println("Unable to delete old file: " + tf.getName());
			        		}
			        	}
		        	}
		        	byte[] buffer = java.nio.file.Files.readAllBytes(p);
		            try (FileOutputStream o = new FileOutputStream(tf)) {
		            	o.write(buffer);
		            	o.flush();
		            }
		            if (tf.getName().endsWith(".sh")) { //$NON-NLS-1$
		            	tf.setExecutable(true, true);
		            }
		            if (tf.getName().startsWith("tool.conf")) { //$NON-NLS-1$
		            	patchJarName(tf, debug);
		            }
	        	}
	        }
        }
	}

	private static boolean isInstallable(String name, File plugins) {
		if ((name == null) || name.isEmpty()) {
			return false;
		}
		name = name.toLowerCase();
		if (plugins == null) {
			return true;
		}
		// Define if the application must be installed according to the required classpath.
		boolean h2 = false;
		boolean pgsql = false;
		boolean ldap = false;
		boolean rest = false;
		for (File f: plugins.listFiles()) {
			if (f.isFile()) {
				String fn = f.getName();
				if (fn.startsWith(Exec.H2)) {
					h2 = true;
				} else if (fn.startsWith(Exec.PGSQL)) {
					pgsql = true;
				} else if (fn.startsWith(Exec.LDAP)) {
					ldap = true;
				} else if (fn.startsWith("com.arcadsoftware.server.restful")) { //$NON-NLS-1$
					rest = true;
				}
			}
		}
		if (!h2) {
			if (name.startsWith("h2")) { //$NON-NLS-1$
				return false;
			}
			if (!pgsql && (name.startsWith("dbpwd") || //$NON-NLS-1$
					name.startsWith("testds") || //$NON-NLS-1$
					name.startsWith("setloginpwd") || //$NON-NLS-1$
					name.startsWith("dbupdate"))) { //$NON-NLS-1$
				return false;
			}
		}
		if (!pgsql) {
			if (name.startsWith("dbmigration")) { //$NON-NLS-1$
				return false;
			}
		}
		if (!ldap) {
			if (name.startsWith("testldap")) { //$NON-NLS-1$
				return false;
			}
		}
		if (!rest) {
			if (name.startsWith("testhttp") || //$NON-NLS-1$
					name.startsWith("selfcerts")) { //$NON-NLS-1$
				return false;
			}
		}
		return true;
	}

	private static void patchJarName(File target, boolean debug) throws IOException, URISyntaxException {
		StringBuilder buffer = new StringBuilder();
		String jarName = new File(Install.class.getProtectionDomain().getCodeSource().getLocation().getPath()).getName();
		if (debug) {
			System.out.println("Inject Jar name \"" + jarName + "\" in " + target.getAbsolutePath());
		}
		try (BufferedReader reader = new BufferedReader(new FileReader(target))) {
			String line = reader.readLine();
			while (line != null) {
				int i = line.indexOf("tools.target.jar");
				if (i >= 0) {
					buffer.append(line.substring(0, i));
					buffer.append(jarName);
					buffer.append(line.substring(i + 16));
				} else {
					buffer.append(line);
				}
				buffer.append(System.lineSeparator());
				line = reader.readLine();
			}
		}
		Charset cs;
		if (target.getName().endsWith(".bat")) {
			cs = StandardCharsets.US_ASCII;
		} else {
			cs = StandardCharsets.UTF_8;
		}
        try (FileOutputStream o = new FileOutputStream(target, false)) {
        	o.write(buffer.toString().getBytes(cs));
        	o.flush();
        }
	}
}
