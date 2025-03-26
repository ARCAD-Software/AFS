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
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.Iterator;
import java.util.stream.Stream;

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
				} else if ("-debug".equalsIgnoreCase(arg)) {
					debug = true;
				}
			}
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
				extractFiles("/build/" + platform, target, debug);
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

	private static void extractFiles(String path, File target, boolean debug) throws IOException, URISyntaxException {
		URI uri = Install.class.getResource(path).toURI();
        Path dirPath;
        if (uri.getScheme().equals("jar")) { //$NON-NLS-1$
            dirPath = FileSystems.newFileSystem(uri, Collections.<String, Object>emptyMap()).getPath(path);
        } else {
        	dirPath = Paths.get(uri);
        }
        try (Stream<Path> walk = Files.walk(dirPath, 1)) {
	        for (Iterator<Path> it = walk.iterator(); it.hasNext();) {
	        	Path p = it.next();
	        	if (!p.equals(dirPath)) {
		        	File tf = new File(target, p.getFileName().toString());
		        	if (debug) {
		        		System.out.println("Extracting file: " + p.toString() + " to " + tf.getAbsolutePath());
		        	}
		        	byte[] buffer = java.nio.file.Files.readAllBytes(p);
		            try (FileOutputStream o = new FileOutputStream(tf)) {
		            	o.write(buffer);
		            	o.flush();
		            }
		            if (tf.getName().endsWith(".sh")) { //$NON-NLS-1$
		            	tf.setExecutable(true, true);
		            }
		            if (tf.getName().startsWith("tool.conf")) {
		            	patchJarName(tf, debug);
		            }
	        	}
	        }
        }
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
