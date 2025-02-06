/*******************************************************************************
 * Copyright (c) 2025 ARCAD Software.
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
package com.arcadsoftware.cm.simple.internal;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.URI;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.ParseException;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.Enumeration;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.stream.Stream;
import java.util.Map.Entry;
import java.util.Properties;

import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.service.cm.ConfigurationAdmin;

public abstract class AbstractConfigurationStorage {
	
	private static final String DOC_PROP_SECTION = "section"; //$NON-NLS-1$
	private static final String DOC_PROP_COMMENT = "comment"; //$NON-NLS-1$
	private static final String DOC_PROP_COMMENT_SUFFIX = '.' + DOC_PROP_COMMENT;
	//TODO valeurs par défaut si la section n'existe pas du tout.
	//private static final String DOC_PROP_DEFAULT_SUFFIX = ".default"; //$NON-NLS-1$
	//TODO une option de stockage pour des données sensible (dans un fichier binaire à part ? / crypté / référence à fichier externe) ?
	//private static final String DOC_PROP_STORAGE_SUFFIX = ".storage"; //$NON-NLS-1$
	
	protected static boolean isNotIgnored(String key) {
		return !Constants.SERVICE_PID.equals(key) && //
				!ConfigurationAdmin.SERVICE_FACTORYPID.equals(key) && //
				!ConfigurationAdmin.SERVICE_BUNDLELOCATION.equals(key);
	}

	private final HashMap<String, Properties> pidSections;
	private final HashMap<String, String> aliasesSectionPids;

	protected AbstractConfigurationStorage(Activator activator) {
		super();
		pidSections = new HashMap<String, Properties>();
		if (activator != null) {
			init(activator.getContext(), pidSections);
		} else {
			init(pidSections);
		}
		aliasesSectionPids = new HashMap<String, String>(pidSections.size());
		for (Entry<String, Properties> e: pidSections.entrySet()) {
			String section = (String) e.getValue().get(DOC_PROP_SECTION);
			if ((section != null) && !section.isEmpty()) {
				aliasesSectionPids.put(section.toLowerCase(), e.getKey());
			}
		}
	}
	
	private void init(BundleContext context, HashMap<String, Properties> table) {
		Enumeration<URL> urls = context.getBundle().findEntries("docs", "*.properties", true); //$NON-NLS-1$ //$NON-NLS-2$
		if (urls == null) {
			// for AFS debug only !!!
			urls = context.getBundle().findEntries("src/docs", "*.properties", true); //$NON-NLS-1$ //$NON-NLS-2$
		}
		if (urls != null) {
			while (urls.hasMoreElements()) {
				URL u = urls.nextElement();
				try (InputStream is = u.openStream()) {
					String p = u.getPath();
					if (!p.isEmpty()) {
						String pn = new File(p).getName();
						if (pn.length() > 11) {
							String pid = pn.substring(0, pn.length() - 11).trim();
							if (!pid.isEmpty()) {
			        			Properties props = new Properties();
			        			props.load(is);
		        				table.put(pid, props);
							}
						}
					}
				} catch (Exception e) {
					if (u != null) {
						System.err.println("Equinox return invalid URI \"" + u.toString() + "\" : " + e.getLocalizedMessage());
					} else {
						System.err.println("Equinox return invalid URI \"null\": " + e.getLocalizedMessage());
					}
				}
			}
		} else {
			init(table);
		}
	}

	/**
	 * Initialization 
	 * @param table the PID, properties file association.
	 */
	protected void init(HashMap<String, Properties> table) {
		try {
			URI uri = AbstractConfigurationStorage.class.getResource("/docs/").toURI(); //$NON-NLS-1$
	        if (uri.getScheme().equals("jar")) { //$NON-NLS-1$
	            try (FileSystem fileSystem = FileSystems.newFileSystem(uri, Collections.<String, Object>emptyMap())) {
	            	try (Stream<Path> walk = Files.walk(fileSystem.getPath("/docs"), 1)) { //$NON-NLS-1$
		                for (Iterator<Path> it = walk.iterator(); it.hasNext();) {
	                    	load(table, it.next());
		                }
	            	}
	            }
	        } else {
		        try (Stream<Path> walk = Files.walk(Paths.get(uri), 1)) {
			        for (Iterator<Path> it = walk.iterator(); it.hasNext();) {
	                	load(table, it.next());
			        }
		        }
	        }
		} catch (Exception e) {
			System.err.println("CM: Unable to access to docs: " + e.getLocalizedMessage());
		}
	}

	protected void load(HashMap<String, Properties> table, Path path) {
    	String pid = path.getFileName().toString();
        if (pid.endsWith(".properties")) { //$NON-NLS-1$
        	int i = pid.lastIndexOf('/');
        	if (i < 0) {
        		i = pid.lastIndexOf('\\');
        		if (i < 0) {
        			i = 0;
        		}
        	}
        	int j = pid.lastIndexOf('.');
        	if (j > i) {
        		pid = pid.substring(i, j).trim();
        		if (!pid.isEmpty()) {
        			Properties p = new Properties();
        			try (InputStream is = Files.newInputStream(path)) {
        				p.load(is);
        				table.put(pid, p);
        			} catch (IOException e) {}
        		}
        	}
        }
	}

	/**
	 * Load the configuration from the given storage root.
	 * 
	 * @param rootStorage An file existing file or folder where to look for configuration.
	 * @throws IOException
	 */
	public void load(File rootStorage, boolean useINI, boolean useCFG, boolean useJSON) throws IOException {
		// 1. Initialization from rootfile
		if (rootStorage.isFile() && useINI) {
			loadFromIniFile(rootStorage);
		}
		File root = null;
		if (rootStorage.isDirectory()) {
			root = rootStorage;
		} else {
			rootStorage = rootStorage.getParentFile();
			if (rootStorage.isDirectory()) {
				root = rootStorage;
			}
		}
		if (root != null) {
			if (useCFG) {
				for(File f: root.listFiles(new FilenameFilter() {
					@Override
					public boolean accept(File dir, String name) {
						return name.endsWith(".cfg"); //$NON-NLS-1$
					}
				})) {
					loadFromCFGFile(f);
				}
			}
			if (useJSON) {
				for(File f: root.listFiles(new FilenameFilter() {
					@Override
					public boolean accept(File dir, String name) {
						return name.endsWith(".json"); //$NON-NLS-1$
					}
				})) {
					loadFromJSONFile(f);
				}
			}
		}
	}

	public void save(File rootStorage, Map<String, ? extends Hashtable<String, Object>> config, boolean useINI, boolean useCFG, boolean useJSON) throws IOException {
		File backup = null;
		File root;
		if (rootStorage.isFile()) {
			root = rootStorage.getParentFile();
			backup = new File(rootStorage.getParent(), rootStorage.getName() + ".backup"); //$NON-NLS-1$
			if (backup.exists()) {
				if (!backup.delete()) {
					throw new IOException("Unable to delete old version of Configuration file: " + backup.getAbsolutePath());
				}
			}
			if (!rootStorage.renameTo(backup)) {
				throw new IOException("Unable to create backup of Configuration file: " + rootStorage.getAbsolutePath());
			}
		} else {
			if (rootStorage.getName().toLowerCase().endsWith(".ini")) {
				root = rootStorage.getParentFile();
			} else {
				useINI = false;
				root = rootStorage;
			}
			root.mkdirs();
		}
		if (useJSON) {
			for(File f: root.listFiles(new FilenameFilter() {
				@Override
				public boolean accept(File dir, String name) {
					return name.endsWith(".json"); //$NON-NLS-1$
				}
			})) {
				String pid = f.getName().substring(0, f.getName().length() - 5);
				Hashtable<String, Object> conf = config.get(pid);
				if (conf != null) {
					try {
						saveToJSON(f, pid, conf);
						config.remove(pid);
					} catch (IOException e) {
						System.err.println("OSGi Configuration Error (JSON Recording): " + e.getLocalizedMessage());
					}
				}
			}
		}
		if (useCFG) {
			for(File f: root.listFiles(new FilenameFilter() {
				@Override
				public boolean accept(File dir, String name) {
					return name.endsWith(".cfg"); //$NON-NLS-1$
				}
			})) {
				String pid = f.getName().substring(0, f.getName().length() - 4);
				Hashtable<String, Object> conf = config.get(pid);
				if (conf != null) {
					try {
						saveToCFG(f, pid, conf);
						config.remove(pid);
					} catch (IOException e) {
						System.err.println("OSGi Configuration Error (CFG Recording): " + e.getLocalizedMessage());
					}
				}
			}
		}
		if (useINI) {
			if (!config.isEmpty()) {
				saveToINI(rootStorage, config);
			}
		} else if (useCFG) {
			for (Entry<String, ? extends Hashtable<String, Object>> e: config.entrySet()) {
				saveToCFG(new File(root, e.getKey() + ".cfg"), e.getKey(), e.getValue()); //$NON-NLS-1$
			}
		} else if (useJSON) {
			for (Entry<String, ? extends Hashtable<String, Object>> e: config.entrySet()) {
				saveToJSON(new File(root, e.getKey() + ".json"), e.getKey(), e.getValue()); //$NON-NLS-1$
			}
		}
		if ((backup != null) && backup.isFile()) {
			if (!backup.delete()) {
				throw new IOException("Unable to delete of Configuration backup file: " + backup.getAbsolutePath());
			}
		}
	}

	private <T extends Hashtable<String, Object>> void saveToINI(File file, Map<String, T> config) throws IOException {
		try (FileOutputStream fos = new FileOutputStream(file)) {
			try (OutputStreamWriter out = new OutputStreamWriter(fos, StandardCharsets.UTF_8)) {
				try (BufferedWriter w = new BufferedWriter(out)) {
					writeIniFileHeader(w);
					TreeMap<String, Entry<String, T>> sortedConf = new TreeMap<String, Entry<String, T>>();
					for (Entry<String, T> ce: config.entrySet()) {
						String section = getSection(getFactoryPid(ce.getKey()), ce.getKey());
						if (section != null) {
							sortedConf.put(section, ce);
						}
					}
					for (Entry<String, Entry<String, T>> e: sortedConf.entrySet()) {
						TreeMap<String, String> values = new TreeMap<String, String>();
						int mtl = 0;
						for (Entry<String, Object> p: e.getValue().getValue().entrySet()) {
							String token = getToken(p.getKey());
							if (token != null) {
								if (mtl < token.length()) {
									mtl = token.length();
								}
								String val = getFileValue(p.getValue());
								if (val != null) {
									values.put(token, val);
								}
							}
						}
						if (!values.isEmpty()) {
							w.write(e.getKey());
							w.newLine();
							Properties doc = pidSections.get(e.getValue().getKey());
							if (doc != null) {
								writeComment(w, doc.getProperty(DOC_PROP_COMMENT));
							}
							w.newLine();
							for(Entry<String, String> v: values.entrySet()) {
								if (doc != null) {
									writeComment(w, doc.getProperty(v.getKey() + DOC_PROP_COMMENT_SUFFIX));
								}
								w.write(v.getKey());
								for(int i = v.getKey().length(); i < mtl; i++) {
									w.write(' ');
								}
								w.write(" = "); //$NON-NLS-1$
								w.write(v.getValue());
								w.newLine();
							}
							w.newLine();
							w.newLine();
						}
					}
				}
			}
		}
	}

	private void writeComment(BufferedWriter writer, String comment) throws IOException {
		if ((comment != null) && !comment.isEmpty()) {
			while (comment.length() > 80) {
				int i = comment.indexOf(' ', 80);
				if (i < 80) {
					break;
				}
				String c = comment.substring(0, i);
				comment = comment.substring(i + 1);
				writer.write('#');
				writer.write(' ');
				writer.write(c);
				writer.newLine();
			}
			if (!comment.isEmpty()) {
				writer.write('#');
				writer.write(' ');
				writer.write(comment);
				writer.newLine();
			}
		}
		
	}

	private void saveToCFG(File file, String pid, Hashtable<String, Object> conf) throws IOException {
		Properties doc = pidSections.get(pid);
		Properties p = new Properties();
		for (Entry<String, Object> e: conf.entrySet()) {
			if ((e.getValue() != null) && isNotIgnored(e.getKey())) {
				if (e.getValue() instanceof Boolean) {
					if ((Boolean) e.getValue()) {
						p.put(e.getKey(), "true");
					} else {
						p.put(e.getKey(), "false");
					}
				} else if (e.getValue() instanceof Date) {
					Calendar cal = new GregorianCalendar(TimeZone.getTimeZone("GMT")); //$NON-NLS-1$
					cal.setTime((Date) e.getValue());
					p.put(e.getKey(), toString(cal));
				} else if (e.getValue() instanceof Calendar) {
					p.put(e.getKey(), toString((Calendar) e.getValue()));
				} else {
					p.put(e.getKey(), e.getValue().toString());
				}
			}
		}
		String comment = "OSGI Configuration File"; //$NON-NLS-1$
		if (doc != null) {
			String c = doc.getProperty(DOC_PROP_COMMENT);
			if (c != null) {
				comment = c;
			}
		}
		if (p.isEmpty()) {
			if (file.isFile()) {
				if (!file.delete()) {
					try (FileOutputStream fos = new FileOutputStream(file)) {
						p.store(fos, comment);
					}
				}
			}
		}
		try (FileOutputStream fos = new FileOutputStream(file)) {
			p.store(fos, comment);
		}
	}

	private void saveToJSON(File file, String pid, Hashtable<String, Object> conf) throws IOException {
		Properties doc = pidSections.get(pid);
		String comment = "OSGI Configuration File, "; //$NON-NLS-1$
		if (doc != null) {
			String c = doc.getProperty(DOC_PROP_COMMENT);
			if (c != null) {
				comment = c + ", ";
			}
		}
		try (FileWriter w = new FileWriter(file)) {
			w.write("{\n  \"_comment\": \"" + comment + "save date: ");
			w.write(new Date().toString());
			w.write('"');
			for(Entry<String, Object> e: conf.entrySet()) {
				if (isNotIgnored(e.getKey())) {
					w.write(",\n  "); //$NON-NLS-1$
					writeString(w, e.getKey());
					w.write(": "); //$NON-NLS-1$
					writeValue(w, e.getValue());
				}
			}
			w.write("\n}\n"); //$NON-NLS-1$
		}
	}

	private void writeString(FileWriter w, String string) throws IOException {
		w.write('"');
		for(char c: string.toCharArray()) {
			switch (c) {
			case '\b':
				w.write("\\b"); //$NON-NLS-1$
				break;
			case '\f':
				w.write("\\f"); //$NON-NLS-1$
				break;
			case '\n':
				w.write("\\n"); //$NON-NLS-1$
				break;
			case '\r':
				w.write("\\r"); //$NON-NLS-1$
				break;
			case '\t':
				w.write("\\t"); //$NON-NLS-1$
				break;
			case '\\':
			case '"':
			case '/':
				w.write('\\');
			default:
				w.write(c);
			}
		}
		w.write('"');
	}

	private void writeValue(FileWriter w, Object value) throws IOException {
		if (value == null) {
			w.write("{}"); //$NON-NLS-1$
		} else if (value instanceof Boolean) {
			if ((Boolean) value) {
				w.write("true");
			} else {
				w.write("false");
			}
		} else if (value instanceof Integer) {
			w.write(value.toString());
		} else if (value instanceof Date) {
			Calendar cal = new GregorianCalendar(TimeZone.getTimeZone("GMT")); //$NON-NLS-1$
			cal.setTime((Date) value);
			w.write(toString(cal));
		} else if (value instanceof Calendar) {
			w.write(toString((Calendar) value));
		} else {
			writeString(w, value.toString());
		}
	}

	/**
	 * Write INI file comments.
	 * 
	 * @param w the Writer.
	 * @throws IOException
	 */
	protected void writeIniFileHeader(BufferedWriter w) throws IOException {
		w.write("# General Application configuration.");
		w.newLine();
		w.write("#");
		w.newLine();
		w.write("# Do not edit this file during the application execution, it will be overwritten.");
		w.newLine();
		w.write("# Latest generation: " + new Date().toString());
		w.newLine();
		w.newLine();
		w.newLine();
	}

	private void loadFromIniFile(File rootStorage) throws IOException {
		try (FileInputStream fis = new FileInputStream(rootStorage)) {
			try (InputStreamReader in = new InputStreamReader(fis, StandardCharsets.UTF_8)) {
				try (BufferedReader r = new BufferedReader(in)) {
					String line = r.readLine();
					Hashtable<String, Object> curconf = null; 
					while (line != null) {
						line = line.trim();
						if (!line.isEmpty() && (line.charAt(0) != '#') && (line.charAt(0) != ';')) {
							if ((line.charAt(0) == '[') && (line.charAt(line.length() - 1) == ']')) {
								line = line.substring(1, line.length() - 1).trim();
								int i = line.indexOf('/');
								String pid;
								String fpid = null;
								if (i > 0) {
									fpid = getPid(line.substring(0, i - 1).trim());
									try {
										Integer x = Integer.valueOf(line.substring(i + 1).trim()); 
										pid = fpid + '.' + x;
										Integer z = getFactoryCount(fpid);
										if ((z == null) || (z.intValue() < x.intValue())) {
											setFactoryCount(fpid, x);
										}
									} catch (NumberFormatException e) {
										logError("Invalid Section name in configuration: " + line, e);
										pid = getPid(line);
									}
								} else {
									pid = getPid(line);
								}
								curconf = addConfiguration(fpid, pid);
							} else if (curconf != null) {
								int i = line.indexOf('=');
								if (i > 0) {
									String key = getKey(line.substring(0, i));
									if (key != null) {
										Object value = getConfValue(line.substring(i + 1), r);
										if (value != null) {
											curconf.put(key, value);
										}
									}
								}
							}
						}
						line = r.readLine();
					}
				}
			}
		}
	}

	private void loadFromCFGFile(File file) throws IOException {
		String pid = file.getName().substring(0, file.getName().length() - 4);
		String fpid = getFPid(pid);
		Properties p = new Properties();
		try (FileInputStream fis = new FileInputStream(file)) {
			p.load(fis);
		}
		if (!p.isEmpty()) {
			Hashtable<String, Object> curconf = addConfiguration(fpid, pid);
			if (curconf != null) {
				Enumeration<Object> e = p.keys();
				while (e.hasMoreElements()) {
					Object k = e.nextElement();
					curconf.put(k.toString(), p.get(k));
				}
			}
		}
	}
	
	private void loadFromJSONFile(File file) throws IOException {
		String pid = file.getName().substring(0, file.getName().length() - 5);
		String fpid = getFPid(pid);
		Hashtable<String, Object> curconf = addConfiguration(fpid, pid);
		if (curconf != null) {
			try (FileReader r = new FileReader(file)) {
				if (readChar(r, '{')) {
					String key = readString(r);
					while (key != null) {
						if (!readChar(r, ':')) {
							break;
						}
						if ("_comment".equalsIgnoreCase(key)) { //$NON-NLS-1$
							readString(r);
						} else {
							curconf.put(key, getConfValue(readString(r)));
						}
						if (!readChar(r, ',')) {
							break;
						}
						key = readString(r);
					}
				}
			}
		}
	}

	private String readString(FileReader r) throws IOException {
		int c = r.read();
		while ((c >= 0) && Character.isSpaceChar(c)) {
			c = r.read();
		}
		if ((c == 0) || (c == '}') || (c == ',') || (c == ':')) {
			return null;
		}
		StringBuilder sb = new StringBuilder();
		if (c == '"') {
			c = r.read();
			while (c >= 0) {
				if (c == '"') {
					break;
				}
				if (c == '\\') {
					c = r.read();
					switch (c) {
					case '"':
					case '\\':
					case '/':
						break;
					case 'b':
						c = '\b';
						break;
					case 'n':
						c = '\n';
						break;
					case 'f':
						c = '\f';
						break;
					case 'r':
						c = '\r';
						break;
					case 'u':
						int h1 = r.read();
						if (h1 < 0) {
							return null;
						}
						int h2 = r.read();
						if (h2 < 0) {
							return null;
						}
						int h3 = r.read();
						if (h3 < 0) {
							return null;
						}
						int h4 = r.read();
						if (h4 < 0) {
							return null;
						}
						try {
							c = Integer.parseInt(new String(new char[] {(char) h1, (char) h2, (char) h3, (char) h4}), 16);
						} catch (NumberFormatException e) {
							return null;
						}
						break;
					default:
						return null;
					}
				}
				sb.append((char) c);
				c = r.read();
			}
		} else if (c == '{') {
			c = r.read(); // assume '}' ! do not care of malformed of to complex JSON files !
			return null;
		} else {
			sb.append((char) c);
			c = r.read();
			while ((c >= 0) && !Character.isSpaceChar(c) && (c != ',') && (c != '}')) {
				sb.append((char) c);
				c = r.read();
			}
		}
		return sb.toString();
	}

	private boolean readChar(FileReader r, char ch) throws IOException {
		int c = r.read();
		while ((c >= 0) && Character.isSpaceChar(c)) {
			c = r.read();
		}
		if (c == ch) {
			return true;
		}
		return false;
	}

	private String getFPid(String name) {
		int i = name.lastIndexOf('.');
		if (i > 0) {
			try {
				if (Integer.parseInt(name.substring(i + 1)) > 0) {
					return name.substring(0, i - 1);
				}
			} catch (NumberFormatException e) {}
		}
		return null;
	}

	private String getKey(String token) {
		token = token.trim();
		if (token.isEmpty() || "_".equals(token)) { //$NON-NLS-1$
			return null;
		}
		if (token.charAt(0) == '_') {
			return token.substring(1);
		}
		return token;
	}

	private String getPid(String section) {
		if (section.charAt(0) == '[') {
			section = section.substring(1, section.length() - 1).trim();
		}
		String sectionlc = section.toLowerCase();
		String p = aliasesSectionPids.get(sectionlc);
		if (p != null) {
			return p;
		}
		if (containChar(section, '.', '_')) {
			return section;
		}
		StringBuilder sb = new StringBuilder();
		if (sectionlc.startsWith("rest ")) { //$NON-NLS-1$
			section = section.substring(4).trim();
			sb.append("com.arcadsoftware.server.restful"); //$NON-NLS-1$
		} else {
			sb.append("com.arcadsoftware"); //$NON-NLS-1$
		}
		for (String s: section.split(" ")) { //$NON-NLS-1$
			sb.append('.');
			if (!s.isEmpty()) {
				char[] c = s.toCharArray();
				sb.append(Character.toLowerCase(c[0]));
				for(int i = 1; i < c.length; i++) {
					sb.append(c[i]);
				}
			}
		}
		return sb.toString();
	}

	private boolean containChar(String string, char... chars) {
		for(char c: string.toCharArray()) {
			for(char cc: chars) {
				if (c == cc) {
					return true;
				}
			}
		}
		return false;
	}
	
	private Object getConfValue(String string) {
		if (string == null) {
			return null;
		}
		String s = string.trim();
		try {
			switch (simpleType(s)) {
			case 1:
				return Boolean.valueOf(s);
			case 2:
				return Integer.valueOf(s);
			case 3:
				return Double.valueOf(s);
			case 4:
				return toDate(s);
			}
		} catch (ParseException | NumberFormatException e) {}
		return string;
	}	
	
	private Object getConfValue(String string, BufferedReader reader) throws IOException {
		string = string.trim();
		try {
			switch (simpleType(string)) {
			case 1:
				return Boolean.valueOf(string);
			case 2:
				return Integer.valueOf(string);
			case 3:
				return Double.valueOf(string);
			case 4:
				return toDate(string);
			}
		} catch (ParseException | NumberFormatException e) {}
		if (string.endsWith("\\")) { //$NON-NLS-1$
			if (string.endsWith("\\\\")) { //$NON-NLS-1$
				return string.substring(0, string.length() - 1);
			}
			StringBuilder sb = new StringBuilder(string.substring(0, string.length() - 1));
			String line = reader.readLine();
			while (line != null) {
				sb.append('\n');
				if (line.endsWith("\\")) { //$NON-NLS-1$
					sb.append(line.substring(0, line.length() - 1));
					if (line.endsWith("\\\\")) { //$NON-NLS-1$
						break;
					}
				} else {
					sb.append(line);
					break;
				}
				line = reader.readLine();
			}
			return sb.toString();
		}
		return string;
	}

	/**
	 * Convert a string representing an ISO Date format to a Calendar Object
	 * 
	 * @param string
	 * @return a GregorianCalendar representing the date in the GMT timezone.
	 * @throws ParseException If the string can not be parsed as an ISO Date Format.
	 */
	private Date toDate(String string) throws ParseException {
		Calendar result = new GregorianCalendar(TimeZone.getTimeZone("GMT")); //$NON-NLS-1$
		result.set(Calendar.MILLISECOND, 0);
		int i = 0;
		try {
			result.set(Calendar.YEAR, Integer.parseInt(string.substring(0,4)));
			i = 5;
			result.set(Calendar.MONTH, Integer.parseInt(string.substring(5,7)) - 1);
			i = 8;
			result.set(Calendar.DAY_OF_MONTH, Integer.parseInt(string.substring(8,10)));
			i = 11;
			result.set(Calendar.HOUR_OF_DAY, Integer.parseInt(string.substring(11,13)));
			i = 14;
			result.set(Calendar.MINUTE, Integer.parseInt(string.substring(14,16)));
			i = 17;
			result.set(Calendar.SECOND, Integer.parseInt(string.substring(17,19)));
			if ((string.length() > 20) && (string.charAt(19) == ',')) {
				i = 20;
				result.set(Calendar.MILLISECOND, Integer.parseInt(string.substring(20,24)));
			}
		} catch (NumberFormatException e) {
			throw new ParseException(e.getMessage(), i);
		} catch (IndexOutOfBoundsException e) {
			throw new ParseException(e.getMessage(), i);
		}
		return result.getTime();
	}
	
	private String toString(Calendar calendar) {
		calendar.set(Calendar.ZONE_OFFSET, 0);
		return String.format("%04d-%02d-%02dT%02d:%02d:%02d,%04dZ", //$NON-NLS-1$
				calendar.get(Calendar.YEAR),
				calendar.get(Calendar.MONTH) + 1,
				calendar.get(Calendar.DAY_OF_MONTH),
				calendar.get(Calendar.HOUR_OF_DAY),
				calendar.get(Calendar.MINUTE),
				calendar.get(Calendar.SECOND),
				calendar.get(Calendar.MILLISECOND));
	}
	
	private int simpleType(String string) {
		if (string.isEmpty()) {
			return 0;
		}
		if ("true".equalsIgnoreCase(string)) { //$NON-NLS-1$
			return 1;
		}
		if ("false".equalsIgnoreCase(string)) { //$NON-NLS-1$
			return 1;
		}
		boolean flt = false;
		boolean it = true;
		char[] chars = string.toCharArray();
		int i = 0;
		if ((chars[0] == '-') || (chars[0] == '+')) {
			i++;
		}
		for(;i < chars.length; i++) {
			char c  = chars[i];
			if (c == '.') {
				if (flt) {
					it = false;
					break;
				} else {
					flt = true;
				}
			} else if ((c < '0') || ( c > '9')) {
				it = false;
				break;
			}
		}
		if (it) {
			if (flt) {
				return 3;
			}
			return 2;
		}
		if ((chars.length >= 19) && isDigit(chars[0]) && isDigit(chars[1]) && isDigit(chars[2]) && isDigit(chars[3]) && // year
				(chars[4] == '-') && isDigit(chars[5]) && isDigit(chars[6]) && // month
				(chars[7] == '-') && isDigit(chars[8]) && isDigit(chars[9]) && // day
				(chars[10] == 'T') && isDigit(chars[11]) && isDigit(chars[12]) && // hour
				(chars[13] == ':') && isDigit(chars[14]) && isDigit(chars[15]) && // minute
				(chars[16] == ':') && isDigit(chars[17]) && isDigit(chars[19])) { // seconds
			return 4;
		}
		return 0;
	}
	
	private boolean isDigit(char c) {
		return (c >= '0') || (c <= '9');
	}

	private String getFileValue(Object value) {
		if (value == null) {
			return null;
		}
		if (value instanceof Date) {
			Calendar cal = new GregorianCalendar(TimeZone.getTimeZone("GMT")); //$NON-NLS-1$
			cal.setTime((Date) value);
			return toString(cal);
		}
		if (value instanceof Calendar) {
			return toString((Calendar) value);
		}
		String s = value.toString().trim();
		if (s.isEmpty()) {
			return ""; //$NON-NLS-1$
		}
		if (s.charAt(s.length() - 1) == '\\') {
			s = s + '\\'; 
		}
		return s.replace("\r", "").replace("\n", "\\\n"); //$NON-NLS-1$
	}

	private String getToken(String key) {
		if ((key == null) || !isNotIgnored(key)) {
			return null;
		}
		String token = key.toString().trim().replace('=', ':');
		if (token.isEmpty()) {
			return null;
		}
		if (token.charAt(0) == '_') {
			return '_' + token;
		}
		if (token.charAt(0) == '[') {
			return '_' + token;
		}
		if (token.charAt(0) == '#') {
			return '_' + token;
		}
		return token;
	}

	private String getSection(String fpid, String pid) {
		if (pid == null) {
			return null;
		}
		pid = pid.trim();
		if (pid.isEmpty()) {
			return null;
		}
		Properties doc = pidSections.get(pid);
		if (doc != null) {
			String sec = doc.getProperty(DOC_PROP_SECTION);
			if (sec != null) {
				return "[ " + sec + " ]"; //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		StringBuilder sb = new StringBuilder("["); //$NON-NLS-1$
		if (fpid != null) {
			if (fpid.startsWith("com.arcadsoftware.server.restful")) { //$NON-NLS-1$
				sb.append(' ');
				sb.append("REST"); //$NON-NLS-1$
				if (fpid.length() > 32) {
					String[] ss = fpid.substring(33).split("\\."); //$NON-NLS-1$
					for(String s: ss) {
						sb.append(' ');
						if (!s.isEmpty()) {
							char[] c = s.toCharArray();
							sb.append(Character.toUpperCase(c[0]));
							for(int i = 1; i < c.length; i++) {
								sb.append(c[i]);
							}
						}
					}
				}
			} else if (fpid.startsWith("com.arcadsoftware.")) { //$NON-NLS-1$
				String[] ss = fpid.substring(18).split("\\."); //$NON-NLS-1$
				for(String s: ss) {
					sb.append(' ');
					if (!s.isEmpty()) {
						char[] c = s.toCharArray();
						sb.append(Character.toUpperCase(c[0]));
						for(int i = 1; i < c.length; i++) {
							sb.append(c[i]);
						}
					}
				}
			} else {
				sb.append(fpid);
			}
			sb.append(" / "); //$NON-NLS-1$
			int nb = 1;
			int i = pid.lastIndexOf('.');
			if (i > 0) {
				try {
					nb = Integer.parseInt(pid.substring(i + 1));
				} catch (NumberFormatException e) {
					Integer z = getFactoryCount(fpid);
					if (z != null) {
						nb = z.intValue() + 1;
						setFactoryCount(fpid, nb);
					}
				}
			}
			sb.append(nb);
		} else if (pid.startsWith("com.arcadsoftware.server.restful")) { //$NON-NLS-1$
			sb.append(' ');
			sb.append("REST"); //$NON-NLS-1$
			if (pid.length() > 32) {
				String[] ss = pid.substring(33).split("\\."); //$NON-NLS-1$
				for(String s: ss) {
					sb.append(' ');
					if (!s.isEmpty()) {
						char[] c = s.toCharArray();
						sb.append(Character.toUpperCase(c[0]));
						for(int i = 1; i < c.length; i++) {
							sb.append(c[i]);
						}
					}
				}
			}
			sb.append(' ');
		} else if (pid.startsWith("com.arcadsoftware.")) { //$NON-NLS-1$
			String[] ss = pid.substring(18).split("\\."); //$NON-NLS-1$
			for(String s: ss) {
				sb.append(' ');
				if (!s.isEmpty()) {
					char[] c = s.toCharArray();
					sb.append(Character.toUpperCase(c[0]));
					for(int i = 1; i < c.length; i++) {
						sb.append(c[i]);
					}
				}
			}
			sb.append(' ');
		} else {
			sb.append(pid);
		}
		sb.append(']');
		return sb.toString();
	}

	/**
	 * Log an Error Message.
	 * @param string
	 * @param e
	 */
	protected abstract void logError(String message, Throwable e);

	/**
	 * Create a new Configuration container.
	 * 
	 * @param factoryPid
	 * @param pid
	 * @return
	 */
	protected abstract Hashtable<String, Object> addConfiguration(String factoryPid, String pid);
	
	/**
	 * Get the current count value of Factory PID instance.
	 * 
	 * @param fpid the Factory PID.
	 * @return a non null integer.
	 */
	protected abstract Integer getFactoryCount(String fpid);
	
	/**
	 * Update the current count value of a factory instances.
	 * 
	 * @param fpid the Factory PID.
	 * @param value The new maximal used value of a factory PID.
	 */
	protected abstract void setFactoryCount(String fpid, Integer value);

	/**
	 * Get the Factory PID of the given configuration PID.
	 * 
	 * @param pid
	 * @return null if this PID is not a factory PID instance.
	 */
	protected abstract String getFactoryPid(String pid);
}
