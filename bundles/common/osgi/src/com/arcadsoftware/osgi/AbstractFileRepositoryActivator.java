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
package com.arcadsoftware.osgi;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Dictionary;
import java.util.Enumeration;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import com.arcadsoftware.osgi.internal.Messages;

/**
 * This class implement an abstract Activator linked to a file repository (i.e. file system storage). This repository can be :
 * 
 * <ul>
 * <li><b>Localized</b>: support language code prefixes.
 * <li><b>Internal</b> and/or <b>external</b> to the bundler Jar.
 * <li>Dedicated to a specific <b>file extension</b>.
 * <li>The external directory can be changed to any accessible <b>remote directory</b> (NFS or SAMBA).
 * </ul>
 * 
 * <p>By default the external directory is localized into a relative directory to the server base directory.
 */
public abstract class AbstractFileRepositoryActivator extends AbstractConfiguredActivator implements IFileSystemTrackerCallBack {

	// The property name to store the external Path
	protected static final String PROP_BASEDIR = "basedir"; //$NON-NLS-1$
	protected static final String PROP_WATCHPERIOD = "watchdir"; //$NON-NLS-1$

	private String baseDir;
	private String internalDir;
	private FileSystemTracker fileTracker;
	
	@Override
	public void start(BundleContext context) throws Exception {
		baseDir = "./" + getDefaultDirName(); //$NON-NLS-1$
		if (internalDir == null) {
			internalDir = "/" + getDefaultDirName(); //$NON-NLS-1$
		}
		super.start(context);
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		if (fileTracker != null) {
			fileTracker.close();
			fileTracker = null;
		}
	}

	@Override
	public void updatedConfiguration(Dictionary<String,Object> properties) {
		if (properties != null) {
			Object o = properties.get(PROP_WATCHPERIOD);
			int d = 0;
			if (o != null) {
				try {
					d = Integer.parseInt(o.toString());
					if (d < 0) {
						d = 0;
					}
				} catch (NumberFormatException e) {}
			}
			if (d != 0) {
				setTrackingDelay(d);
			} else if (fileTracker != null) {
				setTrackingDelay(0);
			}
			o = properties.get(PROP_BASEDIR);
			if ((o instanceof String) && (!((String)o).equals(baseDir))) {
				baseDir = (String) o;
				if ((baseDir == null) || (baseDir.length() == 0)) {
					baseDir = "./" + getDefaultDirName(); //$NON-NLS-1$
				}
				if (fileTracker != null) {
					fileTracker.setBaseDir(new File(baseDir));
				}
				debug(Messages.getString("osgi.DebugDirectoryChanged") + baseDir); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Set a specific internal directory where to search files.
	 * 
	 * <p>
	 * If null then only external files will be available.
	 * 
	 * @param directory A folder name relative to the Bundle root.
	 */
	protected void setInternalDirectory(String directory) {
		internalDir = directory;
		if (internalDir != null) {
			if (internalDir.length() == 0)  {
				internalDir = null;
			} else if ((internalDir.charAt(0) != '/') && (internalDir.charAt(0) != '\\')) {
				internalDir = "/" + internalDir; //$NON-NLS-1$
			}
		}
	}
	/**
	 * Get the internal folder name used to store default files.
	 * @return
	 */
	protected String getInternalDirectory() {
		return internalDir;
	}
	
	/**
	 * Define the period used to check the file system state and throw the
	 * methods <code>addFile</code>, <code>updateFile</code> or <code>removeFile</code>. 
	 * 
	 * @param delay the tracking delay, in seconds.
	 */
	private void setTrackingDelay(int delay) {
		if (fileTracker == null) {
			fileTracker = new FileSystemTracker(this, baseDir, getFileExtension(), delay, this);
		} else {
			fileTracker.setDelay(delay);
		}
	}

	/**
	 * @return The period used to check the file system state.
	 */
	protected int getTrackingDelay() {
		if (fileTracker == null) {
			return 0;
		}
		return fileTracker.getDelay();
	}
	
	/**
	 * Return a File into the repository corresponding to the given key. 
	 * 
	 * <p> The key is the local file name. It must correspond to it locally to
	 *  the repository base directory. It can be in a sub-directory. 
	 *  It nust not use a postfix (e.g. "_en") nor file extension (e.g. ".xml").
	 * 
	 * @param filekey The base file name into the repository.
	 * @param langcode the desired language code (e.g. "en", "fr")
	 * @param fileLastModification return the real file last date of modification (depending on the location of the file).
	 * @return
	 */
	public File getFile(String filekey, String langcode, Calendar fileLastModification) {
		return getFile(getFileName(filekey, langcode), fileLastModification);
	}
	
	/**
	 * Return a File into the repository corresponding to the given filename. 
	 * 
	 * <p>This method first check if the file does not exist into the external repository. 
	 * And if not, check the internal one. Then modify the given Calendar to the 
	 * last change of the file, witch is :
	 * 
	 * <ul>
	 * <li>for an external file: the file system time stamp.
	 * <li>for an internal file: the last modification of the bundle.
	 * </ul>
	 * 
	 * @param filename a file patch into the file repository.
	 * @param fileLastModification return the last modification date (if the file exist).
	 * @return null if the file does not exist into the external and the internal repository.
	 */
	public File getFile(String filename, Calendar fileLastModification) {
		File file = getExternalFile(filename);
		if (file.isFile()) {
			// Return an external file...
			if (fileLastModification != null) {
				fileLastModification.setTime(new Date(file.lastModified()));
			}
			return file;
		}
		if (internalDir != null) {
			return getInternalFile(filename, fileLastModification);
		}
		return null;
	}

	
	/**
	 * Return a File into the repository corresponding to the given filename. 
	 * 
	 * @param filename a file patch into the file repository.
	 * @return null if the file does not exist.
	 * @see com.arcadsoftware.osgi.AbstractFileRepositoryActivator#getFile(String,java.util.Calendar)
	 */
	public File getInternalFile(String filename) {
		return getInternalFile(filename, null);
	}

	/**
	 * Return the internal repository version of the given file.
	 * 
	 * @param filekey The base file name into the repository.
	 * @param langcode the desired language code (e.g. "en", "fr")
	 * @param fileLastModification return the last modification date (if the file exist).
	 * @return null if the file does not exist into the internal repository.
	 */
	public File getInternalFile(String filekey, String langcode, Calendar fileLastModification) {
		return getInternalFile(getFileName(filekey, langcode),fileLastModification);
	}
	
	/**
	 * Return the internal repository version of the given file.
	 * 
	 * @param filename a file patch into the file repository.
	 * @param fileLastModification return the last modification date (if the file exist).
	 * @return null if the file does not exist into the internal repository.
	 */
	public File getInternalFile(String filename, Calendar fileLastModification) {
		if (internalDir == null) {
			return null;
		}
		return getbundleFile(getContext().getBundle(), internalDir + filename, fileLastModification);
	}

	/**
	 * Get the file from the given bundle.
	 * 
	 * @param bundle
	 * @param filename
	 * @param fileLastModification
	 * @return
	 */
	protected File getbundleFile(Bundle bundle, String filepath, Calendar fileLastModification) {
		// Extract the resource from the bundle jar:
		URL url = bundle.getEntry(filepath);
		if (url == null) {
			url = bundle.getResource(filepath);
			if (url != null) {
				debug("Get Internal file (No entry) = " + url.toString()); //$NON-NLS-1$
			} else {
				debug("Get Internal file (not found) = " + filepath); //$NON-NLS-1$
			}
		} else {
			debug("Get Internal file = " + url.toString()); //$NON-NLS-1$
		}
		if (url != null) {
			File file = getInternalFile(url);
			if (file.isFile()) {
				// Return an internal file...
				// Try to guess that we are in a development environment.
				if (fileLastModification != null) {
					if (getContext().getBundle().getEntry("/.classpath") != null) { //$NON-NLS-1$
						fileLastModification.setTime(new Date(file.lastModified()));
					} else {
						fileLastModification.setTime(new Date(getContext().getBundle().getLastModified()));
					}
				}
				return file;
			}
		}
		return null;
	}

	/**
	 * Return internal file form an URL.
	 * @param url
	 * @return
	 */
	protected File getInternalFile(URL url) {
		return toFile(url);
	}
	
	/**
	 * Create a new empty File instance into the external repository.
	 *  
	 * <p>If the file already exist on the disc then it is deleted first.
	 * 
	 * @param filekey
	 * @return The newly created file.
	 */
	public File getNewFile(String filekey) {
		return getNewFile(filekey, null);
	}
	
	/**
	 * Create a new empty File instance into the external repository.
	 *  
	 * <p>If the file already exist on the disc then it is deleted first.
	 * 
	 * @param filekey
	 * @param langcode the desired language code (e.g. "en", "fr")
	 * @return The newly created file.
	 */
	public File getNewFile(String filekey, String langcode) {
		File file = getExternalFile(filekey, langcode);
		if (file.exists()) {
			if (!file.delete()) {
				info("Unable to delete file: " + file.getAbsolutePath());
			}
		}
		file.getParentFile().mkdirs();
		try {
			file.createNewFile();
		} catch (IOException e) {
			error(Messages.getString("osgi.CreateFileError"), e); //$NON-NLS-1$
		}
		return file;
	}
	
	/**
	 * Return the external file. This file may not exist.
	 * 
	 * @param filekey
	 * @param langcode
	 * @return
	 */
	public File getExternalFile(String filekey, String langcode) {
		return getExternalFile(getFileName(filekey, langcode));
	}
	
	/**
	 * Return the external file. This file may not exist.
	 * 
	 * @param fileName
	 * @return
	 */
	public File getExternalFile(String filename) {
		return new File(baseDir + filename);
	}

	/**
	 * Build a file name from an optional language code and the default extension.
	 * @param filekey
	 * @param langcode
	 * @return
	 */
	public String getFileName(String filekey, String langcode) {
		String filename;
		if (filekey.charAt(0) == '/') {
			filename = filekey;
		} else {
			filename = "/" + filekey; //$NON-NLS-1$
		}
		if ((langcode != null) && (langcode.length() > 0)) {
			filename += "_" + langcode; //$NON-NLS-1$
		}
		String ext = getFileExtension();
		if (ext != null) {
			if (filename.endsWith(ext)) {
				return filename;
			}
			return filename + ext;
		}
		return filename;
	}

	/**
	 * Return a list of filename relative to the the repository root. 
	 *  
	 * @param foldername the repository relative 
	 * @param useExtension if true append the repository file extension to the search pattern. If used the extension will be removed from the filename 
	 * @return local filename (relative to the repository root).
	 */
	public String[] getFileList(String foldername, boolean useExtension) {
		return getFileList(foldername, useExtension, false);
	}
	
	/**
	 * Return a list of filename relative to the the repository root. 
	 *  
	 * @param foldername the repository relative 
	 * @param useExtension if true append the repository file extension to the search pattern. If used the extension will be removed from the filename
	 * @param recurse if true go trough sub directories. 
	 * @return local filename (relative to the repository root).
	 */
	public String[] getFileList(String foldername, boolean useExtension, boolean recurse) {
		if (foldername == null) {
			foldername = "/"; //$NON-NLS-1$
		} else if (!foldername.endsWith("/")) { //$NON-NLS-1$
			foldername += "/"; //$NON-NLS-1$
		}
		if (!foldername.startsWith("/")) { //$NON-NLS-1$
			foldername = "/" + foldername; //$NON-NLS-1$
		}
		ArrayList<String> files = new ArrayList<String>();
		filesInternal(foldername,useExtension,recurse,files);
		filesExternal(foldername,useExtension,recurse,files);
		return files.toArray(new String[files.size()]);
	}
	
	private void filesExternal(String foldername, boolean useExtension, boolean recurse, ArrayList<String> files) {
		File folder = getExternalFile(foldername);
		String[] extfiles = folder.list();
		if (extfiles != null) {
			for (String name:extfiles) {
				File file = new File(folder.getAbsolutePath() + '/' + name);
				if (file.isFile()) {
					if (!useExtension) {
						name = foldername + name;
						if (!files.contains(name)) {
							files.add(name);
						}
					} else if (name.endsWith(getFileExtension())) {
						name = foldername + name.substring(0, name.length() - getFileExtension().length());
						if (!files.contains(name)) {
							files.add(name);
						}
					}
				} if (file.isDirectory() && recurse) {
					filesExternal(foldername + name + '/', useExtension, recurse, files);
				}
			}
		}
	}

	private void filesInternal(String foldername, boolean useExtension, boolean recurse, ArrayList<String> files) {
		addBundleFiles(getContext().getBundle(), foldername, useExtension, recurse, files);
	}

	/**
	 * Add the files from the given bundle to the list of files.
	 * 
	 * @param bundle
	 * @param foldername
	 * @param useExtension
	 * @param recurse
	 * @param files
	 */
	protected void addBundleFiles(Bundle bundle, String foldername, boolean useExtension, boolean recurse, ArrayList<String> files) {
		if (internalDir == null) {
			return;
		}
		if (foldername == null) {
			foldername = "/"; //$NON-NLS-1$
		}
		if (internalDir.endsWith("/")) { //$NON-NLS-1$
			if (foldername.startsWith("/")) { //$NON-NLS-1$
				if (foldername.length() == 1) {
					foldername = ""; //$NON-NLS-1$
				} else {
					foldername = foldername.substring(1);
				}
			}
		} if (!foldername.startsWith("/")) { //$NON-NLS-1$
			foldername = '/' + foldername;
		}
		Enumeration<URL> e = bundle.findEntries(internalDir + foldername, null, false);
		if (e == null) {
			return;
		}
		String lcest = getFileExtension().toLowerCase();
		while (e.hasMoreElements()) {
			File file = getInternalFile(e.nextElement());
			if (file.isFile()) {
				String name = file.getName();
				if (!useExtension) {
					if (!files.contains(name)) {
						files.add(foldername + name);
					}
				} else if (name.toLowerCase().endsWith(lcest)) {
					if (!files.contains(name)) {
						files.add(foldername + name.substring(0, name.length() - getFileExtension().length()));
					}
				}
			} else if (file.isDirectory() && recurse) {
				addBundleFiles(bundle, foldername + file.getName() + '/', useExtension, recurse, files);
			}
		}
	}

	/**
	 * Extender must provide the default directory name.
	 * This directory can also be an bundle internal directory so that the default
	 * files will be looked for into the bundle if they do not exist in the external directory.
	 * 
	 * <p>This directory name must be a relative one and should not change because the default path
	 * is initialized into the <code>start</code> method.</p>
	 * 
	 * @return the default and internal directory name.
	 */
	protected abstract String getDefaultDirName();

	/**
	 * Extender can define the file extension, if not, the <code>filekey</code> will containt
	 * the file extension and language specification will be useless.
	 * 
	 * @return a file extension (e.g. ".xml").
	 */
	protected abstract String getFileExtension();

	@Override
	public boolean addFile(FileSystemTracker tracker, String name, File file) {
		return true;
	}

	@Override
	public void updateFile(FileSystemTracker tracker, String name, File file) {
	}

	@Override
	public void removeFile(FileSystemTracker tracker, String name, File file) {
	}
	
}