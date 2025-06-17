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
package com.arcadsoftware.osgi;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InvalidClassException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamClass;
import java.io.Serializable;
import java.util.Base64;
import java.util.Date;
import java.util.Dictionary;
import java.util.HashMap;
import java.util.Timer;
import java.util.TimerTask;

import com.arcadsoftware.osgi.internal.Messages;

/**
 * FileSystemTracker check an associated external directory to track files change (addition, deletion and modifications). 
 * 
 * <p>
 * This class may by extended or associated to a callback objet to broadcast the modifications event.
 * 
 * <p>
 * A file map hold the last known modification date of the tracked files.
 * 
 * <p>
 * A File system tracker can be serialized into bundles properties (see {@link #saveToProperties(Dictionary)} and {@link #loadFromDictionary(Dictionary)}). 
 * In this cases the tracked file list will be persistent. If not serialised then the first check of a new basedir will throw adding events for every files
 * stored into the directory. 
 */
public class FileSystemTracker extends TimerTask {

	public static final String PROP_WATCHMAP = "fs.mapping"; //$NON-NLS-1$
	public static final String PROP_BASEDIR = "fs.basedir"; //$NON-NLS-1$
	public static final String PROP_PERIOD = "fs.period"; //$NON-NLS-1$
	public static final String PROP_EXTENSION = "fs.extension"; //$NON-NLS-1$

	/*
	 * Le test sur les dates de modification des fichier n'est pas suffisant car sur certain système
	 * ce test peut être contourné. On ajoute donc un test sur la taille du fichier.
	 */
	private class FileMark implements Serializable {
		
		private static final long serialVersionUID = 2355825707421001998L;

		private long len;
		private long date;
		
		public FileMark(File file) {
			len = file.length();
			date = file.lastModified();
		}
		
		@Override
		public boolean equals(Object value) {
			return (value instanceof FileMark) && //
				(((FileMark) value).len == len) && // possibilité de prendre en compte une approximation
				(((FileMark) value).date == date);
		}
		
		public Date getDate() {
			return new Date(date);
		}
		
		public long getLength() {
			return len;
		}
	}
	
	private final AbstractActivator activator;
	private final IFileSystemTrackerCallBack callbacks;
	private File baseDir;
	private String extension;
	private int delay;
	private Timer timer;
	private HashMap<String,FileMark> fileMap;
	
	/**
	 * Create a File system change tracker.
	 * 
	 * <p>
	 * This tracker is not initialized. 
	 * 
	 * @param activator The bundle parent activator.
	 */
	public FileSystemTracker(AbstractActivator activator) {
		super();
		this.activator = activator;
		callbacks = null;
		fileMap = new HashMap<String, FileMark>();
	}
	
	/**
	 * Create a File system change tracker.
	 * 
	 * <p>This tracker is ready to go.
	 * 
	 * @param activator The bundle parent activator.
	 * @param baseDir the External base directory to track.
	 * @param extension the file extension to track.
	 * @param delay the time in seconds between successive task executions.
	 * @param callbacks the callback object associated to events, can be null if you override the corresponding methods.
	 */
	public FileSystemTracker(AbstractActivator activator, File baseDir, String extension, int delay, IFileSystemTrackerCallBack callbacks) {
		super();
		this.activator = activator;
		this.extension = extension;
		this.baseDir = baseDir;
		this.callbacks = callbacks;
		fileMap = new HashMap<String, FileMark>();
		setDelay(delay);
	}

	/**
	 * Create a File system change tracker.
	 * 
	 * <p>This tracker is ready to go.
	 * 
	 * @param activator The bundle parent activator.
	 * @param baseDir the External base directory to track.
	 * @param extension the file extension to track.
	 * @param delay the time in seconds between successive task executions.
	 * @param callbacks the callback object associated to events, can be null if you override the corresponding methods.
	 */
	public FileSystemTracker(AbstractActivator activator, String baseDir, String extension, int delay, IFileSystemTrackerCallBack callbacks) {
		super();
		this.activator = activator;
		this.extension = extension;
		if ((baseDir != null) && (baseDir.length() > 0)) {
			this.baseDir = new File(baseDir);
		}
		this.callbacks = callbacks;
		fileMap = new HashMap<String, FileMark>();
		setDelay(delay);
	}

	public void close() {
		setDelay(0);
	}

	/**
	 * Load a persistent File system tracker from properties.
	 * This include delay, basedir and currently tracked files.
	 * 
	 * @param properties
	 */
	@SuppressWarnings("unchecked")
	public void loadFromDictionary(String prefix,Dictionary<String, Object> properties) {
		if (properties == null) {
			return;
		}
		Object o = properties.get(prefix + PROP_BASEDIR);
		if (o != null) {
			setBaseDir(new File(o.toString()));
		} else {
			setBaseDir(null);
		}
		setExtension((String)properties.get(prefix + PROP_EXTENSION));
		o = properties.get(prefix + PROP_WATCHMAP);
		if (o != null) {
			try {
				ByteArrayInputStream bis = new ByteArrayInputStream(Base64.getDecoder().decode(o.toString()));
				try {
					ObjectInputStream ois = new ObjectInputStream(bis) {
						@Override
						protected Class<?> resolveClass(ObjectStreamClass desc)
								throws IOException, ClassNotFoundException {
							if (!desc.getName().equals(HashMap.class.getName())) {
								throw new InvalidClassException("Unauthorized deserialization attempt", desc.getName()); //$NON-NLS-1$
							}
							return super.resolveClass(desc);
						}
					};
					try {
						fileMap = (HashMap<String, FileMark>) ois.readObject();
					} catch (ClassNotFoundException e) {
						activator.error(e.getLocalizedMessage(), e);
					} finally {
						ois.close();
					}
				} catch (IOException e) {
					activator.error(e.getLocalizedMessage(), e);
				}
			} catch (IllegalArgumentException e) {
				activator.error(e.getLocalizedMessage(),e);
			}
		}
		o = properties.get(prefix + PROP_PERIOD);
		if (o != null) {
			try {
				setDelay(Integer.parseInt(o.toString()));
			} catch (NumberFormatException e) {
				setDelay(0);
			}
		} else {
			setDelay(0);
		}
	}
	
	public void saveToProperties(String prefix, Dictionary<String,Object> properties) {
		if (baseDir == null) {
			properties.remove(prefix + PROP_BASEDIR);
		} else {
			properties.put(prefix + PROP_BASEDIR, baseDir.getAbsolutePath());
		}
		if (extension == null) {
			properties.remove(prefix + PROP_EXTENSION);
		} else {
			properties.put(prefix + PROP_EXTENSION, extension);
		}
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		try {
			ObjectOutputStream oos = new ObjectOutputStream(bos);
			try {
				oos.writeObject(fileMap);
				oos.flush();
			} finally {
				oos.close();
			}
		} catch (IOException e) {
			activator.error(e.getLocalizedMessage(), e);
		}
		properties.put(prefix + PROP_WATCHMAP, Base64.getEncoder().encodeToString(bos.toByteArray()));
		properties.put(prefix + PROP_PERIOD, delay);
	}
	
	public AbstractActivator getActivator() {
		return activator;
	}
	
	public File getBaseDir() {
		return baseDir;
	}

	/**
	 * Setting a new Base Directory will reset the map and throw removes event for all previously added items...
	 * @param baseDir the new Base directory.
	 */
	public void setBaseDir(File baseDir) {
		if ((baseDir == null) && ((this.baseDir == null) || this.baseDir.equals(baseDir))) {
			return;
		}
		for (String name:fileMap.keySet()) {
			removeFile(name, new File(baseDir,name));
		}
		fileMap = new HashMap<String, FileMark>();
		this.baseDir = baseDir;
	}
	
	/**
	 * Define the time in seconds between successive task executions.
	 * 
	 * <p>null or negative delay will stop the tracker.
	 * 
	 * @param delay the delay to set
	 */
	public void setDelay(int delay) {
		if (this.delay != delay) {
			this.delay = delay;
			synchronized (this) {
				if (timer != null) {
					try {
						timer.cancel();
						timer = null;
					} catch (Throwable e) {
						activator.debug(e);
					}
				}
				if (delay > 0) {
					timer = new Timer("File System Tracker"); //$NON-NLS-1$
					timer.schedule(this, delay * 1000, delay * 1000);
				}
			}
		}
	}

	/**
	 * @return the time in seconds between successive task executions.
	 */
	public int getDelay() {
		return delay;
	}

	@Override
	public void run() {
		activator.debug(Messages.getString("osgi.FSStarting")); //$NON-NLS-1$
		HashMap<String, FileMark> current = new HashMap<>(fileMap);
		if (baseDir != null) {
			checkDirectory(baseDir, "", current); //$NON-NLS-1$
		}
		// Process to deletions.
		for (String name: current.keySet()) {
			removeFile(name, new File(name));
			fileMap.remove(name);
		}
	}

	private void checkDirectory(File dir, String baseDirName, HashMap<String, FileMark> current) {
		if (dir.isDirectory()) {
			for (String name: dir.list()) {
				File file = new File(dir,name);
				String localName = baseDirName + name;
				if (file.isDirectory()) {
					if (!(".".equals(name) || "..".equals(name))) { //$NON-NLS-1$ //$NON-NLS-2$
						checkDirectory(file, localName, current);
						// Récursivité terminale (possibilité de transformation en boucle).
					}
				} else if (file.isFile() && ((extension == null) || localName.toLowerCase().endsWith(extension))) {
					FileMark cm = current.remove(localName);
					FileMark m = new FileMark(file);
					if (cm == null) {
						if (addFile(localName,file)) {
							fileMap.put(localName, m);
						}
					} else if (!cm.equals(m)) {
						updateFile(localName, file);
						fileMap.put(localName, m);
					}
				}
			}
		}
	}

	public Date getFileDate(String localFileName) {
		FileMark m =  fileMap.get(localFileName);
		if (m == null) {
			return null;
		}
		return m.getDate();
	}

	public long getFileLength(String localFileName) {
		FileMark m =  fileMap.get(localFileName);
		if (m == null) {
			return 0L;
		}
		return m.getLength();
	}
	
	protected boolean addFile(String name, File file) {
		if (callbacks != null) {
			return callbacks.addFile(this, name, file);
		}
		return false;
	}

	protected void updateFile(String name, File file) {
		if (callbacks != null) {
			callbacks.updateFile(this, name, file);
		}
	}

	protected void removeFile(String name, File file) {
		if (callbacks != null) {
			callbacks.removeFile(this, name, file);
		}
	}

	/**
	 * Setting a new file extension will reset the map and throw removes event for all previously added items...
	 * <p>Null string will track all files.
	 * @param extension the new file extension to check.
	 */
	public void setExtension(String extension) {
		if ((extension != null) && (extension.length() == 0)) {
			extension = null;
		} else {
			extension = extension.toLowerCase();
		}
		if ((extension == null) && ((this.extension == null) || this.extension.equals(extension))) {
			return;
		}
		for (String name:fileMap.keySet()) {
			removeFile(name, new File(baseDir,name));
		}
		fileMap = new HashMap<String, FileMark>();
		this.extension = extension;
	}

	/**
	 * @return the file extension to check.
	 */
	public String getExtension() {
		return extension;
	}
}