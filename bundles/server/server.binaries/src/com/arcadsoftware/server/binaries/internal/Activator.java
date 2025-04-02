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
package com.arcadsoftware.server.binaries.internal;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.util.Date;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Timer;
import java.util.TimerTask;

import org.osgi.framework.BundleContext;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;
import org.osgi.util.tracker.ServiceTracker;

import com.arcadsoftware.osgi.AbstractConfiguredActivator;
import com.arcadsoftware.osgi.IBinariesTranferService;

public class Activator extends AbstractConfiguredActivator {

	private static final String FILE_EVENT_TOPIC = "com/arcadsoftware/binaries/file"; //$NON-NLS-1$
	private static final String FILE_EVENT_TYPE = "type"; //$NON-NLS-1$
	private static final String FILE_EVENT_TYPE_DELETE = "delete"; //$NON-NLS-1$
	private static final String FILE_EVENT_TYPE_CHANGE = "change"; //$NON-NLS-1$
	private static final String FILE_EVENT_FILE = "file"; //$NON-NLS-1$
	private static final String FILE_EVENT_FILENAME = "filename"; //$NON-NLS-1$
	private static final String FILE_EVENT_CATEGORY = "category"; //$NON-NLS-1$
	private static final String FILE_EVENT_ID = "id"; //$NON-NLS-1$
	private static final String CONF_KEYLIMITDATE = "limit"; //$NON-NLS-1$
	private static final String CONF_KEYPATH = "path"; //$NON-NLS-1$
	private static final String CONF_KEYSERVER = "server.address"; //$NON-NLS-1$
	private static final String CONF_KEYMAXSIZE = "maxFileSize"; //$NON-NLS-1$
	private static final int CONF_KEYMAXSIZE_DEFAULT = 10240000; 
	private static final String DEFAULTPATH = "./files/bin"; //$NON-NLS-1$

	private static Activator instance;
	
	public static Activator getInstance() {
		return instance;
	}
	
	private Hashtable<String, BinariesKey> cache = new Hashtable<String, BinariesKey>();
	private int timeKeyDuration = 9000000; //150 minutes
	private Timer timer;
	private String serverAdress = "http://localhost"; //$NON-NLS-1$
	private File path = new File(DEFAULTPATH);
	@SuppressWarnings("rawtypes")
	private ServiceTracker eventTracker;
	private BinariesTranferService binTransfer;
	private int maxFileSize = CONF_KEYMAXSIZE_DEFAULT; //10 Mo
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		instance = this;
		eventTracker = new ServiceTracker(context, EventAdmin.class.getName(), null);
		eventTracker.open();
		binTransfer = new BinariesTranferService(this);
		registerService(IBinariesTranferService.clazz, binTransfer);
		registerService(Branch.clazz, new Branch(), Branch.properties(Branch.ROOTBRANCH));
		registerService(BranchSecure.clazz, new BranchSecure(), BranchSecure.properties(BranchSecure.SECUREDBRANCH));
		updateTimer();
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		if (timer != null) {
			timer.cancel();
			timer = null;
		}
		if (eventTracker != null) {
			eventTracker.close();
			eventTracker = null;
		}
		super.stop(context);
		cache.clear();
		instance = null;
	}

	private void updateTimer() {
		if (timer != null) {
			timer.cancel();
		}
		timer = new Timer("Binaries Key cache Purge");
		timer.schedule(new TimerTask() {
			@Override
			public void run() {
				long t = System.currentTimeMillis();
				Date now = new Date();
				// Cette méthode limite l'expension de la table mais peut provoquer un GC au moment de son
				// exécution (par défaut toute les 5 heures).
				synchronized (cache) {
					Enumeration<BinariesKey> keys = cache.elements();
					cache = new Hashtable<String, BinariesKey>();
					while (keys.hasMoreElements()) {
						BinariesKey key = keys.nextElement();
						if (key.getLimit().after(now)) {
							cache.put(key.getKey(),key);
						}
					}
				}
				t = (System.currentTimeMillis() - t) / 1000;
				info(Messages.Activator_Purge_duration + t + Messages.Activator_Purge_duration_end);
			}
		}, timeKeyDuration, timeKeyDuration * 20);
	}
	
	@Override
	public void updatedConfiguration(@SuppressWarnings("rawtypes") Dictionary properties) {
		if (properties != null) {
			Object o = properties.get(CONF_KEYLIMITDATE);
			if (o instanceof Integer) {
				timeKeyDuration = ((Integer) o).intValue();
			} else if (o != null) {
				try {
					timeKeyDuration = Integer.parseInt(o.toString());
				} catch (NumberFormatException e) {
					error(Messages.Activator_Error_invalid_configuration, e);
				}
			}
			o = properties.get(CONF_KEYSERVER);
			if (o != null) {
				serverAdress = o.toString();
				if (serverAdress.endsWith("/")) { //$NON-NLS-1$
					serverAdress = serverAdress.substring(0, serverAdress.length() - 1);
				}
			}
			o = properties.get(CONF_KEYPATH);
			if (o != null) {
				String p = o.toString().trim();
				if (p.length() == 0) {
					p = DEFAULTPATH;
				} else if ((p.endsWith("/")) || (p.endsWith("\\"))) { //$NON-NLS-1$ //$NON-NLS-2$
					p = p.substring(0, p.length() - 1);
				}
				path = new File(p);
			} else {
				path = new File(DEFAULTPATH);
			}
			o = properties.get(CONF_KEYMAXSIZE);
			if (o != null) {
				if (o instanceof Integer){
					setMaxFileSize(((Integer)o).intValue());
				} else {
					try {
						setMaxFileSize(Integer.parseInt(o.toString()));
					} catch (NumberFormatException e) {
						setMaxFileSize(CONF_KEYMAXSIZE_DEFAULT);
						error(Messages.Activator_Error_invalid_configuration, e);
					}
				}
			}
		}
	}

	/**
	 * Store the key.
	 * 
	 * Threadsafe.
	 */
	protected String store(String category, int id, boolean readonly) {
		BinariesKey key = new BinariesKey(getCurrentLimit(), id, category, readonly);
		cache.put(key.getKey(), key);
		return serverAdress  + "/bin/" + key.getKey(); //$NON-NLS-1$
	}
	
	/**
	 * Return the Binary file key if its exist in the current valid keys.
	 */
	protected BinariesKey find(String key) {
		BinariesKey result = cache.get(key);
		if ((result == null) || result.getLimit().after(new Date())) {
			return result;
		}
		cache.remove(key);
		return null;
	}
	
	protected Date getCurrentLimit() {
		return new Date(System.currentTimeMillis() + timeKeyDuration);
	}
	
	protected synchronized int getNewFileId(String category) {
		// We should be sure that the configuration is already loaded when this method is called.
		// (If not we may create a false file which may lead to a security breach.)
		if (!isConfigAdminStarted()) {
			return -1;
		}
		// Check accessibility of the repository folder.
		final File dir = getDir(category);
		if (!dir.isDirectory()) {
			dir.mkdirs();
			if (!dir.isDirectory()) {
				throw new RuntimeException("The Binaries File Transfert Service does not have write access to: " + path.getAbsolutePath());
			}
		}
		File counter = new File(dir, ".counter");
		boolean newCounter = false;
		if (!counter.exists()) {
			try {
				if (counter.createNewFile()) {
					newCounter = true;
				}
			} catch (IOException e) {
				if (!counter.exists()) {
					error("The Binaries File Transfert Service can not create files into: " + path.getAbsolutePath(), e);
					return -1;
				}				
			}
		}
		try (RandomAccessFile file = new RandomAccessFile(counter, "rw")) { //$NON-NLS-1$
			try (FileChannel fc = file.getChannel()) {
				FileLock lock = getLock(fc);
				if (lock == null) {
					return -1;
				}
				try {
					long size = fc.size();
					if (size > 128l) {
						throw new IOException(String.format("Invalid %s/.counter file size.", category));
					}
					ByteBuffer buf = ByteBuffer.allocate((int) size);
					int c = 0;
					if (fc.read(buf) > 0) {
						// convert to int...
						c = buf.getInt(0);
					} else if (newCounter) {
						File dirMax = null;
						int dm = -1;
						for (File f: dir.listFiles()) {
							if (f.isDirectory()) {
								String name = f.getName().toLowerCase();
								if (name.startsWith("dir")) {
									try {
										int i = Integer.parseInt(name.substring(3));
										if (i > dm) {
											dm = i;
											dirMax = f;
										}
									} catch (NumberFormatException e) {}
								}
							}
						}
						if (dirMax != null) {
							c = dm * 1000;
							for (File f: dir.listFiles()) {
								if (f.isFile()) {
									String name = f.getName();
									int i = name.indexOf('_');
									if (i > 0) {
										try  {
											i = Integer.parseInt(name.substring(0, i));
											if (i > c) {
												c = i;
											}
										} catch (NumberFormatException e) {}
									}
								}
							}
						}
					}
					c++;
					// write back the new value...
					fc.position(0);
					fc.truncate(Integer.BYTES);
					buf = ByteBuffer.allocate(Integer.BYTES);
					buf.putInt(c);
					buf.flip();
					while (buf.hasRemaining()) {
						fc.write(buf);
					}
					fc.force(true);
					return c;
				} finally {
					lock.close();
				}
			}
		} catch (IOException e1) {
			error(e1);
			return -1;
		}
	}
	
	private FileLock getLock(FileChannel channel) throws IOException {
		FileLock lock = channel.tryLock();
		if (lock == null) {
			try {
				// Wait up to 5 seconds...
				for (int i = 1; i < 10; i++) {
					Thread.sleep(500);
					lock = channel.tryLock();
					if (lock != null) {
						break;
					}
				}
			} catch (InterruptedException e) {
				throw new IOException("Interrupted lock acquisition.", e);
			}
		}
		return lock;
	}
	
	protected File getPath() {
		return path;
	}
	
	protected File getDir(String category) {
		return new File(path, category.replace('.', '_'));
	}

	protected File getSubDir(String category, int id) {
		return new File(getDir(category), "dir" + Integer.toString(id / 1000)); //$NON-NLS-1$
	}

	private void fireEvent(Event event) {
		if (eventTracker != null) {
			EventAdmin ea = (EventAdmin) eventTracker.getService();
			if (ea != null) {
				ea.postEvent(event);
			} else {
				info("No Event Admin service available to send Binaries Transfer Service events.");
			}
		}
	}

	private Event newEvent(String type, String category, int id, File file) {
		HashMap<String, Object> props = new HashMap<>(5);
		props.put(FILE_EVENT_TYPE, type);
		if (file != null) {
			props.put(FILE_EVENT_FILENAME, file.getAbsolutePath());
			props.put(FILE_EVENT_FILE, file);
		}
		props.put(FILE_EVENT_CATEGORY, category);
		props.put(FILE_EVENT_ID, id);
		return new Event(FILE_EVENT_TOPIC, props);
	}
	
	/**
	 * Post an event after deleting a File.
	 */
	protected void fileEventDel(String category, int id, File file) {
		fireEvent(newEvent(FILE_EVENT_TYPE_DELETE, category, id, file));
	}

	/**
	 * Post an event after creating of modifying a File.
	 */
	protected void fileEventNew(String category, int id, File file) {
		fireEvent(newEvent(FILE_EVENT_TYPE_CHANGE, category, id, file));
	}

	/**
	 * Look up for the corresponding file into the repository.
	 */
	protected File getFile(String category, int id) {
		final String sid = Integer.toString(id) + '_';
		File parent = getSubDir(category, id);
		if ((parent != null) && parent.isDirectory()) {
			for (File file: parent.listFiles(new FilenameFilter() {
				public boolean accept(File dir, String name) {
					return name.startsWith(sid);
				}
			})) {
				if (file.isFile()) {
					return file;
				}
			}
		}
		debug("No file found for: ", category, '/', id);
		return null;
	}

	protected boolean removeFiles(String category, int id) {
		final String sid = Integer.toString(id) + '_';
		final File parent = getSubDir(category, id);
		boolean deleted = false;
		if ((parent != null) && parent.isDirectory()) {
			for (File file: parent.listFiles(new FilenameFilter() {
				public boolean accept(File dir, String name) {
					return name.startsWith(sid);
				}
			})) {
				if (file.isFile()) {
					if (file.delete()) {
						fileEventDel(category, id, file);
						deleted = true;
					} else {
						debug("Unable to delete file: " + file.getAbsolutePath());
					}
				}
			}
		}
		return deleted;
	}
	
	protected BinariesTranferService getBinTransfer() {
		return binTransfer;
	}

	protected int getMaxFileSize() {
		return maxFileSize;
	}

	private void setMaxFileSize(int maxFileSize) {
		this.maxFileSize = maxFileSize;
	}
	
}
