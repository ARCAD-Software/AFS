/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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
import java.util.Date;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Properties;
import java.util.Timer;
import java.util.TimerTask;
import java.util.Map.Entry;

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
	private static final String CONF_KEYCATEGORY = "category."; //$NON-NLS-1$
	private static final String CONF_KEYMAXSIZE = "maxFileSize"; //$NON-NLS-1$
	private static final int CONF_KEYMAXSIZE_DEFAULT = 10240000; 
	private static final String DEFAULTPATH = "./files/bin/"; //$NON-NLS-1$
		
	private final HashMap<String, Integer> categories = new HashMap<String, Integer>();
	private Hashtable<String, BinariesKey> cache = new Hashtable<String, BinariesKey>();
	private int timeKeyDuration = 9000000; //150 minutes
	private Timer timer;
	private String serverAdress = "http://localhost"; //$NON-NLS-1$
	private File path = new File(DEFAULTPATH);
	private boolean categoriesUpdated;
	@SuppressWarnings("rawtypes")
	private ServiceTracker eventTracker;
	private BinariesTranferService binTransfer;
	
	private int maxFileSize = CONF_KEYMAXSIZE_DEFAULT; //10 Mo

	private static Activator instance;
	
	public static Activator getInstance() {
		return instance;
	}
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		instance = this;
		eventTracker = new ServiceTracker(context,EventAdmin.class.getName(),null);
		eventTracker.open();
		categoriesUpdated = false;
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
				timeKeyDuration = ((Integer)o).intValue();
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
			Enumeration<?> keys = properties.keys();
			while (keys.hasMoreElements()) {
				String key = (String) keys.nextElement();
				if (key.startsWith(CONF_KEYCATEGORY)) {
					Object value = properties.get(key);
					String category = key.substring(CONF_KEYCATEGORY.length()); 
					Integer ivalue = null;
					if (value instanceof Integer) {
						ivalue = (Integer) value;
					} else if (value instanceof String) {
						try {
							ivalue = Integer.parseInt((String) value);
						} catch (NumberFormatException e) {}
					}
					if (ivalue != null) {
						synchronized(categories) {
							Integer oldvalue = categories.get(category); 
							if ((oldvalue == null) || (oldvalue < ivalue)) {
								categories.put(category, ivalue);
							}
						}
					}
				}
			}
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean finalizeConfiguration(@SuppressWarnings("rawtypes") Dictionary properties) {
		if (categoriesUpdated) {
			synchronized (categories) {
				for(Entry<String, Integer> entry:categories.entrySet()) {
					properties.put(CONF_KEYCATEGORY + entry.getKey(), entry.getValue());
				}
			}
			categoriesUpdated = false;
			return true;
		}
		return false;
	}

	/**
	 * Store the key.
	 * 
	 * Threadsafe.
	 */
	protected String store(String category, int id, boolean readonly) {
		BinariesKey key = new BinariesKey(getCurrentLimit(),id,category, readonly);
		cache.put(key.getKey(), key);
		return serverAdress  + "/bin/" + key.getKey(); //$NON-NLS-1$
	}
	
	/**
	 * Return the Binary file key if its exist in the current valid keys.
	 */
	protected BinariesKey find(String key) {
		BinariesKey result = cache.get(key);
		if (result == null) {
			return null;
		}
		if (result.getLimit().after(new Date())) {
			return result;
		} else {
			cache.remove(key);
		}
		return null;
	}
	
	protected Date getCurrentLimit() {
		return new Date(System.currentTimeMillis() + timeKeyDuration);
	}
	
	protected int getNewFileId(String category) {
		//TODO On ne devrait pas servir de nouvel Id tant que la config n'a pas été chargée...
		categoriesUpdated = true;
		Integer value;
		synchronized (categories) {
			value = categories.get(category);
			if (value == null) {
				value = new Integer(1);
			} else {
				value = new Integer(value.intValue() + 1);
			}
			categories.put(category, value);
		}
		return value.intValue();
	}

	protected void test(String category, int id) {
		synchronized (categories) {
			Integer value = categories.get(category);
			if ((value == null) || (value.intValue() < id)) {
				categoriesUpdated = true;
				categories.put(category, new Integer(id));
			}
		}
	}
	
	public File getPath() {
		return path;
	}
	
	protected String getDirName(String category) {
		return path.getAbsolutePath() + category.replace('.', '_');
	}
	
	protected String getFileNamePrefix(String category, int id) {
		return path.getAbsolutePath() + category.replace('.', '_') + getSubDir(id) + '/' + id + '_';
	}

	private void fileEvent(Event event) {
		if (eventTracker != null) {
			EventAdmin ea = (EventAdmin)eventTracker.getService();
			if (ea != null) {
				ea.postEvent(event);
			}
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private Event newEvent(String type, String category, int id, File file) {
		Properties props = new Properties();
		props.put(FILE_EVENT_TYPE, type);
		if (file != null) {
			props.put(FILE_EVENT_FILENAME, file.getAbsolutePath());
			props.put(FILE_EVENT_FILE, file);
		}
		props.put(FILE_EVENT_CATEGORY, category);
		props.put(FILE_EVENT_ID, id);
		return new Event(FILE_EVENT_TOPIC, (Dictionary) props);
	}
	
	/**
	 * Post an event after deleting a File.
	 */
	public void fileEventDel(String category, int id,File file) {
		fileEvent(newEvent(FILE_EVENT_TYPE_DELETE, category, id, file));
	}

	/**
	 * Post an event after creating of modifying a File.
	 */
	public void fileEventNew(String category, int id, File file) {
		fileEvent(newEvent(FILE_EVENT_TYPE_CHANGE, category, id, file));
	}

	/**
	 * Look up for the corresponding file into the repository.
	 */
	public File getFile(String category, int id) {
		final String sid = Integer.toString(id) + '_';
		File parent = new File(getDirName(category) + getSubDir(id));
		if ((parent != null) && parent.isDirectory()) {
			for(File file: parent.listFiles(new FilenameFilter() {
				public boolean accept(File dir, String name) {
					return name.startsWith(sid);
				}
			})) {
				if (file.isFile()) {
					return file;
				}
			}
		}		
		return null;
	}

	public boolean removeFiles(String category, int id) {
		final String sid = Integer.toString(id) + '_';
		boolean deleted = false;
		File parent = new File(getDirName(category) + getSubDir(id));
		if ((parent != null) && parent.isDirectory()) {
			for (File file: parent.listFiles(new FilenameFilter() {
				public boolean accept(File dir, String name) {
					return name.startsWith(sid);
				}
			})) {
				if (file.isFile()) {
					if (file.delete()) {
						deleted = true;
					} else {
						debug("Unable to delete file: " + file.getAbsolutePath());
					}
				}
			}
		}		
		return deleted;
	}

	private String getSubDir(int id) {
		return "/dir" + Integer.toString(id / 1000); //$NON-NLS-1$
	}
	
	public BinariesTranferService getBinTransfer() {
		return binTransfer;
	}

	public int getMaxFileSize() {
		return maxFileSize;
	}

	public void setMaxFileSize(int maxFileSize) {
		this.maxFileSize = maxFileSize;
	}
	
}
