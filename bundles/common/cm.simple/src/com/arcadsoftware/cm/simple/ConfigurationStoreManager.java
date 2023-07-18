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
package com.arcadsoftware.cm.simple;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;

import com.arcadsoftware.cm.simple.internal.AbstractConfigurationStorage;

/**
 * This class allow to load and save a bunch of configuration files. 
 * 
 * <p>
 * It use in order of priority:
 * 
 * <ul>
 * <li> JSON object files (.json).
 * <li> Properties files (.cfg).
 * <li> a Global, unique, configuration file (.ini).
 * </ul>
 * 
 * @author ARCAD Software
 */
public class ConfigurationStoreManager extends AbstractConfigurationStorage {

	private final File root;
	private final HashMap<String, Configuration> confs;
	private final HashMap<String, Integer> factoryCount;
	private boolean useCFG;
	private boolean useJSON;
	private boolean useINI;

	/**
	 * Default constructor using "./configuration/osgi.cm.ini" as configuration storage.
	 */
	public ConfigurationStoreManager() {
		this(new File("./configuration/osgi.cm.ini")); //$NON-NLS-1$
	}
	
	/**
	 * Constructor with specific storage location.
	 * @param root
	 */
	public ConfigurationStoreManager(File root) {
		super(null);
		this.root = root;
		confs = new HashMap<String, Configuration>();
		factoryCount = new HashMap<String, Integer>();
	}

	/**
	 * Constructor using a predefined list of configurations.
	 * 
	 * @param root
	 * @param configurations a list of rpedefined configuration.
	 */
	public ConfigurationStoreManager(File root, Collection<Configuration> configurations) {
		this(root);
		if (configurations != null) {
			for(Configuration c: configurations) {
				confs.put(c.getPid(), c);
			}
		}
	}

	/**
	 * Constructor using a predefined list of configurations.
	 * 
	 * @param root
	 * @param csm another ConfigurationStoreManager.
	 */
	public ConfigurationStoreManager(File root, ConfigurationStoreManager csm) {
		this(root, csm.confs.values());
	}

	@Override
	protected void logError(String message, Throwable e) {
		// Override this method to print errors to console !
	}

	@Override
	public Hashtable<String, Object> addConfiguration(String factoryPid, String pid) {
		Configuration c = confs.get(pid);
		if (c != null) {
			return c;
		}
		c = new Configuration(factoryPid, pid);
		confs.put(pid, c);
		return c;
	}

	/**
	 * Add a new empty configuration, if it does not already exist.
	 * 
	 * @param pid
	 * @return
	 */
	public Hashtable<String, Object> addConfiguration(String pid) {
		return addConfiguration(null, pid);
	}

	/**
	 * Add the given configuration, or fusion it with the existing one.
	 * 
	 * @param factoryPid
	 * @param pid
	 * @param conf
	 */
	public void addConfiguration(String factoryPid, String pid, Dictionary<String, Object> conf) {
		Hashtable<String, Object> c = addConfiguration(factoryPid, pid);
		Enumeration<String> en = conf.keys();
		while (en.hasMoreElements()) {
			String k = en.nextElement();
			c.put(k, conf.get(k));
		}
	}


	/**
	 * Add the given configuration, or fusion it with the existing one.
	 * 
	 * @param pid
	 * @param conf
	 */
	public void addConfiguration(String pid, Dictionary<String, Object> conf) {
		addConfiguration(null, pid, conf);
	}
	
	/**
	 * Remove the given configuration.
	 * 
	 * @param pid
	 */
	public void removeConfiguration(String pid) {
		confs.remove(pid);
	}

	@Override
	protected Integer getFactoryCount(String fpid) {
		return factoryCount.get(fpid);
	}

	/**
	 * Get a list of all managed Configuration.
	 * 
	 * @return a List not synchronized with the internal configuration list.
	 */
	public List<Configuration> listAllConfigurations() {
		return new ArrayList<Configuration>(confs.values());
	}
	
	@Override
	protected void setFactoryCount(String fpid, Integer value) {
		factoryCount.put(fpid, value);
	}

	@Override
	protected String getFactoryPid(String pid) {
		Configuration c = confs.get(pid);
		if (c != null) {
			return c.getFactoryPid();
		}
		return null;
	}

	/**
	 * Load the configurations files.
	 * 
	 * @throws IOException
	 */
	public void load() throws IOException {
		load(root, useINI, useCFG, useJSON);
	}

	/**
	 * Update the configuration files.
	 * 
	 * @throws IOException
	 */
	public void save() throws IOException {
		save(root, confs, useINI, useCFG, useJSON);
	}
	
	/**
	 * @return true if the configuration store is empty.
	 */
	public boolean isEmpty() {
		return confs.isEmpty();
	}
	
	/**
	 * @return true if the .cfg files will be loaded and updated.
	 */
	public boolean isUseCFG() {
		return useCFG;
	}

	public void setUseCFG(boolean useCFG) {
		this.useCFG = useCFG;
	}

	/**
	 * @return true if the .json files will be loaded and updated.
	 */
	public boolean isUseJSON() {
		return useJSON;
	}

	public void setUseJSON(boolean useJSON) {
		this.useJSON = useJSON;
	}

	/**
	 * @return true if the global .ini file will be loaded and updated.
	 */
	public boolean isUseINI() {
		return useINI;
	}

	/**
	 * Define if the global .ini file is used. 
	 * 
	 * <p>
	 * if false then the "root" file may be a directory path. All configuration will be stored is json or cfg files.
	 *  
	 * @param useINI
	 */
	public void setUseINI(boolean useINI) {
		this.useINI = useINI;
	}

	/**
	 * Get the base configuration file (.ini file) or the folder to store all configuraiton files.
	 * 
	 * @return
	 */
	public File getRoot() {
		return root;
	}
	
}
