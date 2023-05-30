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

import java.util.Hashtable;

/**
 * A Configuration Container.
 * 
 * @author ARCAD Software
 */
public class Configuration extends Hashtable<String, Object> {

	private static final long serialVersionUID = 7529239431242852082L;
	
	private final String factoryPid;
	private final String pid;
	
	/**
	 * Create a Factory configuration instance.
	 *  
	 * @param factoryPid The ID of the ManagedServiceFactory 
	 * @param pid The PID of the configuration instance, basically this pid is the "factoryPid + '.' + x" where x is a positive number. 
	 */
	public Configuration(String factoryPid, String pid) {
		super();
		this.factoryPid = factoryPid;
		this.pid = pid;
	}
	
	/**
	 * Create a simple configuration object.
	 * @param pid
	 */
	public Configuration(String pid) {
		super();
		this.factoryPid = null;
		this.pid = pid;
	}

	/**
	 * Get the Configuration Factory associated to this configuration object.
	 * 
	 * @return null for a simple configuration.
	 */
	public String getFactoryPid() {
		return factoryPid;
	}

	/**
	 * Get the Permanent IDentifier (PID) associated to this configuration Object.
	 * 
	 * @return
	 */
	public String getPid() {
		return pid;
	}

}
