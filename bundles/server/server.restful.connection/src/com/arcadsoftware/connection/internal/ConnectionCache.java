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
package com.arcadsoftware.connection.internal;

import java.util.Hashtable;
import java.util.List;

import com.arcadsoftware.rest.connection.ConnectionUserBean;
import com.arcadsoftware.rest.connection.IAuthentificationService;
import com.arcadsoftware.rest.connection.IConnectionCache;
import com.arcadsoftware.rest.connection.IConnectionCredential;

/*
 * Implement a simple cache with an Hashtable.
 * 
 * This cache is a multiple entry cache. Each user is cached for any credential it contain.
 * Update of one user update all the cached users.
 * 
 * TODO optimize the update phase (all users share the same object instance).
 * 
 */
public class ConnectionCache implements IConnectionCache {

	private Hashtable<String, ConnectionUserBean> cache = new Hashtable<String, ConnectionUserBean>();
	private Hashtable<String, Hashtable<Integer, ConnectionUserBean>> userCache = new Hashtable<String, Hashtable<Integer, ConnectionUserBean>>();
	
	private Activator activator;
	
	public ConnectionCache(Activator activator) {
		super();
		this.activator = activator;
	}
	
	public ConnectionUserBean get(Object uniqueID) {
		return cache.get(uniqueID);
	}
	
	public ConnectionUserBean getUser(String userType, int userID) {
		if (userType == null) {
			userType = "null"; //$NON-NLS-1$
		}
		Hashtable<Integer, ConnectionUserBean> table = userCache.get(userType);
		if (table == null) {
			return null;
		}
		return table.get(userID);
	}
	
	public ConnectionUserBean put(ConnectionUserBean user) {
		//Test if the user is not already cached with one of the new credentials.
		ConnectionUserBean old = null;
		for(IConnectionCredential credential : user.getCredentials()) {
			old = cache.get(credential.getUniqueId());
			if (old != null) {
				// Credentials fusion...
				for(IConnectionCredential oldCredential : old.getCredentials()) {
					if (user.getCredential(oldCredential.getUniqueId()) == null) {
						user.addCredential(oldCredential);
					}
				}
				break;
			}
		}
		// update the cache 
		for(IConnectionCredential credential : user.getCredentials()) {
			cache.put(credential.getUniqueId(),user);
		}
		String userType = user.getUserType();
		if (userType == null) {
			userType = "null"; //$NON-NLS-1$
		}
		Hashtable<Integer, ConnectionUserBean> table = userCache.get(userType);
		if (table == null) {
			table = new Hashtable<Integer, ConnectionUserBean>(); 
			userCache.put(userType, table);
		}
		table.put(user.getId(),user);
		return user;
	}

	/**
	 * Return the size of the cache.
	 * @return
	 */
	public int count() {
		return userCache.size();
	}
	
	public void purge(String userType, int userID) {
		if (userType == null) {
			userType = "null"; //$NON-NLS-1$
		}
		Hashtable<Integer, ConnectionUserBean> table = userCache.get(userType);
		if (table != null) {
			ConnectionUserBean user = table.get(userID);
			if (user != null) {
				// we get one user, we will use it to purge the cache.
				for(IConnectionCredential credential : user.getCredentials()) {
					cache.remove(credential.getUniqueId());
				}
				// purge userCache
				table.remove(userID);
			}
		}
		List<IAuthentificationService> services = activator.getServices(IAuthentificationService.class);
		if (services != null) {
			for (IAuthentificationService service: services) {
				service.purgeConnectionCache(userID);
			}
		}
	}
	
	public void purgeAll() {
		cache.clear();
		userCache.clear();
		List<IAuthentificationService> services = activator.getServices(IAuthentificationService.class);
		if (services != null) {
			for (IAuthentificationService service: services) {
				service.purgeConnectionCache();
			}
		}
		activator.debug("Connection Users Cache purged.");
	}

	public void purgeAll(String userType) {
		if (userType == null) {
			userType = "null"; //$NON-NLS-1$
		}
		Hashtable<Integer, ConnectionUserBean> table = userCache.get(userType);
		if (table != null) {
			for(ConnectionUserBean user: table.values()) {
				// we get one user, we will use it to purge the cache.
				for(IConnectionCredential credential : user.getCredentials()) {
					cache.remove(credential.getUniqueId());
				}
			}
			userCache.remove(userType);
		}
		List<IAuthentificationService> services = activator.getServices(IAuthentificationService.class);
		if (services != null) {
			for (IAuthentificationService service: services) {
				service.purgeConnectionCache();
			}
		}
	}
}
