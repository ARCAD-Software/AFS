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
package com.arcadsoftware.database.sql;

import java.util.ArrayList;
import java.util.List;

import org.osgi.framework.BundleContext;
import org.osgi.util.tracker.ServiceTracker;

import com.arcadsoftware.database.IQueryRepository;
import com.arcadsoftware.osgi.AbstractConfiguredActivator;

/**
 * Helper class that allow to create configured activator with database connection and 
 * query facilities.
 * 
 */
public abstract class AbstractDatabaseActivator extends AbstractConfiguredActivator {
	
	@SuppressWarnings("rawtypes")
	private ServiceTracker queryTracker;

	private ArrayList<SQLDatabaseAccess> SDAlist = new ArrayList<SQLDatabaseAccess>();

	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.osgi.AbstractConfiguredActivator#start(org.osgi.framework.BundleContext)
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		queryTracker = new ServiceTracker(context,IQueryRepository.class.getName(),null);
		queryTracker.open();
	}

	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.osgi.AbstractConfiguredActivator#stop(org.osgi.framework.BundleContext)
	 */
	@Override
	public void stop(BundleContext context) throws Exception {
		queryTracker.close();
		queryTracker = null;
		for(SQLDatabaseAccess sda:SDAlist) {
			sda.close();
		}
		SDAlist.clear();
		super.stop(context);
	}

	/**
	 * Create a new Database access.
	 * 
	 * @param databaseID
	 * @return
	 */
	protected SQLDatabaseAccess openDatabaseAccess(String databaseID) {
		SQLDatabaseAccess result = new SQLDatabaseAccess(this,databaseID);
		SDAlist.add(result);
		return result;
	}
	
	protected void closeDatabaseAccess(SQLDatabaseAccess sda) {
		sda.close();
		SDAlist.remove(sda);
	}
	
	
	/**
	 * Return an SQL query form the centralized repository.
	 * @param id
	 * @return
	 */
	public String getQuery(String id) {
		if (queryTracker == null) {
			return null;
		}
		Object service = queryTracker.getService();
		if (service instanceof IQueryRepository) {
			return ((IQueryRepository)service).getQuery(id);
		} else {
			return null;
		}
	}

	/**
	 * Return an SQL query form the centralized repository.
	 * @param id
	 * @return
	 */
	public List<String> getQueriesList(String prefix) {
		if (queryTracker == null) {
			return null;
		}
		Object service = queryTracker.getService();
		if (service instanceof IQueryRepository) {
			return ((IQueryRepository)service).getQueriesList(prefix);
		} else {
			return null;
		}
	}
}
