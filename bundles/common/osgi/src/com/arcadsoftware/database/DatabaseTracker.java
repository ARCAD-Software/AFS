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
package com.arcadsoftware.database;

import java.sql.Connection;
import java.sql.SQLException;

import javax.sql.DataSource;

import org.eclipse.core.runtime.IAdaptable;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.service.log.LogService;
import org.osgi.util.tracker.ServiceTracker;

import com.arcadsoftware.osgi.internal.Activator;
import com.arcadsoftware.osgi.internal.FakeDataSource;

/**
 * This tracker represent a basic system to provide database connection across bundles.
 * 
 * It provide access to any OSGi service referenced as a javax.sql.DataSource.
 */
public class DatabaseTracker extends ServiceTracker<DataSource, Object> {

	/**
	 * Define the unique name of the database source connection.
	 */
	public static final String DatabaseID = "DatabaseID"; //$NON-NLS-1$
	
	/**
	 * Define the SQL Dialect name.
	 * 
	 * @see IDataSourceInformations
	 */
	public static final String DatabaseDialect = "DatabaseDialect"; //$NON-NLS-1$
	
	/**
	 * Define the Database Type name.
	 * 
	 * @see IDataSourceInformations
	 */
	public static final String DatabaseType = "DatabaseType"; //$NON-NLS-1$

	/**
	 * Define the JDBC Database URL.
	 */
	public static final String DatabaseURL = "DatabaseURL"; //$NON-NLS-1$
	
	
	private final String databaseID;
	private final boolean keepit;
	private Connection cxn;
	
	/**
	 * Create a database tracker.
	 * 
	 * @param context the current bundle context
	 * @param datavaseId the unique name of the database to be tracked. 
	 */
	public DatabaseTracker(BundleContext context,String databaseID, boolean keepit) {
		super(context, DataSource.class, null);
		this.databaseID = databaseID;
		this.keepit = keepit;
	}

	@Override
	public Object addingService(ServiceReference<DataSource> reference) {
		String id = (String) reference.getProperty(DatabaseID);
		if ((databaseID == null) || (databaseID.equals(id))) {
			Object service = super.addingService(reference);
			if (keepit && (cxn == null)) {
				DataSource ds = getDataSource(service);
				if (ds != null) {
					try {
						cxn = ds.getConnection();
					} catch (SQLException e) {
					}
				}
			}
			return service;
		} else {
			return null;
		}
	}

	@Override
	public synchronized void close() {
		if (cxn != null) {
			try {
				cxn.close();
			} catch (SQLException e) {
			}
			cxn = null;
		}
		super.close();
	}

	@Override
	public void removedService(ServiceReference<DataSource> reference, Object service) {
		// TODO we should wait for the last unregister !
		if (cxn != null) {
			try {
				cxn.close();
			} catch (SQLException e) {
			}
			cxn = null;
		}
		super.removedService(reference, service);
	}

	/**
	 * Return a connection to the database.
	 * 
	 * @return null if no connection is available.
	 */
	public Connection getConnection() {
		Object service = getService();
		if (service == null) {
			return null;
		}
		if (service instanceof DataSource) {
			try {
				return ((DataSource)service).getConnection();
			} catch (SQLException e) {
				Activator.getInstance().log(getServiceReference(),LogService.LOG_ERROR,e.getLocalizedMessage(),e);
				return null;
			}
		}
		if (service instanceof IAdaptable) {
			return (Connection)((IAdaptable)service).getAdapter(Connection.class);
		}
		if (service instanceof Connection) {
			return (Connection)service;
		}
		return null;
	}

	private DataSource getDataSource(Object service) {
		if (service == null) {
			return null;
		}
		if (service instanceof DataSource) {
			return (DataSource)service;
		}
		if (service instanceof IAdaptable) {
			return (DataSource)((IAdaptable)service).getAdapter(DataSource.class);
		}
		if (service instanceof Connection) {
			return new FakeDataSource((Connection)service);
		}
		return null;
	}
	
	/**
	 * Return a DataSource linked to the database.
	 * 
	 * @return
	 */
	public DataSource getDataSource() {
		return getDataSource(getService());
	}
}
