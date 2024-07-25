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

import java.sql.SQLException;
import java.util.Iterator;
import java.util.Map;
import java.util.ServiceLoader;

import javax.naming.InitialContext;
import javax.sql.DataSource;

import com.arcadsoftware.database.IDataSourceInformations;
import com.arcadsoftware.database.internal.Activator;
import com.arcadsoftware.database.internal.Messages;
import com.arcadsoftware.osgi.AbstractActivator;
import com.arcadsoftware.osgi.ILoggedPlugin;

/**
 * Helper class that can be used to generate Datasources from a predefined set of properties.
 * 
 * <p>
 * Work like JNDI (without JNDI) allow to use specific classloader and different Drivers.
 * 
 * <p>
 * Note that this DataSource will be <b>not</b> registered as an OSGi service.
 * 
 * Creation Date: 2011-8-9
 */
public class DataSourceFactory {

	/**
	 * 
	 * @param databaseID
	 * @param databaseType
	 * @param databaseURL
	 * @param databaseLogin
	 * @param databasePwd
	 * @param databasePoolmin
	 * @param databasePoolmax
	 * @param databaseTimeOut
	 * @return
	 * @deprecated use {@link #createDataSource(ILoggedPlugin, DataSourceParameters)}
	 */
	public static DataSource createDataSource(String databaseID, String databaseType, String databaseURL, String databaseLogin, char[] databasePwd, int databasePoolmin, int databasePoolmax, int databaseTimeOut) {
		return createDataSource(Activator.getInstance(), databaseID, databaseType, databaseURL, databaseLogin, databasePwd, databasePoolmin, databasePoolmax, databaseTimeOut);
	}

	/**
	 * 
	 * @param activator a Bundle activator, or a logger facade.
	 * @param databaseID
	 * @param databaseType
	 * @param databaseURL
	 * @param databaseLogin
	 * @param databasePwd
	 * @param databasePoolmin
	 * @param databasePoolmax
	 * @param databaseTimeOut
	 * @return
	 * @deprecated use {@link #createDataSource(ILoggedPlugin, DataSourceParameters)}
	 */
	public static DataSource createDataSource(ILoggedPlugin activator, String databaseID, String databaseType, String databaseURL, String databaseLogin, char[] databasePwd, int databasePoolmin, int databasePoolmax, int databaseTimeOut) {
		return createDataSource(activator, new DataSourceParameters(databaseID, databaseType, databaseURL, databaseLogin, databasePwd, databasePoolmin, databasePoolmax, databaseTimeOut, null));
	}

	/**
	 * 
	 * @param activator a Bundle activator, or a logger facade.
	 * @param parameters The Data Source parameters
	 * @return
	 */
	public static DataSource createDataSource(ILoggedPlugin activator, DataSourceParameters parameters) {
		if (IDataSourceInformations.DBTYPE_JNDI.equalsIgnoreCase(parameters.getType())) {
			try {
				Object ds = (new InitialContext()).lookup(parameters.getUrl());
				if (ds instanceof DataSource) {
					((DataSource) ds).setLoginTimeout(parameters.getTimeOut());
					return (DataSource) ds;
				} else {
					activator.error(parameters.getUrl() + Messages.Activator_NotValidJNDI, null);
				}
			} catch (Exception e) {
				activator.error(Messages.Activator_Error + parameters.getUrl() , e);
			}
		}
		if (activator instanceof AbstractActivator) {
			for (IDataSourceProvider p: ((AbstractActivator) activator).getServices(IDataSourceProvider.class)) {
				if (p.acceptDatabaseType(parameters.getType())) {
					try {
						return p.createDataSource(parameters);
					} catch (SQLException e) {
						activator.error("Error while creating Data Source from OSGi service, database type: " + parameters.getType(), e);
					}
				}
			}
		}
		ServiceLoader<IDataSourceProvider> loader = ServiceLoader.load(IDataSourceProvider.class);
		if (loader != null) {
			Iterator<IDataSourceProvider> itt = loader.iterator();
			while (itt.hasNext()) {
				IDataSourceProvider p = itt.next();
				if (p.acceptDatabaseType(parameters.getType())) {
					try {
						return p.createDataSource(parameters);
					} catch (SQLException e) {
						activator.error("Error while creating Data Source from Java service, database type: " + parameters.getType(), e);
					}
				}
			}
		}
		return null;
	}

	/**
	 * Get a running OSGi Service DataSource.
	 * 
	 * @param activator
	 * @param name
	 * @return
	 */
	public static DataSource getDataSource(String name) {
		return Activator.getInstance().getDataSource(name);
	}
	
	/**
	 * Get all data sources currently declared on this platform.
	 * 
	 * @return never return null.
	 */
	public static Map<String, DataSource> getDatasources() {
		return Activator.getInstance().getDataSources();
	}
}
