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
package com.arcadsoftware.database.sql.postgresql.internal;

import java.sql.SQLException;
import java.util.Map.Entry;

import javax.sql.DataSource;

import org.postgresql.Driver;

import com.arcadsoftware.database.IDataSourceInformations;
import com.arcadsoftware.database.sql.DataSourceParameters;
import com.arcadsoftware.database.sql.IDataSourceProvider;
import com.zaxxer.hikari.HikariConfig;

public class DataSourceProvider implements IDataSourceProvider {

	private final Activator activator;
	
	public DataSourceProvider(Activator activator) {
		super();
		this.activator = activator;
	}

	@Override
	public boolean acceptDatabaseType(String databaseType) {
		return IDataSourceInformations.DBTYPE_PostgreSQL.equals(databaseType);
	}

	@Override
	public DataSource createDataSource(DataSourceParameters parameters) throws SQLException {
		String url = parameters.getUrl();
		activator.debug("Generate a DataSource for PostgreSQL: " + url);
		if (!Driver.isRegistered()) {
			Driver.register();
		}
		HikariConfig conf = new HikariConfig();
		conf.setJdbcUrl(parameters.getUrl());
		conf.setDriverClassName("org.postgresql.Driver"); //$NON-NLS-1$
		conf.setPoolName(parameters.getId());
		conf.setUsername(parameters.getLogin());
		conf.setPassword(new String(parameters.getPwd()));
		if (parameters.getTimeOut() > 10) {
			conf.setConnectionTimeout(parameters.getTimeOut());
		}
		if (parameters.getPoolmax() > 1) {
			conf.setMaximumPoolSize(parameters.getPoolmax());
		}
		for(Entry<String, Object> e: parameters.getParameters().entrySet()) {
			conf.addDataSourceProperty(e.getKey(), e.getValue());
		}
		return new ExtendableHikariDataSource(activator, activator.getContext(), conf);
	}
}