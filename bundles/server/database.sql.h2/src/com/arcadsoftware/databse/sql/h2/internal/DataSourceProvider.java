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
package com.arcadsoftware.databse.sql.h2.internal;

import java.sql.SQLException;

import javax.sql.DataSource;

import org.h2.jdbcx.JdbcConnectionPool;

import com.arcadsoftware.database.IDataSourceInformations;
import com.arcadsoftware.database.sql.DataSourceParameters;
import com.arcadsoftware.database.sql.IDataSourceProvider;

public class DataSourceProvider implements IDataSourceProvider {

	private final Activator activator;
	
	public DataSourceProvider(Activator activator) {
		super();
		this.activator = activator;
	}

	@Override
	public boolean acceptDatabaseType(String databaseType) {
		return IDataSourceInformations.DBTYPE_H2.equals(databaseType) || //
				IDataSourceInformations.DBTYPE_H2embed.equals(databaseType) || //
				IDataSourceInformations.DBTYPE_H2pooled.equals(databaseType);
	}

	@Override
	public DataSource createDataSource(DataSourceParameters parameters) throws SQLException {
		activator.debug("Generate a DataSource for H2Database: type=" + parameters.getType());
		String url = parameters.getUrl();
		if (url == null) {
			url = "jdbc:h2:./database/" + parameters.getId(); //$NON-NLS-1$
		} else if ((url.length() < 5) || !"jdbc".equals(url.substring(0, 4).toLowerCase())) { //$NON-NLS-1$
			url = "jdbc:h2:" + url; //$NON-NLS-1$
		}
		String login = parameters.getLogin();
		if ((login == null) || login.isEmpty()) {
			login = "sa"; //$NON-NLS-1$
		}
		if (IDataSourceInformations.DBTYPE_H2.equalsIgnoreCase(parameters.getType()) || //
				IDataSourceInformations.DBTYPE_H2embed.equalsIgnoreCase(parameters.getType())) { // only here for ascendant compatibility !
			return new H2ConnectionWarper(activator, login, parameters.getPwd(), url);
		} 
		if (IDataSourceInformations.DBTYPE_H2pooled.equalsIgnoreCase(parameters.getType())) {
			JdbcConnectionPool jcp = JdbcConnectionPool.create(url, login, new String(parameters.getPwd()));
			jcp.setMaxConnections(parameters.getPoolmax());
			jcp.setLoginTimeout(parameters.getTimeOut());
			return jcp;
		}
		throw new SQLException("Internal ERROR: Unsupported database type: " + parameters.getType());
	}

}
