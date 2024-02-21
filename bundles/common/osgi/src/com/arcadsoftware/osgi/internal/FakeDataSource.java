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
package com.arcadsoftware.osgi.internal;

import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.util.logging.Logger;

import javax.sql.DataSource;

/**
 * Return a warping datasource around a single (unique) Connection.
 */
public class FakeDataSource implements DataSource {

	private Connection connection;
	private int tt = 0;
	private PrintWriter logWriter = null;
	
	public FakeDataSource(Connection connection) {
		this.connection = connection;
	}
	
	/* (non-Javadoc)
	 * @see javax.sql.DataSource#getConnection()
	 */
	public Connection getConnection() throws SQLException {
		return connection;
	}

	/* (non-Javadoc)
	 * @see javax.sql.DataSource#getConnection(java.lang.String, java.lang.String)
	 */
	public Connection getConnection(String username, String password) throws SQLException {
		return connection;
	}

	/* (non-Javadoc)
	 * @see javax.sql.DataSource#getLogWriter()
	 */
	public PrintWriter getLogWriter() throws SQLException {
		return logWriter ;
	}

	/* (non-Javadoc)
	 * @see javax.sql.DataSource#getLoginTimeout()
	 */
	public int getLoginTimeout() throws SQLException {
		return tt;
	}

	/* (non-Javadoc)
	 * @see javax.sql.DataSource#setLogWriter(java.io.PrintWriter)
	 */
	public void setLogWriter(PrintWriter out) throws SQLException {
		logWriter = out;
	}

	/* (non-Javadoc)
	 * @see javax.sql.DataSource#setLoginTimeout(int)
	 */
	public void setLoginTimeout(int seconds) throws SQLException {
		tt  = seconds;
	}

	/* (non-Javadoc)
	 * @see java.sql.Wrapper#isWrapperFor(java.lang.Class)
	 */
	public boolean isWrapperFor(Class<?> iface) throws SQLException {
		return iface.isAssignableFrom(DataSource.class);
	}

	/* (non-Javadoc)
	 * @see java.sql.Wrapper#unwrap(java.lang.Class)
	 */
	public <T> T unwrap(Class<T> iface) throws SQLException {
		if (iface.isAssignableFrom(DataSource.class)) {
			return iface.cast(this);
		}
		return null;
	}

	public Logger getParentLogger() throws SQLFeatureNotSupportedException {
		return null;
	}
}