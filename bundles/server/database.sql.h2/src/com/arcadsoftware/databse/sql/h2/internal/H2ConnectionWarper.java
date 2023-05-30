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
package com.arcadsoftware.databse.sql.h2.internal;

import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.util.logging.Logger;

import javax.sql.DataSource;

import org.h2.Driver;

import com.arcadsoftware.osgi.ILoggedPlugin;

/**
 * Create a fake pooling data source.
 * 
 * H2 database connection are fast enough to not require a pooling device.
 */
public class H2ConnectionWarper implements DataSource {

	private ILoggedPlugin activator;
	private String url;
	private String login;
	private char[] pwd;
	private PrintWriter logWriter;
	private int timeout = 30;
	
	/**
	 * Create the data source service.
	 * 
	 * @param login
	 * @param pwd
	 * @param url
	 */
	public H2ConnectionWarper(ILoggedPlugin activator, String login, char[] pwd, String url) {
		super();
		this.activator = activator;
		this.login = login;
		this.pwd = pwd;
		this.url = url;
	}

	/* (non-Javadoc)
	 * @see javax.sql.DataSource#getConnection()
	 */
	public Connection getConnection() throws SQLException {
		try {
			Driver.load();
		    return DriverManager.getConnection(url, login, new String(pwd));
		} catch (SQLException e) {
			activator.error(e.getLocalizedMessage(), e);
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.sql.DataSource#getConnection(java.lang.String, java.lang.String)
	 */
	public Connection getConnection(String username, String password)
			throws SQLException {
		return getConnection();
	}

	/* (non-Javadoc)
	 * @see javax.sql.DataSource#getLogWriter()
	 */
	public PrintWriter getLogWriter() throws SQLException {
		return logWriter;
	}

	/* (non-Javadoc)
	 * @see javax.sql.DataSource#getLoginTimeout()
	 */
	public int getLoginTimeout() throws SQLException {
		return timeout;
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
		timeout = seconds;
	}

	/* (non-Javadoc)
	 * @see java.sql.Wrapper#isWrapperFor(java.lang.Class)
	 */
	public boolean isWrapperFor(Class<?> iface) throws SQLException {
		return false;
	}

	/* (non-Javadoc)
	 * @see java.sql.Wrapper#unwrap(java.lang.Class)
	 */
	public <T> T unwrap(Class<T> iface) throws SQLException {
		return null;
	}

	public Logger getParentLogger() throws SQLFeatureNotSupportedException {
		// TODO Auto-generated method stub
		return null;
	}
}