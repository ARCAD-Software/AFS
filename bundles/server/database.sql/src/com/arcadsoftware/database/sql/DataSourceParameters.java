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
package com.arcadsoftware.database.sql;

import java.util.HashMap;
import java.util.Map;

/**
 * Generic parameters of the DataSource.
 * 
 * @author ARCAD Software
 */
public class DataSourceParameters {

	private final String id;
	private final String type;
	private final String url;
	private final String login;
	private final String dialect;
	private final char[] pwd;
	private final int poolmin;
	private final int poolmax;
	private final int timeOut;
	private final HashMap<String, Object> parameters;
	
	public DataSourceParameters(final String id, final String type, final String url,
			final String login, final char[] pwd, final int poolmin, final int poolmax, final int timeOut, final String dialect) {
		super();
		parameters = new HashMap<String, Object>();
		this.id = id;
		this.type = type;
		this.url = url;
		this.login = login;
		this.pwd = pwd;
		this.poolmin = poolmin;
		this.poolmax = poolmax;
		this.timeOut = timeOut;
		this.dialect = dialect;
	}

	public Map<String, Object> getParameters() {
		return parameters;
	}
	
	public void addParameter(String name, Object value) {
		parameters.put(name, value);
	}

	public String getId() {
		return id;
	}

	public String getType() {
		return type;
	}

	public String getUrl() {
		return url;
	}

	public String getLogin() {
		return login;
	}

	public char[] getPwd() {
		return pwd;
	}

	public int getPoolmin() {
		return poolmin;
	}

	public int getPoolmax() {
		return poolmax;
	}

	public int getTimeOut() {
		return timeOut;
	}

	public String getDialect() {
		return dialect;
	}
	
}
