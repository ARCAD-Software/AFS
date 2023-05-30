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
package com.arcadsoftware.database.internal;

import com.arcadsoftware.database.IDataSourceInformations;

public class DataSourceInformations implements IDataSourceInformations {
	
	private Activator activator;
	
	public DataSourceInformations(Activator activator) {
		this.activator = activator;
	}

	private String getDBType(String dataSourceName) {
		return activator.getDatabaseProperty(dataSourceName + Activator.KEY_DATABASETYPE);
	}

	public int getDataBaseType(String dataSourceName) {
		String dbtype = getDBType(dataSourceName);
		if ((dbtype == null) || 
				(dbtype.length() == 0) ||
				(dbtype.equals(DBTYPE_NONE))) {
			return NONE;
		}
		if (dbtype.equals(DBTYPE_JNDI)) {
			return 0;
		}
		if (dbtype.equals(DBTYPE_H2)) {
			return 1;
		}
		if (dbtype.equals(DBTYPE_H2pooled)) {
			return 2;
		}
		if (dbtype.equals(DBTYPE_H2embed)) {
			return 3;
		}
		if (dbtype.equals(DBTYPE_Firebird)) {
			return 4;
		}
		if (dbtype.equals(DBTYPE_FirebirdDistant)) {
			return 5;
		}
		if (dbtype.equals(DBTYPE_FirebirdEmbedded)) {
			return 6;
		}
		if (dbtype.equals(DBTYPE_FirebirdJava)) {
			return 7;
		}
		if (dbtype.equals(DBTYPE_DB2)) {
			return 8;
		}
		if (dbtype.equals(DBTYPE_DB400)) {
			return 9;
		}
		if (dbtype.equals(DBTYPE_Oracle)) {
			return 10;
		}
		if (dbtype.equals(DBTYPE_MSSqlServer)) {
			return 11;
		}
		if (dbtype.equals(DBTYPE_MySql)) {
			return 12;
		}
		if (dbtype.equals(DBTYPE_Derby)) {
			return 13;
		}
		if (dbtype.equals(DBTYPE_DerbyEmbedded)) {
			return 14;
		}
		if (dbtype.equals(DBTYPE_PostgreSQL)) {
			return 15;
		}
		if (dbtype.equals(DBTYPE_HSQLDB)) {
			return 16;
		}
		if (dbtype.equals(DBTYPE_ODBC)) {
			return 17;
		}
		return BDNAMES.length - 1; // = unknown
	}

	public boolean isEmbedded(String dataSourceName) {
		String dbtype = getDBType(dataSourceName);
		if (dbtype == null) {
			return false;
		}
		if (DBTYPE_H2.equalsIgnoreCase(dbtype) || 
				DBTYPE_H2pooled.equalsIgnoreCase(dbtype)) {
			String url = activator.getDatabaseProperty(dataSourceName + Activator.KEY_DATABASEURL);
			if (url == null) {
				return false;
			}
			return !url.startsWith("jdbc:h2:tcp"); //$NON-NLS-1$
		}
		if (DBTYPE_HSQLDB.equalsIgnoreCase(dbtype)) {
			String url = activator.getDatabaseProperty(dataSourceName + Activator.KEY_DATABASEURL);
			return url.startsWith("jdbc:hsqldb:mem:") || url.startsWith("jdbc:hsqldb:file:") || url.startsWith("jdbc:hsqldb:res:"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		return DBTYPE_DerbyEmbedded.equalsIgnoreCase(dbtype) || 
				DBTYPE_FirebirdEmbedded.equalsIgnoreCase(dbtype);
	}

	public String getDataSourceDescription(String dataSourceName) {
		String desc = activator.getDatabaseProperty(dataSourceName + Activator.KEY_DATABASEDESC);
		if (desc == null) {
			return ""; //$NON-NLS-1$
		}
		return desc;
	}

	public String[] getDataSources() {
		return activator.getDataSourcesId();
	}

	public void deleteDataSource(String dataSourceName) {
		activator.removeDataSource(dataSourceName);
	}

	public int getDataBaseDialect(String dataSourceName) {
		String dbtype = activator.getDatabaseProperty(dataSourceName + Activator.KEY_DATABASEDIALECT);
		if (dbtype == null) {
			dbtype = getDBType(dataSourceName);
		}
		return getDialectFromType(dbtype);
	}

	public static int getDialectFromType(String dbtype) {
		if ((dbtype == null) || (dbtype.length() == 0)) {
			return DIALECT_SQLStandart;
		}
		if (DBTYPE_HSQLDB.equalsIgnoreCase(dbtype)) {
			return DIALECT_HSQLDB;
		}
		if (DBTYPE_Oracle.equalsIgnoreCase(dbtype)) {
			return DIALECT_Oracle;
		}
		if (DBTYPE_DB2.equalsIgnoreCase(dbtype)) {
			return DIALECT_DB2UDB;
		}
		if (DBTYPE_DB400.equalsIgnoreCase(dbtype)) {
			return DIALECT_DB2400;
		}
		if (DBTYPE_Derby.equalsIgnoreCase(dbtype) || 
				DBTYPE_DerbyEmbedded.equalsIgnoreCase(dbtype)) {
			return DIALECT_Derby;
		}
		if (DBTYPE_Firebird.equalsIgnoreCase(dbtype) || 
				DBTYPE_FirebirdDistant.equalsIgnoreCase(dbtype) ||
				DBTYPE_FirebirdEmbedded.equalsIgnoreCase(dbtype) ||
				DBTYPE_FirebirdJava.equalsIgnoreCase(dbtype)) {
			return DIALECT_Firebird;
		}
		if (DBTYPE_H2.equalsIgnoreCase(dbtype) || 
				DBTYPE_H2pooled.equalsIgnoreCase(dbtype) ||
				DBTYPE_H2embed.equalsIgnoreCase(dbtype)) {
			return DIALECT_H2DATABASE;
		}
		if (DBTYPE_MSSqlServer.equalsIgnoreCase(dbtype) ||
				DBTYPE_MSSqlServerjtds.equalsIgnoreCase(dbtype)) {
			return DIALECT_MSSqlServer;
		}
		if (DBTYPE_MySql.equalsIgnoreCase(dbtype)) {
			return DIALECT_MySql;
		}
		if (DBTYPE_PostgreSQL.equalsIgnoreCase(dbtype)) {
			return DIALECT_PostGreSQL;
		}
		if (DBTYPE_JNDI.equalsIgnoreCase(dbtype) || DBTYPE_ODBC.equalsIgnoreCase(dbtype)) {
			return DIALECT_SQLStandart;
		}
		return DIALECT_SQLStandart;
	}

	public String getLogin(String dataSourceName) {
		return activator.getDatabaseProperty(dataSourceName + Activator.KEY_DATABASELOGIN);
	}

	public int getMaxPool(String dataSourceName) {
		String val = activator.getDatabaseProperty(dataSourceName + Activator.KEY_DATABASEPOOLMAX);
		if (val == null) {
			return Activator.DEFAULT_POOLMAX;
		}
		try {
			return Integer.parseInt(val);
		} catch (NumberFormatException e) {
			return Activator.DEFAULT_POOLMAX;
		}
	}

	public int getMinPool(String dataSourceName) {
		String val = activator.getDatabaseProperty(dataSourceName + Activator.KEY_DATABASEPOOLMIN);
		if (val == null) {
			return Activator.DEFAULT_POOLMIN;
		}
		try {
			return Integer.parseInt(val);
		} catch (NumberFormatException e) {
			return Activator.DEFAULT_POOLMIN;
		}
	}

	public String getPwd(String dataSourceName) {
		return activator.getDatabaseProperty(dataSourceName + Activator.KEY_DATABASEPWD);
	}

	public int getTimeout(String dataSourceName) {
		String val = activator.getDatabaseProperty(dataSourceName + Activator.KEY_DATABASETIMEOUT);
		if (val == null) {
			return Activator.DEFAULT_TIMEOUT;
		}
		try {
			return Integer.parseInt(val);
		} catch (NumberFormatException e) {
			return  Activator.DEFAULT_TIMEOUT;
		}
	}

	public String getURL(String dataSourceName) {
		return activator.getDatabaseProperty(dataSourceName + Activator.KEY_DATABASEURL);
	}

	public void setDataSource(String name, int dbtype, int dialect, String url, String login, String pwd,
			int minpool, int maxpool, int timeout) {
		if ((name == null) || (name.length() == 0) || (url == null) || (dbtype < 0) || (dbtype >= DBTYPEMAPPING.length)) {
			return;
		}
		String dbtypes = DBTYPEMAPPING[dbtype];
		String dbDialect = null;
		if ((dialect >= 0) && (dialect < BDDIALECTS_SHORT.length)) {
			dbDialect = BDDIALECTS_SHORT[dialect];
		}
		if ((login != null) && (login.length() == 0)) {
			login = null;
		}
		if ((pwd != null) && (pwd.length() == 0)) {
			pwd = null;
		}
		if (timeout < 0) {
			timeout = 0;
		}
		if (minpool < 0) {
			minpool = 0;
		}
		if (maxpool < 0) {
			maxpool = 0;
		}
		if (maxpool < minpool) {
			int t = maxpool;
			maxpool = minpool;
			minpool = t;
		}
		name = name.trim().replace('.', '_').replace(' ', '_');
		activator.setDataSource(name,dbtypes,dbDialect,url,login,pwd,minpool,maxpool,timeout);
	}

}
