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
package com.arcadsoftware.database;

/**
 * This service provide some extra informations about dataSources.
 * 
 * <p>
 * Database Types are used to retrieve and configure JDBC driver. Thus
 * many RDBM possess multiples types.
 * 
 * <p>
 * Database Dialect are used for specific RDBM with uses non standard SQL.
 * 
 * <p>
 * Database Names are purely informal.
 */
public interface IDataSourceInformations {

	public static final String DBTYPE_NONE = "none"; //$NON-NLS-1$
	public static final String DBTYPE_JNDI = "jndi"; //$NON-NLS-1$
	public static final String DBTYPE_H2 = "h2"; //$NON-NLS-1$
	public static final String DBTYPE_H2pooled = "h2p"; //$NON-NLS-1$
	public static final String DBTYPE_H2embed = "h2e"; //$NON-NLS-1$
	public static final String DBTYPE_Firebird = "firebird"; //$NON-NLS-1$
	public static final String DBTYPE_FirebirdDistant = "firebirdserver"; //$NON-NLS-1$
	public static final String DBTYPE_FirebirdEmbedded = "firebirdembedded"; //$NON-NLS-1$
	public static final String DBTYPE_FirebirdJava = "firebirdjava"; //$NON-NLS-1$
	public static final String DBTYPE_DB2 = "db2"; //$NON-NLS-1$
	public static final String DBTYPE_DB400 = "jt400"; //$NON-NLS-1$
	public static final String DBTYPE_Oracle = "oracle"; //$NON-NLS-1$
	public static final String DBTYPE_MSSqlServer = "mssqlserver"; //$NON-NLS-1$
	public static final String DBTYPE_MSSqlServerjtds = "jtds"; //$NON-NLS-1$
	public static final String DBTYPE_MySql = "mysql"; //$NON-NLS-1$
	public static final String DBTYPE_Derby = "derby"; //$NON-NLS-1$
	public static final String DBTYPE_DerbyEmbedded = "derby-embedded"; //$NON-NLS-1$
	public static final String DBTYPE_PostgreSQL = "postgresql"; //$NON-NLS-1$
	public static final String DBTYPE_HSQLDB = "hsql"; //$NON-NLS-1$
	public static final String DBTYPE_ODBC = "odbc"; //$NON-NLS-1$
	// Supported dialects:
	public static final int NONE = -2;
	public static final int UNKNOWN = -1;
	public static final int DIALECT_H2DATABASE = 0;
	public static final int DIALECT_Firebird = 1;
	public static final int DIALECT_DB2UDB = 2;
	public static final int DIALECT_DB2400 = 3;
	public static final int DIALECT_Oracle = 4;
	public static final int DIALECT_MSSqlServer = 5;
	public static final int DIALECT_MySql = 6;
	public static final int DIALECT_Derby = 7;
	public static final int DIALECT_PostGreSQL = 8;
	public static final int DIALECT_HSQLDB = 9;
	public static final int DIALECT_SQLStandart = 10;
	
	/**
	 * Database SGDB Names, these names can be used as files prefixes.
	 */
	public static final String[] BDNAMES = new String[] {
		"JNDI Data Source", //$NON-NLS-1$
		"H2 Database (Single)", //$NON-NLS-1$ 
		"H2 Database (Pooled)", //$NON-NLS-1$ 
		"H2 Database (Embed)", //$NON-NLS-1$ 
		"Firebird SQL (Local)", //$NON-NLS-1$
		"Firebird SQL (Distant)", //$NON-NLS-1$
		"Firebird SQL (Embedded)", //$NON-NLS-1$
		"Firebird SQL (Java Driver)", //$NON-NLS-1$
		"IBM DB2 UDB", //$NON-NLS-1$
		"IBM DB2 OS400", //$NON-NLS-1$
		"Oracle Database", //$NON-NLS-1$
		"Microsoft SQL Server", //$NON-NLS-1$
		"MySQL Database", //$NON-NLS-1$
		"Derby Database", //$NON-NLS-1$
		"Derby Database (Embedded)", //$NON-NLS-1$
		"PostgreSQL", //$NON-NLS-1$
		"HSQL DB", //$NON-NLS-1$
		"ODBC Bridge", //$NON-NLS-1$
		"Microsoft SQL Server (JTDS)", //$NON-NLS-1$
		"Unknown" //$NON-NLS-1$
	};

	/**
	 * List of DataSources URL samples.
	 */
	public static final String[] BDURLTYPES = new String[] {
		"<jdbc/datasourcename>", //$NON-NLS-1$
		"jdbc:h2:<path/to/database>", //$NON-NLS-1$ 
		"jdbc:h2:tcp://<hostname>[:<port>]/<path/to/database>", //$NON-NLS-1$ 
		"<path/to/database>[|<charset>]", //$NON-NLS-1$
		"<hostname>[/port]:<path/to/database/on/server>[|<charset>]", //$NON-NLS-1$
		"<path/to/database>[|<charset>", //$NON-NLS-1$
		"<hostname>[/port]:<path/to/database>[|<charset>", //$NON-NLS-1$
		"<hostname>:<port>:<databasename>", //$NON-NLS-1$
		"jdbc:as400://<system>[;<option>=<value>]", //$NON-NLS-1$
		"jdbc:oracle:thin:@<databasename>", //$NON-NLS-1$
		"jdbc:sqlserver://<hostname>[:<port>][;databaseName=<dbname>[;...]]", //$NON-NLS-1$
		"jdbc:mysql://<hostname>[:<port>]/<databasename>", //$NON-NLS-1$
		"[<hostname>[:<port>]:]<path/to/database>", //$NON-NLS-1$
		"<path/to/database>", //$NON-NLS-1$
		"jdbc:postgresql:[//<hostname>[:<port>]/]<databasename>", //$NON-NLS-1$
		"jdbc:hsqldb:hsql://<host>:<port>", //$NON-NLS-1$
		"jdbc:odbc:<predefinedODBCconnection>", //$NON-NLS-1$
		"jdbc:jtds:sqlserver://<hostname>[:<port>][/<database>][;domain=<NTLM>[;instance=<SQLInstance>[;...]]]", //$NON-NLS-1$
		"" //$NON-NLS-1$
	};

	/**
	 * Textual representation of known Dialects.
	 */
	public static final String[] BDDIALECTS = new String[] {
		"H2 Database", //$NON-NLS-1$ 
		"Firebird SQL", //$NON-NLS-1$
		"IBM DB2 UDB", //$NON-NLS-1$
		"IBM DB2 OS400", //$NON-NLS-1$
		"Oracle Databse", //$NON-NLS-1$
		"Microsoft SQL Server", //$NON-NLS-1$
		"MySQL Database", //$NON-NLS-1$
		"Derby Database", //$NON-NLS-1$
		"PostgreSQL", //$NON-NLS-1$
		"HSQL DB", //$NON-NLS-1$
		"SQL92 Generic" //$NON-NLS-1$ 
	};

	/**
	 * Known dialect names. 
	 */
	public static final String[] BDDIALECTS_SHORT = new String[] {
		"H2", //$NON-NLS-1$ 
		"Firebird", //$NON-NLS-1$
		"DB2", //$NON-NLS-1$
		"DB2400", //$NON-NLS-1$
		"Oracle", //$NON-NLS-1$
		"MsSQL", //$NON-NLS-1$
		"MySQL", //$NON-NLS-1$
		"Derby", //$NON-NLS-1$
		"PostgreSQL", //$NON-NLS-1$
		"HSQL", //$NON-NLS-1$
		"" //$NON-NLS-1$ 
	};

	public static final String[] DBTYPEMAPPING = new String[] {
		DBTYPE_JNDI,
		DBTYPE_H2,
		DBTYPE_H2pooled,
		DBTYPE_H2embed,
		DBTYPE_Firebird,
		DBTYPE_FirebirdDistant,
		DBTYPE_FirebirdEmbedded,
		DBTYPE_FirebirdJava,
		DBTYPE_DB2,
		DBTYPE_DB400,
		DBTYPE_Oracle,
		DBTYPE_MSSqlServer,
		DBTYPE_MySql,
		DBTYPE_Derby,
		DBTYPE_DerbyEmbedded,
		DBTYPE_PostgreSQL,
		DBTYPE_HSQLDB,
		DBTYPE_ODBC,
		DBTYPE_MSSqlServerjtds,
		DBTYPE_NONE
	};
	
	/**
	 * OSGi service name.
	 */
	public static final String clazz = IDataSourceInformations.class.getName();
	
	/**
	 * @return the list of DataSources names
	 */
	public String[] getDataSources();
	
	/**
	 * 
	 * @param dataSourceName
	 * @return a Database type.
	 */
	public int getDataBaseType(String dataSourceName);
	
	/**
	 * 
	 * @param dataSourceName
	 * @return true if the data-source is liked to an embedded database. 
	 */
	public boolean isEmbedded(String dataSourceName);
	
	/**
	 * @param dataSourceName
	 * @return the data source description.
	 */
	public String getDataSourceDescription(String dataSourceName);

	/**
	 * Useful for JNDI connection. Give a compatible database SQL dialect.
	 *  
	 * @param dataSourceName
	 * @return a compatible database type.
	 */
	public int getDataBaseDialect(String dataSourceName);

	/**
	 * 
	 * @param dataSourceName
	 * @return
	 */
	public int getTimeout(String dataSourceName);
	
	/**
	 * 
	 * @param dataSourceName
	 * @return
	 */
	public int getMinPool(String dataSourceName);
	
	/**
	 * 
	 * @param dataSourceName
	 * @return
	 */
	public int getMaxPool(String dataSourceName);
	
	/**
	 * 
	 * @param dataSourceName
	 * @return
	 */
	public String getURL(String dataSourceName);
	
	/**
	 * 
	 * @param dataSourceName
	 * @return
	 */
	public String getLogin(String dataSourceName);
	
	/**
	 * 
	 * @param dataSourceName
	 * @return
	 */
	public String getPwd(String dataSourceName);
	
	/**
	 * 
	 * @param dataSourceName
	 */
	public void deleteDataSource(String dataSourceName);
	
	/**
	 * Define a new DataSource.
	 * 
	 * @param name a dataSource unique ID.
	 * @param dbtype a known DataSource Type (see constant DBTYPEMAPPING).
	 * @param auxdbtype a SQL Dialect.
	 * @param url the JDBC URL.
	 * @param login to connect to the Database.
	 * @param pwd password
	 * @param minpool 
	 * @param maxpool
	 * @param timeout Connection time out.
	 */
	public void setDataSource(String name, int dbtype, int dialect, String url, String login, String pwd, int minpool, int maxpool, int timeout);
}
