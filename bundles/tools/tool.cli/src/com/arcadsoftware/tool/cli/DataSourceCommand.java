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
package com.arcadsoftware.tool.cli;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;

import org.h2.tools.Server;
import org.h2.util.ScriptReader;
import org.h2.util.StringUtils;

import com.arcadsoftware.crypt.Crypto;

public abstract class DataSourceCommand extends Command {

	static {
		safeLoadDriver("org.h2.Driver");
		safeLoadDriver("org.postgresql.Driver");
		safeLoadDriver("com.ibm.as400.access.AS400JDBCDriver");
	}
	
	private static void safeLoadDriver(final String driverClass) {
		try {
			Class.forName(driverClass);
		} catch (Exception e) {}
	}
	
	protected static final String KEY_DATABASEID = ".dbid"; //$NON-NLS-1$
	protected static final String KEY_DATABASETYPE = ".type"; //$NON-NLS-1$
	protected static final String KEY_DATABASEURL = ".url"; //$NON-NLS-1$
	protected static final String KEY_DATABASELOGIN = ".login"; //$NON-NLS-1$
	protected static final String KEY_DATABASEPWD = ".password"; //$NON-NLS-1$
	protected static final String KEY_DATABASEPOOLMIN = ".poolmin"; //$NON-NLS-1$
	protected static final String KEY_DATABASEPOOLMAX = ".poolmax"; //$NON-NLS-1$
	protected static final String KEY_DATABASETIMEOUT = ".timeout"; //$NON-NLS-1$
	protected static final String KEY_DATABASEDESC = ".desc"; //$NON-NLS-1$
	protected static final String KEY_DATABASEDIALECT = ".dialect"; //$NON-NLS-1$

	private Server h2TCPServer;
	
	public DataSourceCommand() {
		super();
	}

	public DataSourceCommand(String[] args) {
		super(args);
	}

	@Override
	protected int run() {
		try {
			final String ds = getArgumentValue(new String[] {"-ds", "-datasource", "-ds.name"}, (String) null);
			final String dslogin = getArgumentValue(new String[] {"-ds.login", "-datasource.login"}, (String) null);
			String dspwd = getArgumentValue(new String[] {"-ds.pwd", "-datasource.password"}, (String) null);
			if ((dslogin != null) && (dspwd == null)) {
				char[] pwdc = readSecret("Enter the password of the Database user \"" + dslogin + "\": ");
				if (pwdc == null) {
					printError("ERROR: no password provided !");
					return ERROR_MISSING_PARAMETER;
				}
				dspwd = new String(pwdc);
			}
			Hashtable<String, Object> props = getOSGiConfiguration("com.arcadsoftware.database.sql");
			// FIXME this algorithm is really tricky... It need to be reworked to avoid multiple execution on the same data source !!!
			if (props != null) {
				Enumeration<String> keys = props.keys();
				while (keys.hasMoreElements()) {
					String key = keys.nextElement();
					String dbid = null;
					String dbtype = null;
					if (key.endsWith(KEY_DATABASEID)) {
						dbid = key.substring(0, key.length() - KEY_DATABASEID.length());
						dbtype = (String) props.get(dbid + KEY_DATABASETYPE);
					} else if (key.endsWith(KEY_DATABASETYPE)) {
						dbid = key.substring(0, key.length() - KEY_DATABASETYPE.length());
						if ((props.get(dbid + KEY_DATABASEID) != null)) {
							dbid = null;
						} else {
							dbtype = (String) props.get(key);
						}
					} else if (key.endsWith(KEY_DATABASEURL)) {
						dbid = key.substring(0, key.length() - KEY_DATABASEURL.length());
						if ((props.get(dbid + KEY_DATABASEID) == null) && (props.get(dbid + KEY_DATABASETYPE) == null)) {
							String url = props.get(key).toString();
							if (url.startsWith("jdbc:h2:")) { //$NON-NLS-1$
								dbtype = "h2";
							} else if (url.startsWith("jdbc:as400:")) { //$NON-NLS-1$
								dbtype = "jt400";
							} else if (url.startsWith("jdbc:oracle:")) { //$NON-NLS-1$
								dbtype = "oracle";
							} else if (url.startsWith("jdbc:sqlserver:")) { //$NON-NLS-1$
								dbtype = "mssqlserver";
							} else if (url.startsWith("jdbc:jtds:sqlserver:")) { //$NON-NLS-1$
								dbtype = "jtds";
							} else if (url.startsWith("jdbc:mysql:")) { //$NON-NLS-1$
								dbtype = "mysql";
							} else if (url.startsWith("jdbc:postgresql:")) { //$NON-NLS-1$
								dbtype = "postgresql";
							} else if (url.startsWith("jdbc:hsqldb:")) { //$NON-NLS-1$
								dbtype = "hsql";
							} else if (url.startsWith("jdbc:odbc:")) { //$NON-NLS-1$
								dbtype = "odbc";
							} else {
								dbtype = null;
							}
						} else {
							dbid = null;
						}
					}
					if ((dbid != null) && ((ds == null) || dbid.equalsIgnoreCase(ds))) {
						if (dbtype == null) {
							dbtype = "h2"; //$NON-NLS-1$
						} else {
							dbtype = dbtype.toLowerCase();
						}
						String url = null;
						String login = null;
						String pwd = null;
						if (props.get(dbid + KEY_DATABASEURL) != null) {
							url = props.get(dbid + KEY_DATABASEURL).toString().trim();
						}
						if (props.get(dbid + KEY_DATABASELOGIN) != null) {
							login = props.get(dbid + KEY_DATABASELOGIN).toString();
						}
						if (props.get(dbid + KEY_DATABASEPWD) != null) {
							pwd = new String(Crypto.decrypt(props.get(dbid + KEY_DATABASEPWD).toString()));
						}
						// Mimic all production default rules for H2...
						if (dbtype.startsWith("h2")) {
							checkH2ServerConfiguration();
							if (url == null) {
								url = "jdbc:h2:" + new File(getHomeDirectory(), "database/" + dbid).getAbsolutePath(); //$NON-NLS-1$ //$NON-NLS-2$
							} else if ((url.length() < 5) || !"jdbc".equals(url.substring(0, 4).toLowerCase())) { //$NON-NLS-1$
								url = "jdbc:h2:" + url; //$NON-NLS-1$
							}
							if ((login == null) || login.isEmpty()) {
								login = "sa"; //$NON-NLS-1$
							}
						}					
						Properties cnp = new Properties();
						if (login != null) {
							cnp.put("user", login);
						}
						if (pwd != null) {
							cnp.put("password", pwd);
						}
						String dbidp = dbid + '.';
						for (Entry<String, Object> e: props.entrySet()) {
							String k = e.getKey();
							if (k.startsWith(dbidp) && !k.equals(dbid + KEY_DATABASEID) && !k.equals(dbid + KEY_DATABASEURL)
									 && !k.equals(dbid + KEY_DATABASETYPE) && !k.equals(dbid + KEY_DATABASELOGIN)
									 && !k.equals(dbid + KEY_DATABASEPWD) && !k.equals(dbid + KEY_DATABASEPOOLMIN)
									 && !k.equals(dbid + KEY_DATABASEPOOLMAX) && !k.equals(dbid + KEY_DATABASETIMEOUT)
									 && !k.equals(dbid + KEY_DATABASEDESC) && !k.equals(dbid + KEY_DATABASEDIALECT)) {
								cnp.put(k.substring(dbidp.length()), e.getValue());
							}
						}
						// Mimic all production default rules for DB2i...
						if (("jt400".equals(dbtype) || "db2400".equalsIgnoreCase(dbtype) || "db2i".equalsIgnoreCase(dbtype)) && //
								((url == null) || !url.startsWith("jdbc:jt400:"))) {
							dbtype = "jt400";
							if (url != null) {
								url = "jdbc:jt400://" + url;
							} else {
								url = "jdbc:jt400://localhost";
							}
						}
						if (dslogin != null) {
							Connection cn = null;
							try {
								Properties cnp2 = new Properties(cnp);
								cnp2.put("user", dslogin);
								cnp2.put("password", dspwd);
								try {
									println("Try to connect to the Data Source \"" + dbid + "\" with user \"" + dslogin + "\".");
									cn = DriverManager.getConnection(url, cnp2);
								} catch (SQLException e) {
									cn = null;
									if (isArgument("-debug")) { //$NON-NLS-1$
										e.printStackTrace();
									}
									printWarn("Unable to connect to \"" + dbid + "\" with user \"" + dslogin + "\", trying with default user...");
								}
								if (cn != null) {
									int result = runDataSourceCommand(dbid, dbtype, cn, url, cnp2);
									if (result != 0) {
										return result;
									}
									if (ds != null) {
										return 0;
									}
									continue;
								}
							} finally {
								if (cn != null) {
									try {
										cn.close();
									} catch (SQLException e) {
										if (isArgument("-debug")) { //$NON-NLS-1$
											e.printStackTrace();
										}
									}
								}
							}
						}
						try (Connection cn = DriverManager.getConnection(url, cnp)) {
							int result = runDataSourceCommand(dbid, dbtype, cn, url, cnp);
							if (result != 0) {
								return result;
							}
						} catch (SQLException e) {
							printError("ERROR Unable to connect to the Data Source \"" + dbid + "\": " + e.getLocalizedMessage());
							if (isArgument("-debug")) { //$NON-NLS-1$
								printError("user = " + login + ", pwd = " + pwd);
								e.printStackTrace();
							}
						}
						if (ds != null) {
							return 0;
						}
					}
				}
			} else {
				println("Do data sources found in the configuration.");
			}
			if (ds != null) {
				printError("The Data Source \"" + ds + "\" is not declared into the configuration.");
			}
			return 0;
		} finally {
			stopH2Server();
		}
	}
	
	protected void checkH2ServerConfiguration() {
		if (h2TCPServer == null) {
			Hashtable<String, Object> props = getOSGiConfiguration("com.arcadsoftware.h2");
			if (getProperty(props, "autostart", false)) {
				ArrayList<String> params = new ArrayList<String>();
				if (getProperty(props, "ssl", false)) {
					params.add("-tcpSSL"); //$NON-NLS-1$
				}
				params.add("-ifExists"); //$NON-NLS-1$
				int port = getProperty(props, "port", 9092);
				if ((port > 255) && (port != 9092)) {
					params.add("-tcpPort"); //$NON-NLS-1$
					params.add(String.valueOf(port));
				}
				try {
					h2TCPServer = Server.createTcpServer(params.toArray(new String[params.size()]));
					h2TCPServer.start();
					println("H2 Database Server Started...");
					if (isArgument("-debug")) { //$NON-NLS-1$
						println(h2TCPServer.getStatus());
					}
				} catch (Throwable e) { // There may have an NoClassDefFoundError if H2 is configured but not present.
					h2TCPServer = null;
					printError("Unable to start H2 Server: " + e.getLocalizedMessage());
					if (isArgument("-debug")) { //$NON-NLS-1$
						e.printStackTrace();
					}
				}
			}
		}
	}

	protected void stopH2Server() {
		if (h2TCPServer != null) {
			h2TCPServer.stop();
			println("H2 Database Server stopped.");
			if (isArgument("-debug")) { //$NON-NLS-1$
				println(h2TCPServer.getStatus());
			}
		}
	}
	
	protected boolean isLocalH2Server() {
		return h2TCPServer != null;
	}
	
	@Override
	protected Map<String, String> getArgumentsDescription() {
		HashMap<String, String> result = new HashMap<String, String>();
		result.put("[-ds|-datasource <datasource id>]", "Define the data source to be acceded, if not specified all data sources defined by the application will be acceded.");
		result.put("[-ds.login|-datasource.login <user id>]", "Replace the default user login of the datasources be the specified one. This may be required if the original user have limited rights on the database. This parameter should be used with the -ds one, if not it will be applied to all datasources.");
		result.put("[-ds.pwd|-datasource.password <password>]", "Used with -ds.login this parameter define the new user password. If not used the password will be prompted.");
		return result;
	}

	/**
	 * Get the Database Version stored into the ARCADDBV table. This version is assumed to be the "non AFS" one !
	 * 
	 * <p>
	 * A result of -1 indicate that the database is not accessible or not upgraded to the new database management.
	 * 
	 * @param connection
	 * @return The version number of the database or -1 if the table is empty or do not exist.
	 */
	protected int getDBVersion(Connection connection) {
		try (Statement st = connection.createStatement()) {
			try (ResultSet rs = st.executeQuery("SELECT MAX(DBV_VERSION) FROM ARCADDBV WHERE NOT (DBV_CODE = 'AFS')")) { //$NON-NLS-1$
				if (rs.next()) {
					return rs.getInt(1);
				}
			}
		} catch (SQLException e) {
			if (isArgument("-debug")) { //$NON-NLS-1$
				e.printStackTrace();
			}
		}
		return -1;
	}

	/**
	 * Get the given database version. 
	 * 
	 * @param connection
	 * @param code
	 * @return
	 */
	protected int getDBVersion(Connection connection, String code) {
		try (PreparedStatement st = connection.prepareStatement("SELECT MAX(DBV_VERSION) FROM ARCADDBV WHERE (DBV_CODE = ?)")) { //$NON-NLS-1$
			st.setString(1, code);
			try (ResultSet rs = st.executeQuery()) {
				if (rs.next()) {
					return rs.getInt(1);
				}
			}
		} catch (SQLException e) {
			if (isArgument("-debug")) { //$NON-NLS-1$
				e.printStackTrace();
			}
		}
		return -1;
	}
	
	/**
	 * Get all installed Batabases codes.
	 * 
	 * @param connection
	 * @return
	 */
	protected Set<String> getDBCodeList(Connection connection) {
		HashSet<String> result = new HashSet<String>(2);
		try (Statement st = connection.createStatement()) {
			try (ResultSet rs = st.executeQuery("SELECT DISTINCT DBV_CODE FROM ARCADDBV")) { //$NON-NLS-1$
				while (rs.next()) {
					result.add(rs.getString(1));
				}
			}
		} catch (SQLException e) {
			if (isArgument("-debug")) { //$NON-NLS-1$
				e.printStackTrace();
			}
		}
		return result;
	}
	
	/**
	 * Return true is the database is assumed like an empty one (less than 3 tables in it)...
	 * 
	 * @param connection
	 * @return
	 */
	protected boolean isDatabaseEmpty(String dbtype, Connection connection) {
		String schema = null;
		if (dbtype.startsWith("h2")) { //$NON-NLS-1$
			schema = "PUBLIC"; //$NON-NLS-1$
			// ... Essayer "select schema();"
		}
		try (ResultSet tables = connection.getMetaData().getTables(null, schema, null, new String[] {"TABLE"})) { //$NON-NLS-1$
			return !tables.next();
		} catch (SQLException e) {
			return true;
		}
	}
	
	/**
	 * Check if the DB version table is correctly created and initialized.
	 * 
	 * @return 2 if the table is missing, 1 in the version table is empty or not up to date, and 0 if everything is in place.
	 */
	protected int isDatabaseInitialized(Connection connection) {
		try (PreparedStatement ps = connection.prepareStatement("select * from ARCADDBV")) { //$NON-NLS-1$
			try (ResultSet rs = ps.executeQuery()) {
				if (!rs.next()) {
					return 1;
				}
			}
		} catch (SQLException e) {
			return 2;
		}
		try (PreparedStatement ps = connection.prepareStatement("select DBV_CODE from ARCADDBV")) { //$NON-NLS-1$
			try (ResultSet rs = ps.executeQuery()) {
				if (!rs.next()) {
					return 1;
				}
			}
		} catch (SQLException e) {
			return 1;
		}
		return 0;
	}
	
	/**
	 * Allow to execute a SQL script if an error occurs it print an error message and rethrow the exception.
	 *  
	 * @param connection the database connection.
	 * @param fileSQL the file to run
	 * @param verbose if true print a success message.
	 */
	protected void runscript(Connection connection, File fileSQL, boolean verbose) throws Exception {
		runscript(connection, fileSQL, verbose, false);
	}
	
	/**
	 * Allow to execute a SQL script if an error occurs it print an error message and rethrow the exception.
	 *  
	 * @param connection the database connection.
	 * @param fileSQL the file to run
	 * @param verbose if true print a success message.
	 */
	protected void runscript(Connection connection, File fileSQL, boolean verbose, boolean nopublic) throws Exception {
		if (fileSQL == null) {
			throw new FileNotFoundException("Internal error NULL File !!!");
		}
		if (!fileSQL.isFile()) {
			try {
				printError("The SQL Script file required for this command, does not exists: " + fileSQL.getCanonicalPath());
			} catch (IOException e) {
				printError("The SQL Script file required for this command, does not exists: " + fileSQL.getAbsolutePath());
			}
			throw new FileNotFoundException("Script file not found: " + fileSQL.getAbsolutePath());
		}
		try (FileReader fr = new FileReader(fileSQL)) {
			try (ScriptReader sr = new ScriptReader(fr)) {
				try (Statement st = connection.createStatement()) {
		            String sql = sr.readStatement();
					while (sql != null) {
			            if (!StringUtils.isWhitespaceOrEmpty(sql)) {
			            	if (nopublic) {
			            		sql = sql.replace("public.", ""); //$NON-NLS-1$ //$NON-NLS-2$
			            	}
				            try {
				            	st.execute(sql);
				            } catch (SQLException e) {
				            	if (isArgument("-debug")) { //$NON-NLS-1$
				            		println("Unable to execute the following SQL instruction: " + sql);
				            	}
				            	throw e;
				            }
			            }
			            sql = sr.readStatement();
			        }
				}
			}
		}
    	if (isArgument("-debug")) { //$NON-NLS-1$
    		try {
    			println("The Script execution ended correctly: " + fileSQL.getCanonicalPath());
    		} catch (IOException e) {
        		println("The Script execution ended correctly: " + fileSQL.getAbsolutePath());
    		}
    	}
	}
	
	protected abstract int runDataSourceCommand(String dataSourceID, String dataSourceType, Connection connection, String url, Properties connectionProperties);
	
}
