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

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import javax.sql.DataSource;

import org.eclipse.osgi.framework.console.CommandInterpreter;
import org.eclipse.osgi.framework.console.CommandProvider;
import org.osgi.framework.Constants;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;

import com.arcadsoftware.database.DatabaseTracker;
import com.arcadsoftware.database.IDataSourceInformations;
import com.arcadsoftware.database.sql.SQLDatabaseAccess;

public class DatabaseCommandProvider implements CommandProvider {

	private Activator activator;
	
	public DatabaseCommandProvider(Activator activator) {
		super();
		this.activator = activator;
	}
	

	public String getHelp() {
		return Messages.DatabaseCommandProvider_Help+
		Messages.DatabaseCommandProvider_Help_db +
		Messages.DatabaseCommandProvider_Help_dblist +
		Messages.DatabaseCommandProvider_Help_DBhelp+
		Messages.DatabaseCommandProvider_Help_dbtest;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public void _dbdel(CommandInterpreter ci) throws Exception {
		String id = ci.nextArgument();
		if (id == null) {
			_dblist(ci);
		}
		id = id + "."; //$NON-NLS-1$
		// Get the configuration administration service.
		ServiceReference ref = activator.getContext().getServiceReference(ConfigurationAdmin.class.getName());
		if (ref == null) {
			ci.println(Messages.DatabaseCommandProvider_NoConfigAdmin);
			return;
		}
		ConfigurationAdmin configAdmin = (ConfigurationAdmin) activator.getContext().getService(ref);
		Configuration c = null;
		try {
			c = configAdmin.getConfiguration(activator.getContext().getBundle().getSymbolicName(), null);
			if (c == null) {
				ci.println(Messages.DatabaseCommandProvider_NoDB);
				return;
			}
			Dictionary props = c.getProperties();
			if (props == null) {
				ci.println(Messages.DatabaseCommandProvider_NoDB);
				return;
			}
			Enumeration keys = props.keys();
			ArrayList<String> rk = new ArrayList<String>();
			while (keys.hasMoreElements()) {
				try {
					String key = (String)keys.nextElement();
					if (key.startsWith(id)) {
						rk.add(key);
					}
				} catch (ClassCastException e) {}
			}
			for (String key:rk) {
				props.remove(key);
			}
			c.update(props);
		} catch (Exception e) {
			ci.println(Messages.DatabaseCommandProvider_Error + e.getLocalizedMessage());
		}
	}	
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public void _db(CommandInterpreter ci) throws Exception {
		String did = ci.nextArgument();
		if (did == null) {
			_dblist(ci);
		} else {
			String id = did.replace(".", "_"); //$NON-NLS-1$ //$NON-NLS-2$
			// Get the configuration administration service.
			ServiceReference ref = activator.getContext().getServiceReference(ConfigurationAdmin.class.getName());
			if (ref == null) {
				ci.println(Messages.DatabaseCommandProvider_NoConfigAdmin);
				return;
			}
			ConfigurationAdmin configAdmin = (ConfigurationAdmin) activator.getContext().getService(ref);
			Configuration c = null;
			try {
				c = configAdmin.getConfiguration(activator.getContext().getBundle().getSymbolicName(), null);
				if (c != null) {
					Dictionary props = c.getProperties();
					if (props == null) {
						props = new Properties();
						props.put(Constants.SERVICE_PID, activator.getContext().getBundle().getSymbolicName());
					}
					props.put(id + Activator.KEY_DATABASEID, did);
					String value = ci.nextArgument();
					if (value == null) {
						ci.println(Messages.DatabaseCommandProvider_MissingSGDB);
						return;
					}
					// Add some aliases... (need to be documented).
					if (value.equalsIgnoreCase("o") || //$NON-NLS-1$
							value.equalsIgnoreCase("ora") || //$NON-NLS-1$
							value.equalsIgnoreCase("oracle")) { //$NON-NLS-1$
						value = IDataSourceInformations.DBTYPE_Oracle;
					}
					if (value.equalsIgnoreCase("db2udb") || //$NON-NLS-1$
							value.equalsIgnoreCase("db2")) { //$NON-NLS-1$
						value = IDataSourceInformations.DBTYPE_DB2;
					}
					if (value.equalsIgnoreCase("db2400") ||  //$NON-NLS-1$
							value.equalsIgnoreCase("db400") || //$NON-NLS-1$
							value.equalsIgnoreCase("os400")) { //$NON-NLS-1$
						value = IDataSourceInformations.DBTYPE_DB400;
					}
					if (value.equalsIgnoreCase("null") || //$NON-NLS-1$
							value.equalsIgnoreCase("nil")) { //$NON-NLS-1$
						value = IDataSourceInformations.DBTYPE_NONE;
					}
					if (value.equalsIgnoreCase("fb") || //$NON-NLS-1$
							value.equalsIgnoreCase("firebird")) { //$NON-NLS-1$
						value = IDataSourceInformations.DBTYPE_Firebird;
					}
					if (value.equalsIgnoreCase("fbs")) { //$NON-NLS-1$
						value = IDataSourceInformations.DBTYPE_FirebirdDistant;
					}
					if (value.equalsIgnoreCase("fbe")) { //$NON-NLS-1$
						value = IDataSourceInformations.DBTYPE_FirebirdEmbedded;
					}
					if (value.equalsIgnoreCase("fbj")) { //$NON-NLS-1$
						value = IDataSourceInformations.DBTYPE_FirebirdJava;
					}
					if (value.equalsIgnoreCase("my") || //$NON-NLS-1$
							value.equalsIgnoreCase("mysql")) { //$NON-NLS-1$
						value = IDataSourceInformations.DBTYPE_MySql;
					}
					if (value.equalsIgnoreCase("j") || //$NON-NLS-1$
							value.equalsIgnoreCase("jndi")) { //$NON-NLS-1$
						value = IDataSourceInformations.DBTYPE_JNDI;
					}
					if (value.equalsIgnoreCase("mssql") || //$NON-NLS-1$
							value.equalsIgnoreCase("sqlserver")) { //$NON-NLS-1$
						value = IDataSourceInformations.DBTYPE_MSSqlServer;
					}
					if (value.equalsIgnoreCase("dby") || //$NON-NLS-1$
							value.equalsIgnoreCase("derby")) { //$NON-NLS-1$
						value = IDataSourceInformations.DBTYPE_Derby;
					}
					if (value.equalsIgnoreCase("dbye") || //$NON-NLS-1$
							value.equalsIgnoreCase("derby-embedded")) { //$NON-NLS-1$
						value = IDataSourceInformations.DBTYPE_DerbyEmbedded;
					}
					if (value.equalsIgnoreCase("pgs")) { //$NON-NLS-1$
						value = IDataSourceInformations.DBTYPE_PostgreSQL;
					}
					if (value.equalsIgnoreCase("h") || //$NON-NLS-1$
							value.equalsIgnoreCase("hsqldb")) { //$NON-NLS-1$
						value = IDataSourceInformations.DBTYPE_HSQLDB;
					}
					props.put(id + Activator.KEY_DATABASETYPE, value);
					value = ci.nextArgument();
					if (value == null) {
						ci.println(Messages.DatabaseCommandProvider_MissingURL);
						return;
					}
					props.put(id + Activator.KEY_DATABASEURL, value);
					value = ci.nextArgument();
					if (value != null) {
						props.put(id + Activator.KEY_DATABASELOGIN, value);
						value = ci.nextArgument();
						if (value != null) {
							props.put(id + Activator.KEY_DATABASEPWD, value);
							value = ci.nextArgument();
						}
						value = ci.nextArgument();
						if (value != null) {
							props.put(id + Activator.KEY_DATABASETIMEOUT, Integer.valueOf(value));
							value = ci.nextArgument();
							if (value != null) {
								props.put(id + Activator.KEY_DATABASEPOOLMIN, Integer.valueOf(value));
								value = ci.nextArgument();
								if (value != null) {
									props.put(id + Activator.KEY_DATABASEPOOLMAX, Integer.valueOf(value));
								}						
							}
						}
					}
					c.update(props);
				}
			} catch (Exception e) {
				ci.println(Messages.DatabaseCommandProvider_Error + e.getLocalizedMessage());
			}
		}
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public void _dblist(CommandInterpreter ci) throws Exception {
		ci.println(Messages.DatabaseCommandProvider_list);
		// Get the configuration administration service.
		ServiceReference ref = activator.getContext().getServiceReference(ConfigurationAdmin.class.getName());
		if (ref == null) {
			ci.println(Messages.DatabaseCommandProvider_NoConfigAdmin);
			return;
		}
		ConfigurationAdmin configAdmin = (ConfigurationAdmin) activator.getContext().getService(ref);
		Configuration c = null;
		try {
			c = configAdmin.getConfiguration(activator.getContext().getBundle().getSymbolicName(), null);
			if (c == null) {
				ci.println(Messages.DatabaseCommandProvider_NoDB);
				return;
			}
			Dictionary props = c.getProperties();
			if (props == null) {
				ci.println(Messages.DatabaseCommandProvider_NoDB);
				return;
			}
			Enumeration keys = props.keys();
			HashSet<String> ids = new HashSet<String>();
			while (keys.hasMoreElements()) {
				try {
					String key = (String)keys.nextElement();
					if (!"service.pid".equalsIgnoreCase(key)) {
						int i = key.indexOf('.');
						if (i > 0) {
							ids.add(key.substring(0, i));
						}
					}
				} catch (ClassCastException e) {}
			}

			ServiceReference[] dsrefs = activator.getContext().getServiceReferences(DataSource.class.getName(), null);
			for (String dsid: ids) {
				try {
					ci.print("DataSource ");
					ci.print(dsid);
					ci.print(' ');
					boolean inactive = true;
					if (dsrefs != null) {
						for(ServiceReference sr:dsrefs) {
							if (dsid.equals(sr.getProperty(DatabaseTracker.DatabaseID))) {
								ci.print(Messages.DatabaseCommandProvider_Declared);
								inactive = false;
								break;
							}
						}
					}
					if (inactive) {
						ci.print(Messages.DatabaseCommandProvider_Undeclared);
					}
					printDBConfig(dsid, props, ci);
				} catch (ClassCastException e) {}
			}
		} catch (Exception e) {
			ci.println(Messages.DatabaseCommandProvider_Error + e.getLocalizedMessage());
		}
	}

	private void printDBConfig(String id, @SuppressWarnings("rawtypes") Dictionary props, CommandInterpreter ci) {
		ci.println();
		Object value = props.get(id + Activator.KEY_DATABASETYPE);
		if (value != null) {
			ci.println(Messages.DatabaseCommandProvider_sgdb + value);
		}
		value = props.get(id + Activator.KEY_DATABASEURL);
		if (value != null) {
			ci.println(Messages.DatabaseCommandProvider_url + value);
		}
		value = props.get(id + Activator.KEY_DATABASELOGIN);
		if (value != null) {
			ci.println(Messages.DatabaseCommandProvider_login + value);
		}
		value = props.get(id + Activator.KEY_DATABASEPWD);
		if (value != null) {
			if (value.toString().length() == 0) {
				ci.println(Messages.DatabaseCommandProvider_password);
			} else {
				ci.println(Messages.DatabaseCommandProvider_passwordxxxx);
			}
		}
		value = props.get(id + Activator.KEY_DATABASETIMEOUT);
		if (value != null) {
			ci.println(Messages.DatabaseCommandProvider_Timeout + value);
		}
		value = props.get(id + Activator.KEY_DATABASEPOOLMIN);
		if (value != null) {
			ci.println(Messages.DatabaseCommandProvider_PoolMin + value);
		}
		value = props.get(id + Activator.KEY_DATABASEPOOLMAX);
		if (value != null) {
			ci.println(Messages.DatabaseCommandProvider_PoolMax + value);
		}
	}

	@SuppressWarnings("rawtypes")
	public void _dbtest(CommandInterpreter ci) throws Exception {
		String id = ci.nextArgument();
		if (id == null) {
			IDataSourceInformations di = activator.getService(IDataSourceInformations.class);
			for(String i: di.getDataSources()) {
				testDataSource(ci, i);
			}
			return;
		}
		String s = ci.nextArgument();
		if (s == null) {
			testDataSource(ci, id);
		} else {
			StringBuilder sql = new StringBuilder(s);
			s = ci.nextArgument();
			while (s != null) {
				sql.append(' ').append(s);
				s = ci.nextArgument();
			}
			if (sql.length() == 0) {
				ci.println(Messages.DatabaseCommandProvider_SpecifySQL);
				return;
			}
			s = sql.toString();
			SQLDatabaseAccess sda = new SQLDatabaseAccess(activator, id);
			try {
				if (s.toLowerCase().startsWith("select ")) { //$NON-NLS-1$
					long t = System.currentTimeMillis();
					List list = sda.queryList(s);
					ci.println(String.format(Messages.DatabaseCommandProvider_Timing, System.currentTimeMillis() - t)); 
					if (list == null) {
						ci.println(Messages.DatabaseCommandProvider_NoResult);
					} else {
						int i = 1;
						for(Object o: list) {
							ci.println(Messages.DatabaseCommandProvider_Row + i);
							i++;
							if (o instanceof Map) {
								for(Object e:((Map)o).entrySet()) {
									if (e instanceof Entry) {
										try {
											String key = ((Entry)e).getKey().toString();
											String val = ((Entry)e).getValue().toString();
											ci.println(key + " = " + val); //$NON-NLS-1$
										} catch (Exception ee) {}
									}
								}
							}
						}
					}
				} else if (s.toLowerCase().startsWith("insert into ")) { //$NON-NLS-1$
					String idcol = null;
					if (s.charAt(s.length()-1) == ']') {
						int i = s.lastIndexOf('[');
						idcol = s.substring(i+1,s.length()-1);
						s = s.substring(0, i-1);
					}
					if ((idcol != null) && (idcol.length() == 0)) {
						long t = System.currentTimeMillis();
						int r = sda.update(s);
						ci.println(String.format(Messages.DatabaseCommandProvider_Timing, System.currentTimeMillis() - t)); 
						ci.println(Messages.DatabaseCommandProvider_NumberOfRow + r);
					} else {
						long t = System.currentTimeMillis();
						int r = sda.insert(s, idcol);
						ci.println(String.format(Messages.DatabaseCommandProvider_Timing, System.currentTimeMillis() - t)); 
						ci.println(Messages.DatabaseCommandProvider_IDNumber + r);
					}
				} else {
					long t = System.currentTimeMillis();
					int r = sda.update(s);
					ci.println(String.format(Messages.DatabaseCommandProvider_Timing, System.currentTimeMillis() - t)); 
					ci.println(Messages.DatabaseCommandProvider_NumberOfRow + r);
				}
			} finally {
				sda.close();
			}
		}
	}

	private void testDataSource(CommandInterpreter ci, String id) {
		ServiceReference<DataSource> srds = null;
		try {
			for (ServiceReference<DataSource> sr: activator.getContext().getServiceReferences(DataSource.class, null)) {
				if (id.equals(sr.getProperty(DatabaseTracker.DatabaseID))) {
					srds = sr;
					break;
				}
			}
		} catch (InvalidSyntaxException e) {}
		if (srds == null) {
			ci.println(String.format("The datasource \"%s\" is not registered", id));
			return;
		}
		ci.println("Testing the Datasource: " + id);
		ci.println("    Type: " + srds.getProperty(DatabaseTracker.DatabaseType));
		String s = (String) srds.getProperty(DatabaseTracker.DatabaseURL);
		if (s != null) {
			ci.println("    URL: " + s);
		}
		s = (String) srds.getProperty(DatabaseTracker.DatabaseDialect);
		if (s != null) {
			ci.println("    Dialect: " + s);
		}
		ci.println();
		DataSource ds = activator.getContext().getService(srds);
		if (ds == null) {
			ci.println("ERROR DataSource unregistered.");
		}
		long t = System.currentTimeMillis();
		Connection cn;
		try {
			cn = ds.getConnection();
		} catch (SQLException e) {
			ci.println("ERROR: Unable to connect to DataSource: " + e.getLocalizedMessage());
			return;
		}
		try {
			try {
				Statement st = cn.createStatement();
				try {
					st.executeUpdate("create table TESTDBTEMP (test_col varchar(200))"); //$NON-NLS-1$
					ci.println("  Table creation is working...");
				} finally {
					st.close();
				}
			} catch (SQLException e) {
				ci.println("ERROR: Unable to create a Table in the DataSource.");
				return;
			}
			try {
				Statement st = cn.createStatement();
				try {
					st.executeUpdate("insert into TESTDBTEMP (test_col) values ('test string, you can drop this table.')"); //$NON-NLS-1$
					ci.println("  Data Insertion is working...");
				} finally {
					st.close();
				}
			} catch (SQLException e) {
				ci.println("ERROR: Unable to insert data in the DataSource.");
			}
			try {
				Statement st = cn.createStatement();
				try {
					st.executeUpdate("update TESTDBTEMP set test_col='you can drop this table.'"); //$NON-NLS-1$
					ci.println("  Data Update is working...");
				} finally {
					st.close();
				}
			} catch (SQLException e) {
				ci.println("ERROR: Unable to update data in the DataSource.");
			}
			try {
				Statement st = cn.createStatement();
				try {
					st.executeQuery("select * from TESTDBTEMP"); //$NON-NLS-1$
					ci.println("  Data Selection is working...");
				} finally {
					st.close();
				}
			} catch (SQLException e) {
				ci.println("ERROR: Unable to select data in the DataSource.");
			}
			try {
				Statement st = cn.createStatement();
				try {
					st.executeUpdate("delete from TESTDBTEMP"); //$NON-NLS-1$
					ci.println("  Data Deletion is working...");
				} finally {
					st.close();
				}
			} catch (SQLException e) {
				ci.println("ERROR: Unable to delete data in the DataSource.");
			}
			try {
				Statement st = cn.createStatement();
				try {
					st.executeUpdate("drop table TESTDBTEMP"); //$NON-NLS-1$
					ci.println("  Table Deletion is working...");
				} finally {
					st.close();
				}
			} catch (SQLException e) {
				ci.println("ERROR: Unable to drop table in the DataSource (the TESTDBTEMP table must be deleted manually).");
			}
			try {
				Statement st = cn.createStatement();
				try {
					ResultSet rs = st.executeQuery("select ABV_ID, ABV_NAME from ARCADDBV order by ABV_ID desc"); //$NON-NLS-1$
					if (rs.next()) {
						ci.println(String.format("  DataSource Version %s (with ID: %d).", rs.getString(2), rs.getInt(1)));
					} else {
						ci.println("  No DataSource version information available (Empty version).");
					}
					rs.close();
				} finally {
					st.close();
				}
			} catch (SQLException e) {
				ci.println("  No DataSource version information available (Unversioned).");
			}
		} finally {
			try {
				cn.close();
			} catch (SQLException e) {
				ci.println("ERROR while closing connection: " + e.getLocalizedMessage());
			}
			t = System.currentTimeMillis() - t;
			ci.println("Timing indicator = " + t);
		}
	}
}
