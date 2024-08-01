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
package cli;

import java.io.File;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Date;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.stream.Collectors;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.tool.cli.DataSourceCommand;

public final class DBMigration extends DataSourceCommand {

	public static void main(String[] args) {
		new DBMigration(args).exec();
	}

	private final Hashtable<String, Object> confChanged = new Hashtable<String, Object>();
	
	public DBMigration() {
		super();
	}
	
	public DBMigration(String[] args) {
		super(args);
	}

	@Override
	protected String getVersion() {
		return "1.0.0"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandFullName() {
		return "dbmigration"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandDescription() {
		return "This command allow to migrate the application Database from H2 to PostgreSQL or DB2i. To do so, the H2 database must have be updated to the latest version and the target database installed with the current installation scripts. Refer to the application installation documentation for details.";
	}

	@Override
	protected Map<String, String> getArgumentsDescription() {
		Map<String, String> result = super.getArgumentsDescription();
		result.put("[<url>]", //$NON-NLS-1$
				"Specify the target JDBC URL to the new database. This database must be created and be initialized with the current version of the database. If not provided the URL will be prompted.");
		result.put("[-l|-login <login>]", //$NON-NLS-1$
				"Specify the target database login. This login must the one which will be used by the application. If not provided the login will be prompted.");
		result.put("[-p|-password <password>]", //$NON-NLS-1$
				"Specify the target database paswword. If not provided the password will be prompted.");
		result.put("[-nb|-nobackup]", //$NON-NLS-1$
				"If used this parameter disabled the backup of the source H2 database before to process to the migration.");
		return result;
	}

	@Override
	protected int run() {
		try {
			return super.run();
		} finally {
			if (!confChanged.isEmpty()) {
				Hashtable<String, Object> props = getOSGiConfiguration("com.arcadsoftware.database.sql"); //$NON-NLS-1$
				props.clear();
				props.putAll(confChanged);
				saveOSGiConfiguration();
			}
		}
	}

	@Override
	protected int runDataSourceCommand(String dataSourceID, String dataSourceType, Connection connection, String h2url, Properties connectionProperties) {
		if (!dataSourceType.toLowerCase().startsWith("h2")) { //$NON-NLS-1$
			println("The data source \"" + dataSourceID + "\" [" + dataSourceType + "] can not be migrated to PostgreSQL nor to DB2i.");
			return 0;
		}
		// Manage missing parameters
		String url = getArgument(0);
		if ((url == null) || !url.toLowerCase().startsWith("jdbc:")) { //$NON-NLS-1$
			println("You nee to specify a JDBC database URL as follow:");
			println("For PostgreSQL JDBC URL: jdbc:postgresql:[//<host>[:<port>]/]<database>");
			println("For IBM DB2i JDBC URL: jdbc:jt400://<host>[;libraries=<library name>]");
			url = read("Type the JDBC URL of your database: ");
			if (!url.toLowerCase().startsWith("jdbc:")) { //$NON-NLS-1$
				printError("Invalid URL: " + url);
				return ERROR_WRONG_PARAMETER;
			}
		}
		boolean isDB2i = url.toLowerCase().startsWith("jdbc:jt400:"); //$NON-NLS-1$
		String login = getArgumentValue(new String[] {"-l", "-login"}, (String) null); //$NON-NLS-1$ //$NON-NLS-2$
		if (login == null) {
			login = read("Type the database login: ");
		}
		char[] pwd = getArgumentValue(new String[] {"-p", "-pwd", "-password"}, (char[]) null); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		if (pwd == null) {
			pwd = readSecret("Type the database password: ");
		}
		// Test the PostgreSQL connection and get a connection !!!
		Connection pgconnection = null;
		try {
			if (pwd.length > 0) {
				pgconnection = DriverManager.getConnection(url, login, new String(pwd));
			} else if (login.length() > 0) {
				pgconnection = DriverManager.getConnection(url, login, null);
			} else {
				pgconnection = DriverManager.getConnection(url);
			}
		} catch (SQLException e) {
			printError("ERROR: Unable to connect to the target database: " + e.getLocalizedMessage());
			return ERROR_WRONG_PARAMETER;
		}
		// Test the target database Version !
		// Compare this version to the H2 database version.
		int vh2 = getDBVersion(connection);
		if (vh2 < 0) {
			printError("The H2 Database is not accessible, empty, or not updated at the required version level.");
			printError("Check the application server configuration.");
			return ERROR_INVALID_CONFIGURATION;
		}
		int vpg = getDBVersion(pgconnection);
		if (vpg < 0) {
			printError("The target Database is not accessible, empty, or not installed at the required version level.");
			printError("Check the target database.");
			return ERROR_INVALID_CONFIGURATION;
		}
		if (vh2 != vpg) {
			printError(String.format("The Databases version of H2 and target databases are not equals, the migration process can only be performed fron the same database versions (H2 %d != %d target).", vh2, vpg));
			return ERROR_INVALID_CONFIGURATION;
		}
		println(String.format("The Databases version are compatibles (H2 = %d, target = %d).", vh2, vpg));
		// H2 database Backup.
		if (!isArgument("-nb", "-nobackup")) { //$NON-NLS-1$ //$NON-NLS-2$
			File backupFile = new File(String.format("./database/backup/migration_%1$tY%1$tm%1$td_%1$tH%1$tM%1$tS.zip", new Date())); //$NON-NLS-1$
			try {
				backupFile.getParentFile().mkdirs();
				try (PreparedStatement ps = connection.prepareStatement("script to ? compression deflate cipher AES password ?;")) { //$NON-NLS-1$
					ps.setString(1, backupFile.getAbsolutePath());
					ps.setString(2, new String(pwd));
					ps.execute();
				}
			} catch (SQLException e) {
				printError("Unable to backup the H2 database: " + e.getLocalizedMessage());
				return ERROR_INTERNAL_ERROR;
			}
			try {
				println("The original H2 database has been backup in the file: " + backupFile.getCanonicalPath());
			} catch (IOException e1) {
				println("The original H2 database has been backup in the file: " + backupFile.getAbsolutePath());
			}
		}
		// Remove the Constraints.
		try {
			runscript(pgconnection, new File("./database/migrate/drop_fk_constraints.sql"), true, true); //$NON-NLS-1$
		} catch (Exception e) {
			printError("Internal Error durin execution of \"drop_fk_constraints.sql\": " + e.getLocalizedMessage());
			if (isArgument("-debug")) { //$NON-NLS-1$
				e.printStackTrace();
			}
			return ERROR_INTERNAL_ERROR;
		}
		println("The Relation constraint are disabled during migration.");
		// Truncate tables and migrate Data.
		try {
			migrate(connection, pgconnection, isDB2i);
		} catch (Exception e) {
			printError("Error during migration: " + e.getLocalizedMessage());
			printError("There may be an unexpected difference between the H2 Database and the target one...");
			printError("You may try to alter the Databases and retry the automatic migration, try the manual migration process or contact the ARCAD-Software support.");
			return ERROR_INTERNAL_ERROR;
		}
		// Upgrade the sequences.
		if (isDB2i) {
			resetDB2iSequences(connection, pgconnection);
		} else {
			try {
				runscript(pgconnection, new File("./database/migrate/set_sequences.sql"), true); //$NON-NLS-1$
			} catch (Exception e) {
				printError("Internal Error during execution of \"set_sequences.sql\": " + e.getLocalizedMessage());
				if (isArgument("-debug")) { //$NON-NLS-1$
					e.printStackTrace();
				}
				return ERROR_INTERNAL_ERROR;
			}
		}
		println("The Sequence next value are updated to the new database content.");
		// Restore contraints.
		try {
			runscript(pgconnection, new File("./database/migrate/create_fk_constraints.sql"), true, isDB2i); //$NON-NLS-1$
		} catch (Exception e) {
			printError("Internal Error during execution of \"create_fk_constraints.sql\": " + e.getLocalizedMessage());
			if (isArgument("-debug")) { //$NON-NLS-1$
				e.printStackTrace();
			}
			return ERROR_INTERNAL_ERROR;
		}
		println("The Relation constraint are restored.");
		// UPdate the DS configuration... 
		Hashtable<String, Object> props = getOSGiConfiguration("com.arcadsoftware.database.sql"); //$NON-NLS-1$
		String prefix = dataSourceID + '.';
		for (Entry<String, Object> e: props.entrySet()) {
			if (!e.getKey().startsWith(prefix)) {
				confChanged.put(e.getKey(), e.getValue());
			}
		}
		confChanged.put(dataSourceID + KEY_DATABASEURL, url);
		if (!login.isEmpty()) {
			confChanged.put(dataSourceID + KEY_DATABASELOGIN, login);
		}
		if (pwd.length > 0) {
			confChanged.put(dataSourceID + KEY_DATABASEPWD, Crypto.encrypt(pwd));
		}
		println("The Application server configuration is updated to the new database connection. You can restart it.");
		println();
		println("Note that the old H2 database is not deleted, you can archive it, the database is a file stored");
		println("into the \"database\" sub-folder, with the .mv.db extension.");
		return 0;
	}

	private void resetDB2iSequences(Connection connection, Connection connectioni) {
		try {
			// Get the list of tables...
			String catalog = connection.getCatalog();
			String schema = connection.getSchema();
			DatabaseMetaData dbMeta = connection.getMetaData();
			try (ResultSet rs = dbMeta.getTables(catalog, schema, null, new String[] { "TABLE" })) { //$NON-NLS-1$
				while (rs.next()) {
					String table = rs.getString("TABLE_NAME");
					// Get the PK of each table...
					String pkname = null;
					try (ResultSet crs = dbMeta.getColumns(catalog, schema, table, null)) {
						while (crs.next()) {
							// IS_AUTOINCREMENT String => Indicates whether this column is auto incremented
							//   YES --- if the column is auto incremented
							//   NO --- if the column is not auto incremented
							//   empty string --- if it cannot be determined whether the column is auto incremented
							if ("YES".equalsIgnoreCase(crs.getString("IS_AUTOINCREMENT"))) {
								pkname = crs.getString("COLUMN_NAME"); //$NON-NLS-1$
								break;
							}
						}
					} catch (SQLException e) {
						println("Unable to get Table metadata: " + table);
						if (isArgument("-debug")) { //$NON-NLS-1$
							e.printStackTrace();
						}
						continue;
					}
					if (pkname == null) {
						if (isArgument("-debug")) {
							println("No Auto increment column find for table: " + table);
						}
					} else {
						try (PreparedStatement ps = connection.prepareStatement("select max(" + pkname + ") from " + table);
								ResultSet mrs = ps.executeQuery()) {
							if (mrs.next()) {
								int max = mrs.getInt(1);
								if (max > 0) {
									max++;
									try (PreparedStatement rps = connectioni.prepareStatement("alter table " + table + " alter column " + pkname + " restart with ?")) {
										rps.setInt(1, max);
										rps.execute();
									}
								}
							}
						} catch (SQLException e) {
							printError("Unable to reset the Prinary Key sequence value for the table: " + table);
							if (isArgument("-debug")) { //$NON-NLS-1$
								e.printStackTrace();
							}
						}
					}
				}
			}
			
		} catch (SQLException e) {
			printError("Unable to reset the Primary keys identity next value: " + e.getLocalizedMessage());
			if (isArgument("-debug")) { //$NON-NLS-1$
				e.printStackTrace();
			}
		}
	}

	private int copy(String catalog, String schemaName, String tableName, Connection h2Connection, Connection postgresqlConnection) throws SQLException {
		List<String> columns = new ArrayList<>();
		DatabaseMetaData dbMeta = h2Connection.getMetaData();
		int columnCount = 0;
		try (ResultSet rs = dbMeta.getColumns(catalog, schemaName, tableName, null)) {
			while (rs.next()) {
				String columnName = rs.getString("COLUMN_NAME"); //$NON-NLS-1$
				columns.add(columnName);
				columnCount++;
			}
		}
		String queryColumns = columns.stream().collect(Collectors.joining(", ")); //$NON-NLS-1$
		String sqlQuery = "SELECT " + queryColumns + " FROM " + tableName; //$NON-NLS-1$ //$NON-NLS-2$
		try (PreparedStatement pstmtQuery = h2Connection.prepareStatement(sqlQuery);
				ResultSet rsQuery = pstmtQuery.executeQuery()) {

			String sqlTruncate = "TRUNCATE TABLE " + tableName; //$NON-NLS-1$
			String sqlInsert = "INSERT INTO " + tableName + " (" + queryColumns + ") VALUES (" + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					columns.stream().map(c -> "?").collect(Collectors.joining(", ")) + ")"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			try (PreparedStatement pstmtTruncate = postgresqlConnection.prepareStatement(sqlTruncate);
					PreparedStatement pstmtInsert = postgresqlConnection.prepareStatement(sqlInsert)) {
				pstmtTruncate.execute();
				int nbRead = 0;
				while (rsQuery.next()) {
					for (int i = 1; i <= columnCount; i++) {
						pstmtInsert.setObject(i, rsQuery.getObject(i));
						// TODO Manage NULL value, Cast to the right type (VARCHAR does not work on Oracle !)
					}
					pstmtInsert.addBatch();
					nbRead++;
				}
				int nbInsert = 0;
				int[] rowsInsert = pstmtInsert.executeBatch();
				int nbErr = 0;
				for (int i: rowsInsert) {
					if (i > 0) {
						nbInsert += i;
					} else if (i == Statement.EXECUTE_FAILED) {
						nbErr++;
					}
				}
				if (isArgument("-debug")) { //$NON-NLS-1$
					if (nbRead != nbInsert) {
						println("ERROR: Migrated line count do not match for table \"%s\", %d lines read, %d lines inserted.", tableName, nbRead, nbInsert);
					}
					if (nbErr > 0) {
						println("ERROR: %d execution error with SQL instruction: %s", nbErr, sqlInsert);
					}
				}
				return nbInsert;
			}
		}
	}

	private void migrate(Connection h2Connection, Connection postgresqlConnection, boolean isDB2i) throws Exception {
		String catalog = h2Connection.getCatalog();
		String schema = h2Connection.getSchema();
		DatabaseMetaData dbMeta = h2Connection.getMetaData();
		try (ResultSet rs = dbMeta.getTables(catalog, schema, null, new String[] { "TABLE" })) { //$NON-NLS-1$
			int tbcount = 0;
			int lncount = 0;
			while (rs.next()) {
				lncount += copy(catalog, schema, rs.getString("TABLE_NAME"), h2Connection, postgresqlConnection); //$NON-NLS-1$
				tbcount++;
			}
			println(String.format("%d Tables migrated for a total of %d lines.", tbcount, lncount));
		}
	}
	
}
