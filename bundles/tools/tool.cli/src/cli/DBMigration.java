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
		return "This command allow to migrate the application Database from H2 to PostgreSQL. To do so, the H2 database must have be updated to the latest version and the Postgre database installed with the current installation script.";
	}

	@Override
	protected Map<String, String> getArgumentsDescription() {
		Map<String, String> result = super.getArgumentsDescription();
		result.put("[<url>]", //$NON-NLS-1$
				"Specify the PostgreSQL JDBC URL to the new database. This database must be created and be initialized with the current version of the database. If not provided the URL will be prompted.");
		result.put("[-l|-login <login>]", //$NON-NLS-1$
				"Specify the PostgreSQL database login. This login must the one which will be used by the application. If not provided the login will be prompted.");
		result.put("[-p|-password <password>]", //$NON-NLS-1$
				"Specify the PostgreSQL database paswword. If not provided the password will be prompted.");
		result.put("[-nb|-nobackup]", //$NON-NLS-1$
				"If used this parameter disabled the backup of the source database before to process to the migration.");
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
			println("The data source \"" + dataSourceID + "\" [" + dataSourceType + "] can not be migrated to PostgreSQL.");
			return 0;
		}
		// Manage missing parameters
		String url = getArgument(0);
		if ((url == null) || !url.toLowerCase().startsWith("jdbc:")) { //$NON-NLS-1$
			url = read("Type the PostgreSQL JDBC URL (jdbc:postgresql:[//<host>[:<port>]/]<database>): ");
			if (!url.toLowerCase().startsWith("jdbc:")) { //$NON-NLS-1$
				printError("Invalid URL: " + url);
				return ERROR_WRONG_PARAMETER;
			}
		}
		String login = getArgumentValue(new String[] {"-l", "-login"}, (String) null); //$NON-NLS-1$ //$NON-NLS-2$
		if (login == null) {
			login = read("Type the PostgreSQL database login: ");
		}
		char[] pwd = getArgumentValue(new String[] {"-p", "-pwd", "-password"}, (char[]) null); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		if (pwd == null) {
			pwd = readSecret("Type the PostgreSQL database password: ");
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
			printError("ERROR: Unable to connect to the PostgreSQL database: " + e.getLocalizedMessage());
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
			printError("The PostgreSQL Database is not accessible, empty, or not installed at the required version level.");
			printError("Check the postgreSQL database.");
			return ERROR_INVALID_CONFIGURATION;
		}
		if (vh2 != vpg) {
			printError(String.format("The Databases version of H2 and PostgreSQL are not equals, the migration process can only be performed fron the same database versions (H2 %d != %d PG).", vh2, vpg));
			return ERROR_INVALID_CONFIGURATION;
		}
		println(String.format("The Databases version are compatibles (H2 = %d, PostGreSQL = %d).", vh2, vpg));
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
			runscript(pgconnection, new File("./database/migrate/drop_fk_constraints.sql"), true); //$NON-NLS-1$
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
			migrate(connection, pgconnection);
		} catch (Exception e) {
			printError("Error during migration: " + e.getLocalizedMessage());
			printError("There may be an unexpected difference between the H2 Database and the PostgreSQL one...");
			printError("You may try to alter the Databases and retry the automatic migration, try the manual migration process or contact the ARCAD-Software support.");
			return ERROR_INTERNAL_ERROR;
		}
		// Upgrade the sequences.
		try {
			runscript(pgconnection, new File("./database/migrate/set_sequences.sql"), true); //$NON-NLS-1$
		} catch (Exception e) {
			printError("Internal Error durin execution of \"set_sequences.sql\": " + e.getLocalizedMessage());
			if (isArgument("-debug")) { //$NON-NLS-1$
				e.printStackTrace();
			}
			return ERROR_INTERNAL_ERROR;
		}
		println("The Sequence next value are updated to the new database content.");
		// Restore contraints.
		try {
			runscript(pgconnection, new File("./database/migrate/create_fk_constraints.sql"), true); //$NON-NLS-1$
		} catch (Exception e) {
			printError("Internal Error durin execution of \"create_fk_constraints.sql\": " + e.getLocalizedMessage());
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
		println("Note that the old H1 database is not deleted, you can archive it, the database is a file stored");
		println("into the \"database\" sub-folder, with the .mv.db extension.");
		return 0;
	}

	private int copy(String catalog, String schemaName, String tableName, Connection h2Connection, Connection postgresqlConnection) throws SQLException {
		List<String> columns = new ArrayList<>();
		DatabaseMetaData dbMeta = postgresqlConnection.getMetaData();
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
						// TODO Manage NULL value (Cast to the right type (VARCHAR does not work on Oracle !)
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

	private void migrate(Connection h2Connection, Connection postgresqlConnection) throws Exception {
		String catalog = postgresqlConnection.getCatalog();
		if (catalog == null) {
			throw new Exception("The catalog name of the PostgreSQL database must be set.");
		}
		String schema = postgresqlConnection.getSchema();
		if (schema == null) {
			throw new Exception("The schema name of the PostgreSQL database must be set.");
		}
		DatabaseMetaData dbMeta = postgresqlConnection.getMetaData();
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
