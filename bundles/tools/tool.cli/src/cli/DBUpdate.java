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
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.Map.Entry;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.tool.cli.DataSourceCommand;

import run.Exec;

public final class DBUpdate extends DataSourceCommand {

	public static void main(String[] args) {
		System.exit(new DBUpdate(args).exec());
	}
	
	public DBUpdate() {
		super();
	}
	
	public DBUpdate(String[] args) {
		super(args);
	}

	@Override
	protected String getVersion() {
		return "1.2.0"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandFullName() {
		return "dbupdate"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandDescription() {
		return "This command update the Application Database to the latest installed version.";
	}

	@Override
	protected Map<String, String> getArgumentsDescription() {
		Map<String, String> result = super.getArgumentsDescription();
		result.put("[-i|-install]", //$NON-NLS-1$
				"Create the database if it does not exists.");
		result.put("[-np|-noprompt]", //$NON-NLS-1$
				"Disable user input during process.");
		result.put("[-p|-password <password>]", //$NON-NLS-1$
				"Specify H2 Database backup paswword. By default the H2 database connection password is used.");
		result.put("[-ncdb|-nocleandb]", //$NON-NLS-1$
				"If specified the H2 Database will not be cleaned during update through a backup/destroy/restore process.");
		return result;
	}
	
	@Override
	protected int run() {
		// The Upgrade operation must be done before any connection to the database...
		int result = upgradeH2DatabaseVersion();
		if (result != 0) {
			return result;
		}
		return super.run();
	}

	@Override
	protected int runDataSourceCommand(String dataSourceID, String dataSourceType, Connection connection, String url, Properties connectionProperties) {
		// H2 database Backup.
		boolean close = false;
		try {
			File upgradeDir = new File(getHomeDirectory(), "database/upgrade"); //$NON-NLS-1$
			if (!upgradeDir.isDirectory()) {
				upgradeDir = new File(getHomeDirectory(), "sql/upgrade"); //$NON-NLS-1$
				if (!upgradeDir.isDirectory()) {
					printError("Upgrade SQL script not found.");
					printError("The Database is not installed or must be updated according to the legacy database upgrade process.");
					print("Consult the product documentatin and release notes for more information.");
					return ERROR_CANCELLED;
				}
			}
			if (dataSourceType.startsWith("h2")) { //$NON-NLS-1$
				String pwd = getArgumentValue(new String[] {"-p", "-password"}, connectionProperties.getProperty("password", null)); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ 
				File backupFile = new File(getHomeDirectory(), String.format("database/backup/upgrade_%1$tY%1$tm%1$td_%1$tH%1$tM%1$tS.zip", new Date()));
				try {
					backupFile.getParentFile().mkdirs();
					if ((pwd != null) && !pwd.isEmpty()) {
						try (PreparedStatement ps = connection.prepareStatement("script to ? compression deflate cipher AES password ?")) { //$NON-NLS-1$ //$NON-NLS-2$
							ps.setString(1, backupFile.getAbsolutePath());
							ps.setString(2, pwd);
							ps.execute();
						}
					} else {
						try (PreparedStatement ps = connection.prepareStatement("script to ? compression deflate")) { //$NON-NLS-1$ //$NON-NLS-2$
							ps.setString(1, backupFile.getAbsolutePath());
							ps.execute();
						}
					}
					if (isArgument("-debug")) { //$NON-NLS-1$
						println("H2 Database backup done.");
					}
				} catch (SQLException e) {
					printError("Unable to backup the H2 database: " + e.getLocalizedMessage());
					if (isArgument("-debug")) {
						e.printStackTrace();
					}
					return ERROR_INTERNAL_ERROR;
				}
				try {
					println("H2 Database backup created in file: " + backupFile.getCanonicalPath());
				} catch (IOException e1) {
					println("H2 Database backup created in file: " + backupFile.getAbsolutePath());
				}
				if (!isArgument("-ncdb", "-nocleandb") && (!url.startsWith("jdbc:h2:tcp") || isLocalH2Server())) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					println("Compact the H2 database and rebuild indexes.");
					try {
						try (PreparedStatement ps = connection.prepareStatement("drop all objects delete files")) { //$NON-NLS-1$
							ps.execute();
						}
					} catch (SQLException e) {
						printError("Unable to clean the H2 database: " + e.getLocalizedMessage());
						printWarn("You may have to restore the previous backup.");
						if (isArgument("-debug")) { //$NON-NLS-1$
							e.printStackTrace();
						}
						return ERROR_INTERNAL_ERROR;
					}
					if (isArgument("-debug")) { //$NON-NLS-1$
						println("H2 Database deleted.");
					}
					try {
						try (PreparedStatement ps = connection.prepareStatement("shutdown compact")) { //$NON-NLS-1$
							ps.execute();
						}
					} catch (SQLException e) {
						if (isArgument("-debug")) { //$NON-NLS-1$
							e.printStackTrace();
						}
					}
					if (isArgument("-debug")) { //$NON-NLS-1$
						println("H2 Database reconnection to a fresh database.");
					}
					// Recreate the database connection:
					try {
						connection = DriverManager.getConnection(url, connectionProperties);
					} catch (SQLException e) {
						printError("Unable to reconnect to the H2 database: " + e.getLocalizedMessage());
						printWarn("You may have to restore the previous backup.");
						if (isArgument("-debug")) { //$NON-NLS-1$
							e.printStackTrace();
						}
						return ERROR_INTERNAL_ERROR;
					}
					close = true;
					try {
						if ((pwd != null) && !pwd.isEmpty()) {
							try (PreparedStatement ps = connection.prepareStatement("runscript from ? compression deflate cipher AES password ?")) { //$NON-NLS-1$ //$NON-NLS-2$
								ps.setString(1, backupFile.getAbsolutePath());
								ps.setString(2, pwd);
								ps.execute();
							}
						} else {
							try (PreparedStatement ps = connection.prepareStatement("runscript from ? compression deflate")) { //$NON-NLS-1$ //$NON-NLS-2$
								ps.setString(1, backupFile.getAbsolutePath());
								ps.execute();
							}
						}
						if (isArgument("-debug")) { //$NON-NLS-1$
							println("H2 Database restored from backup.");
						}
					} catch (SQLException e) {
						printError("Unable to restore the H2 database: " + e.getLocalizedMessage());
						printWarn("You may have to manually restore the previous backup.");
						if (isArgument("-debug")) {
							e.printStackTrace();
						}
						return ERROR_INTERNAL_ERROR;
					}
				}
			} else if (!isArgument("-noprompt", "-np")) { //$NON-NLS-1$ //$NON-NLS-2$
				String res = read("Have you made a backup of your database ?\nThe upgrade process may corrupt the database data, a backup of the database is strongly recommended.\nType 'c' to cancel the current operation, any other response to continue.");
				if ("c".equalsIgnoreCase(res)) {
					return ERROR_CANCELLED;
				}
			} else {
				printWarn("The upgrade of the database may corrupt the data, depending on any special modifications made on it. A complete backup is recommended but this script does not have access to the backup tools of your database. This backup should have been done before to run it.");
			}
			String typ = "h2"; //$NON-NLS-1$
			if ("postgresql".equals(dataSourceType)) { //$NON-NLS-1$
				typ = "pg"; //$NON-NLS-1$
				if (isArgument("-debug")) { //$NON-NLS-1$
					println("Starting PostgreSQL database upgrade...");
				}
			} else if ("jt400".equals(dataSourceType)) { //$NON-NLS-1$
				typ = "db2i"; //$NON-NLS-1$
				if (isArgument("-debug")) { //$NON-NLS-1$
					println("Starting IBM DB2i database upgrade...");
				}
			} else if (isArgument("-debug")) { //$NON-NLS-1$
				println("Starting H2 database upgrade...");
			}
			switch (isDatabaseInitialized(connection)) {
			case 2: // Database not created.
				if (isArgument("-debug")) { //$NON-NLS-1$
					println("(No ARCADDBV) Assuming the database is not created !");
				}
				// Tester si la base de données n'est pas vide...
				if (isDatabaseEmpty(typ, connection) || isArgument("-install", "-i")) { //$NON-NLS-1$ //$NON-NLS-2$
					print("The Database is empty, proceeding to the installation.");
					try {
						File sql = new File(upgradeDir, "AFS_V0.sql"); //$NON-NLS-1$
						if (sql.isFile()) {
							runscript(connection, sql, true);
						}
						sql = new File(upgradeDir, "AFS_V0_" + typ + ".sql"); //$NON-NLS-1$ //$NON-NLS-2$
						if (sql.isFile()) {
							runscript(connection, sql, true);
						}
						for (File f: upgradeDir.listFiles()) {
							if (f.isFile()) {
								String nl = f.getName().toLowerCase();
								if ((!nl.startsWith("afs_v")) && nl.endsWith("_v0.sql")) { //$NON-NLS-1$ //$NON-NLS-1$
									runscript(connection, f, true);
									break;
								}
							}
						}
						for (File f: upgradeDir.listFiles()) {
							if (f.isFile()) {
								String nl = f.getName().toLowerCase();
								if ((!nl.startsWith("afs_v")) && nl.endsWith("_v0_" + typ + ".sql")) { //$NON-NLS-1$ //$NON-NLS-1$ //$NON-NLS-1$
									runscript(connection, f, true);
									break;
								}
							}
						}
					} catch (Exception e) {
						printError("An error occurred during database installation: " + e.getLocalizedMessage());
						printError("Contact ARCAD Software support line.");
						if (isArgument("-debug")) { //$NON-NLS-1$
							e.printStackTrace();
						}
					}
					println("Database installed.");
					break;
				} else if (isArgument("-debug")) { //$NON-NLS-1$
					println("The database is not empty... Try to initialize it...");
				}
			case 1: // Database not initialized to first level of compatibility.
				println("The database require an initialization before to be updated to latest version...");
				File f = new File(upgradeDir, "initialize.sql"); //$NON-NLS-1$
				if (!f.isFile()) {
					if (isArgument("-debug")) { //$NON-NLS-1$
						printError("Initialize script: " + f.getAbsolutePath());
					}
					printError("The Database is not installed or must be updated according to the previous database upgrade process.");
					printWarn("No script 'initialize.sql' found.");
					printError("Check the application server configuration.");
					return ERROR_INVALID_CONFIGURATION;
				}
				try {
					runscript(connection, f, true);
					println("Initialize Script Executed (upgrade an old database to the current upgrade management).");
				} catch (Exception e) {
					printError("An error occurred during database preparation: " + e.getLocalizedMessage());
					printError("Contact ARCAD Software support line.");
					printWarn("You have to restore the database backup.");
					if (isArgument("-debug")) { //$NON-NLS-1$
						e.printStackTrace();
					}
				}
			}
			// Temporary Fix: update "DROPS" v0 into "PRODUCT"
			try (Statement st = connection.createStatement()) {
				st.execute("update ARCADDBV set DBV_CODE = 'PRODUCT' where DBV_CODE = 'DROPS' and DBV_VERSION = 0");
				if (isArgument("-debug")) { //$NON-NLS-1$
					println("Rename DROPS DB component to PRODUCT: " + st.getUpdateCount());
				}
			} catch (SQLException e) {
				if (isArgument("-debug")) { //$NON-NLS-1$
					e.printStackTrace();
				}
			}
			Set<String> dbs = getDBCodeList(connection);
			if (dbs.isEmpty()) {
				printError("The Database is not installed or must be updated according to the previous database upgrade process.");
				printError("Check the application server configuration.");
				return ERROR_INVALID_CONFIGURATION;
			}
			String afs = null;
			for (String db: dbs) {
				if ("afs".equalsIgnoreCase(db)) { //$NON-NLS-1$
					afs = db;
				}
			}
			try {
				if (afs != null) {
					upgradeDBComponent(connection, upgradeDir, afs, typ);
					if (isArgument("-debug")) { //$NON-NLS-1$
						println("Database Component AFS updated.");
					}
				}
				// Lister les fichiers de modules qui ne sont pas installés !
				addNewDBFiles(connection, upgradeDir, dbs, typ);
				// et les ajouter à dbs pour les mises à jour !
				for (String db: dbs) {
					if (!db.equals(afs)) {
						upgradeDBComponent(connection, upgradeDir, db, typ);
						if (isArgument("-debug")) { //$NON-NLS-1$
							println("Database Component " + db + " updated.");
						}
					}
				}
				// FIXME comment gérer les problèmes de dépendantes inter composant ET inter versions...
				// Option 1: charger un fichier qui liste l'ordre d'exécution de tous les autre fichiers... il n'y a qu'à suivre se fichier.
				// Option 2: ajouter un header dans chaque fichier qui nécessite un prérequis.
			} catch (Exception e) {
				printError("An error occurred during database upgrade: " + e.getLocalizedMessage());
				printError("Contact ARCAD Software support line.");
				printWarn("You must manually restore the database backup.");
				if (isArgument("-debug")) { //$NON-NLS-1$
					e.printStackTrace();
				}
				return ERROR_INTERNAL_ERROR;
			}
			println("Database update operation successful.");
			return 0;
		} finally {
			try {
				if (close && !connection.isClosed()) {
					connection.close();
				}
			} catch (SQLException e) {
				if (isArgument("-debug")) { //$NON-NLS-1$
					e.printStackTrace();
				}
			}
		}
	}

	private void addNewDBFiles(Connection connection, File upgradeDir, Set<String> dbs, String dbtype) throws Exception {
		Set<String> dbmodules = new HashSet<String>();
		for (File f: upgradeDir.listFiles()) {
			int i = f.getName().indexOf("_V0"); //$NON-NLS-1$
			if (i > 0) {
				String s = f.getName().substring(0, i);
				if (!dbs.contains(s)) {
					dbmodules.add(s);
				}
			}
		}
		for (String module: dbmodules) {
			File sql = new File(upgradeDir, module + "_V0.sql"); //$NON-NLS-1$
			if (sql.isFile()) {
				runscript(connection, sql, true);
			}
			sql = new File(upgradeDir, module + "_V0_" + dbtype + ".sql"); //$NON-NLS-1$ //$NON-NLS-2$
			if (sql.isFile()) {
				runscript(connection, sql, true);
			}
			dbs.add(module);
		}
	}

	private void upgradeDBComponent(Connection connection, File upgradeDir, String db, String dbtype) throws Exception {
		int ver = -1;
		boolean upd = false;
		while (true) {
			ver = getDBVersion(connection, db);
			if (ver < 0) {
				break;
			}
			int nv = ver + 1;
			boolean u = false;
			File sql = new File(upgradeDir, db.toUpperCase() + "_V" + nv + ".sql"); //$NON-NLS-1$ //$NON-NLS-2$
			if (sql.isFile()) {
				runscript(connection, sql, true);
				u = true;
			} else if (isArgument("-debug")) { //$NON-NLS-1$
				try {
					println("Component file does not exist (the component is up to date): " + sql.getCanonicalPath());
				} catch (IOException e) {
					println("Component file does not exist (the component is up to date): " + sql.getAbsolutePath());
				}
			}
			sql = new File(upgradeDir, db.toUpperCase() + "_V" + nv + '_' + dbtype + ".sql"); //$NON-NLS-1$ //$NON-NLS-2$
			if (sql.isFile()) {
				runscript(connection, sql, true);
				u = true;
			} else if (isArgument("-debug")) { //$NON-NLS-1$
				try {
					println("Component file does not exist (the component is up to date): " + sql.getCanonicalPath());
				} catch (IOException e) {
					println("Component file does not exist (the component is up to date): " + sql.getAbsolutePath());
				}
			}
			if (u) {
				upd = true;
			} else {
				break;
			}
		}
		if (upd) {
			println(String.format("The Database component \"%s\" has been upgraded to version %d.", db, ver));
		} else {
			println(String.format("The Database component \"%s\" is already up to date.", db));
		}
	}

	private int upgradeH2DatabaseVersion() {
		if (isArgument("-h2noupgrade", "-h2nup")) { //$NON-NLS-1$ //$NON-NLS-2$
			return 0;
		}
		println("Checks if an upgrade of the H2 database is required...");
		HashSet<String> dbids = new HashSet<>(); 
		final String ds = getArgumentValue(new String[] {"-ds", "-datasource", "-ds.name"}, (String) null);
		if (ds != null) {
			dbids.add(ds);
		} else {
			final Hashtable<String, Object> props = getOSGiConfiguration("com.arcadsoftware.database.sql");
			Enumeration<String> keys = props.keys();
			while (keys.hasMoreElements()) {
				String key = keys.nextElement();
				int i = key.lastIndexOf('.');
				if (i > 0) {
					dbids.add(key.substring(0, i));
				}
			}			
		}
		boolean printHeaderMessages = true;
		for (String id: dbids) {
			// Check if h2 version 1.4 is present.
			// and check if the Datasource is an H2 one.
			if (isH2DataSource(id)) {
				File h2dbFile = getH2DBFile(id);
				// Chekc if we have an old format database of an incorrect mv.db format...
				if (((h2dbFile != null) && h2dbFile.getName().toLowerCase().endsWith(".h2.db")) || //$NON-NLS-1$
						 checkH2VersionUpgrade(id)) {
					if (h2dbFile == null) {
						printError("ERROR: The H2 datasource \"" + id +"\" seems to converted to the latest H2 Database Storage format. This operation can not be performed through a TCP connection. You will have to convert this database mannually. Please refer to the product installation guide for details.");
						if (isArgument("-debug")) {
							println("In case of problem due to a particular installtion constrain, this operation may be avoided by the usage of the command line optioon \"-h2noupgrade\". In that case woull will certainly have to fix the connection to some of the declared data-source in this installation. Please refer to the production installation guide for more information.");
						}
						continue;
					}
					if (printHeaderMessages) {
						printHeaderMessages = false;
						// Print important notification about following operations...
						println("An upgraded of the H2 Database storage is required.");
						println("This operation is done in two steps:");
						println("First a Backup of the database is done with the older H2 Driver.");
						println("Then a Restore of the database is done with the current H2 Driver.");
						println("Once these operations are completed the database content update will start.");
						println("");
						if (isArgument("-debug")) {
							println("In case of problem due to a particular installtion constrain, this operation may be avoided by the usage of the command line optioon \"-h2noupgrade\". In that case woull will certainly have to fix the connection to some of the declared data-source in this installation. Please refer to the production installation guide for more information.");
						}
					}
					// Backup the h2 1.4 database.
					int i = h2dbFile.getName().indexOf('.');
					if (i < 1) {
						i = h2dbFile.getName().length();
					}
					File backup = new File(new File(h2dbFile.getParentFile(), "backup"), h2dbFile.getName().substring(0, i) + "_h2upgrade.sql");
					backup.getParentFile().mkdirs();
					if (backup.isFile()) {
						if (isArgument("-debug")) {
							println("A previous H2 upgrade backup file is present, removing this file...");
						}
						if (!backup.delete()) {
							println("WARNING: Unable to delete the previous backup file: " + backup.getAbsolutePath());
						}
					}
					ArrayList<String> args = new ArrayList<>();
					args.add("com.arcadsoftware.tool.cli.DBH2BackupVersion14199"); //$NON-NLS-1$
					boolean ignore = false;
					for (String arg: getArguments()) {
						if (ignore) {
							ignore = false;
						} else {
							String alc = arg.toLowerCase();
							if (!alc.startsWith("-ds:") && !alc.startsWith("-ds=") && //$NON-NLS-1$ //$NON-NLS-2$
									!alc.startsWith("-ds.name:") && !alc.startsWith("-ds.name=") && //$NON-NLS-1$ //$NON-NLS-2$
									!arg.toLowerCase().startsWith("-datasource:") && !arg.toLowerCase().startsWith("-datasource=")) { //$NON-NLS-1$ //$NON-NLS-2$
								if (alc.equals("-ds") || alc.equals("-datasource") || alc.equals("-ds.name")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
									ignore = true;
								} else {
									args.add(arg);
								}
							}
						}
					}
					args.add("-ds"); //$NON-NLS-1$
					args.add(id);
					args.add("-f");
					args.add(backup.getAbsolutePath());
					args.add("-p=");
					args.add("-noExitIfNoError"); //$NON-NLS-1$
					if (isArgument("-debug")) { //$NON-NLS-1$
						args.add("-debug"); //$NON-NLS-1$
						print("Executing command:");
						for (String arg: args) {
							print(" ");
							print(arg);
						}
						println();
					}
					int result = Exec.exec(args.toArray(new String[0]));
					if ((result != 0) || !backup.isFile()) {
						printError("Unable the generate backup of the previous H2 Database file.");
						if (isArgument("-debug")) { //$NON-NLS-1$
							println("Execution result code = " + result);
						}
						return result;
					}
					// Remove the H2 Database file physically (as no connection to it is possible).
					File hardBackup = new File(h2dbFile.getParentFile(), "backup_" + h2dbFile.getName()); //$NON-NLS-1$
					if (h2dbFile.renameTo(hardBackup)) {
						println("The orignal database file of \"" + id + "\" has been renamed: " + hardBackup.getAbsolutePath());
						println("(You will be able to remove this file when the installation process will be terninated.)");
						h2dbFile = new File(h2dbFile.getParentFile(), h2dbFile.getName().replace(".mv.", ".trace.")); //$NON-NLS-1$ //$NON-NLS-2$
						if (h2dbFile.isFile()) {
							hardBackup = new File(h2dbFile.getParentFile(), "backup_" + h2dbFile.getName()); //$NON-NLS-1$
							if (!h2dbFile.renameTo(hardBackup) && isArgument("-debug")) {
								printError("Unable to rename Trace file: " + h2dbFile.getAbsolutePath());
							}
						}
					} else {
						printError("ERROR: The H2 database file \"" + h2dbFile.getAbsolutePath() +"\" can not be seems to converted to the latest H2 Database Storage format. This operation can not be performed through a TCP connection. You will have to convert this database mannually. Please refer to the product installation guide for details.");
						continue;
					}
					// Restore the DB with current version.
					args.add("-fromx1");
					if (isArgument("-debug")) { //$NON-NLS-1$
						print("Executing command: cli.DBH2Restore");
						for (String arg: args) {
							print(" ");
							print(arg);
						}
						println();
					}
					DBH2Restore c = new DBH2Restore(args.toArray(new String[0]));
					c.exec();
					println("The H2 Data Source \"" + id + "\" has been succefully upgraded to the latest H2 Database Storage format.");
				}
			}
		}
		if (printHeaderMessages) {
			println("No H2 upgrade required.");
		}
		return 0;
	}

	private boolean isH2DataSource(String dbid) {
		Hashtable<String, Object> props = getOSGiConfiguration("com.arcadsoftware.database.sql");
		if (props != null) {
			String dbtype = (String) props.get(dbid + KEY_DATABASETYPE);
			if (dbtype != null) {
				return dbtype.toLowerCase().startsWith("h2"); //$NON-NLS-1$
			}
			String url = props.get(dbid + KEY_DATABASEURL).toString();
			if (url == null) {
				return true;
			}
			if (url.startsWith("jdbc:h2:")) { //$NON-NLS-1$
				return true;
			}
			return !url.startsWith("jdbc:as400:") && //$NON-NLS-1$
					!url.startsWith("jdbc:oracle:") && //$NON-NLS-1$
					!url.startsWith("jdbc:sqlserver:") && //$NON-NLS-1$
					!url.startsWith("jdbc:jtds:sqlserver:") && //$NON-NLS-1$
					!url.startsWith("jdbc:mysql:") && //$NON-NLS-1$
					!url.startsWith("jdbc:postgresql:") && //$NON-NLS-1$
					!url.startsWith("jdbc:hsqldb:") && //$NON-NLS-1$
					!url.startsWith("jdbc:odbc:");
		}
		return false;
	}

	// There is multiple way to check the H2 databse version, non is perfect:
	
	// Option 1: look for the presence of the older H2 jar file... this require that the update program do not remove it !
	
	// Option 2: Check the DB file header: The mv.db files have two identical 4096-byte blocks in the beginning. 
	// We can read one of them, discard trailing bytes 0x00, and convert previous bytes to a string.
	// This string contains comma-separated properties in the name:value format. Then we can check value of the property "format:1",
	// if the value is "1" the database require an upgrade.
	// This option does not work with oldest database (pre "MV" format") and encrypted database file must be converted first !
	
	// Option 3: try to open the database, if the operation fail then this database certainly require an Upgrade...
	private boolean checkH2VersionUpgrade(String dbid) {
		// We have to mimic the default connection algorithm...
		try {
			final String dslogin = getArgumentValue(new String[] {"-ds.login", "-datasource.login"}, (String) null);
			String dspwd = getArgumentValue(new String[] {"-ds.pwd", "-datasource.password"}, (String) null);
			if ((dslogin != null) && (dspwd == null)) {
				char[] pwdc = readSecret("Enter the password of the Database user \"" + dslogin + "\": ");
				if (pwdc == null) {
					return false;
				}
				dspwd = new String(pwdc);
				// Avoid multiple user input for same data...
				addArgument("-ds.pwd", dspwd);
			}
			Hashtable<String, Object> props = getOSGiConfiguration("com.arcadsoftware.database.sql");
			if (props != null) {
				String url = (String) props.get(dbid + KEY_DATABASEURL);
				if (url != null) {
					url = url.trim();
					if ((url.length() < 5) || !"jdbc".equals(url.substring(0, 4).toLowerCase())) { //$NON-NLS-1$
						url = "jdbc:h2:" + url; //$NON-NLS-1$
					}
				} else {
					url = "jdbc:h2:" + new File(getHomeDirectory(), "database/" + dbid).getAbsolutePath(); //$NON-NLS-1$ //$NON-NLS-2$
				}
				if (url.startsWith("jdbc:h2:mem")) {
					return false;
				}
				Properties cnp = new Properties();
				String login = (String) props.get(dbid + KEY_DATABASELOGIN);
				if ((login == null) || login.isEmpty()) {
					login = "sa"; //$NON-NLS-1$
				} else {
					login = props.get(dbid + KEY_DATABASELOGIN).toString();
				}
				cnp.put("user", login);
				String pwd = (String) props.get(dbid + KEY_DATABASEPWD).toString();
				if (pwd != null) {
					pwd = new String(Crypto.decrypt(pwd));
					cnp.put("password", pwd);
				}
				// Mimic all production default rules for H2...
				checkH2ServerConfiguration();
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
				Connection cn = null;
				try {
					if (dslogin != null) {
						cnp.put("user", dslogin);
						cnp.put("password", dspwd);
						try {
							println("Check connection to the H2 Data Source \"" + dbid + "\" with user \"" + dslogin + "\".");
							cn = DriverManager.getConnection(url, cnp);
						} catch (SQLException e) {
							println("Unable to connect to \"" + dbid + "\" with user \"" + dslogin + "\".");
							if (isArgument("-debug")) { //$NON-NLS-1$
								println("The following exception stacktrace is the result of the unsuccessful connection test:");
								e.printStackTrace();
							}
							println("Assuming that this H2 database need to be upgraded...");
							return true;
						}
					} else {
						try {
							println("Check connection to the H2 Data Source \"" + dbid + "\".");
							cn = DriverManager.getConnection(url, cnp);
						} catch (SQLException e) {
							println("Unable to connect to the Data Source \"" + dbid + "\".");
							if (isArgument("-debug")) { //$NON-NLS-1$
								println("The following exception stacktrace is the result of the unsuccessful connection test:");
								e.printStackTrace();
							}
							println("Assuming that this H2 database need to be upgraded...");
							return true;
						}
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
			return false;
		} finally {
			stopH2Server();
		}
	}
	
	private File getH2DBFile(String dbid) {
		Hashtable<String, Object> props = getOSGiConfiguration("com.arcadsoftware.database.sql");
		String url = (String) props.get(dbid + KEY_DATABASEURL);
		File dbFile = computeH2DBFile(dbid, url, ".mv.db"); //$NON-NLS-1$
		if (dbFile.isFile()) {
			return dbFile;
		}
		dbFile = computeH2DBFile(dbid, url, ".h2.db"); //$NON-NLS-1$
		if (dbFile.isFile()) {
			return dbFile;
		}
		return null;
	}

	private File computeH2DBFile(String dbid, String url, String extension) {
		if (url == null) {
			return new File(getHomeDirectory(), "database/" + dbid + extension); //$NON-NLS-1$
		} else {
			url = url.trim();
			int i = 0;
			if (url.startsWith("jdbc:h2:tcp://")) { //$NON-NLS-1$
				// Assume a local server... hosted by this application...
				i = url.indexOf('/', url.indexOf('/') + 2);
				if (i < 13) {
					println("WARN: Unable to automatically urgrade a from an Older H2 database version if the connexion is a non local one (i.e. using a TCP-IP connection). You have to upgrade the database \""+dbid+"\" mannually. Please refer to the product installation documentation for details.");
					return null;
				}
				int j = url.indexOf(';');
				if ((j > 0) && (i > j)) {
					println("WARN: Unable to automatically urgrade a from an Older H2 database version if the connexion is a non local one (i.e. using a TCP-IP connection). You have to upgrade the database \""+dbid+"\" mannually. Please refer to the product installation documentation for details.");
					return null;
				}
			} else if (url.startsWith("jdbc:h2:file:")) { //$NON-NLS-1$
				i = 13;
			} else if (url.startsWith("jdbc:h2:")) { //$NON-NLS-1$
				i = 8;
			}
			int j = url.indexOf(';');
			if (j > 0) {
				url = url.substring(i, j);
			} else {
				url = url.substring(i);
			}
			if (url.length() == 0) {
				return new File(getHomeDirectory(), "database/" + dbid + extension); //$NON-NLS-1$
			}
			if (((url.length() > 2) && (url.charAt(1) == ':')) || // windows path.
						(url.charAt(0) == '/') || (url.charAt(0) == '\\') || // absolute path.
						(url.charAt(0) == '~')) { // user home dir.
				return new File(url + extension);
			}
			return new File(getHomeDirectory(), url + extension);
		}
	}
	
	
}
