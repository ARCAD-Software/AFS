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
import java.util.Date;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import com.arcadsoftware.tool.cli.DataSourceCommand;

public final class DBUpdate extends DataSourceCommand {

	public static void main(String[] args) {
		new DBUpdate(args).exec();
	}
	
	public DBUpdate() {
		super();
	}
	
	public DBUpdate(String[] args) {
		super(args);
	}

	@Override
	protected String getVersion() {
		return "1.0.2"; //$NON-NLS-1$
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
	protected int runDataSourceCommand(String dataSourceID, String dataSourceType, Connection connection, String url, Properties connectionProperties) {
		// H2 database Backup.
		boolean close = false;
		try {
			final File upgradeDir = new File(getHomeDirectory(), "database/upgrade"); //$NON-NLS-1$
			if (!upgradeDir.isDirectory()) {
				printError("The Database is not installed or must be updated according to the previous database upgrade process.");
				printError("Upgrade SQL script not found.");
				return ERROR_CANCELLED;
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
				if (!isArgument("-ncdb", "-nocleandb")) { //$NON-NLS-1$ //$NON-NLS-2$
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
						try (PreparedStatement ps = connection.prepareStatement("shutdown")) { //$NON-NLS-1$
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
			}
			String typ = "h2"; //$NON-NLS-1$
			if ("postgresql".equals(dataSourceType)) { //$NON-NLS-1$
				typ = "pg"; //$NON-NLS-1$
				if (isArgument("-debug")) { //$NON-NLS-1$
					println("Starting PostgreSQL database upgrade...");
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
	
}
