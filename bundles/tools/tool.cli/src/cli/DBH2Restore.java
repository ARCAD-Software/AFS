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
import java.util.Map;
import java.util.Properties;

import com.arcadsoftware.tool.cli.DataSourceCommand;

public final class DBH2Restore extends DataSourceCommand {

	public static void main(String[] args) {
		System.exit(new DBH2Restore(args).exec());
	}
	
	public DBH2Restore() {
		super();
	}
	
	public DBH2Restore(String[] args) {
		super(args);
	}

	@Override
	protected String getVersion() {
		return "1.0.0"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandFullName() {
		return "h2restore"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandDescription() {
		return "This command allow to restore the application database.";
	}

	@Override
	protected Map<String, String> getArgumentsDescription() {
		Map<String, String> result = super.getArgumentsDescription();
		result.put("[-f|-file <file path>]", //$NON-NLS-1$
				"Define the backup file path. By default the file path will be prompted.");
		result.put("[-p|-password <password>]", //$NON-NLS-1$
				"Specify a Database backup password. By default the H2 database connection password is used.");
		return result;
	}

	@Override
	protected int runDataSourceCommand(String dataSourceID, String dataSourceType, Connection connection, String url, Properties connectionProperties) {
		if (dataSourceType.toLowerCase().startsWith("h2")) { //$NON-NLS-1$
			String pwd = getArgumentValue(new String[] {"-p", "-password"}, connectionProperties.getProperty("password", null)); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			String file = getArgumentValue(new String[] {"-f", "-file"}, (String) null); //$NON-NLS-1$ //$NON-NLS-2$
			if (file == null) {
				file = read("Enter the path to the backup file (this path can be a relative to the application home directory): ");
				if (file == null) {
					return 0;
				}
			}
			File backupFile = new File(file);
			if (!backupFile.isFile()) {
				try {
					printError("Backup file not found: " + backupFile.getCanonicalPath());
				} catch (IOException e) {
					printError("Backup file not found: " + backupFile.getAbsolutePath());
				}
				return ERROR_WRONG_PARAMETER;
			}
			try (PreparedStatement ps = connection.prepareStatement("drop all objects delete files")) { //$NON-NLS-1$
				ps.execute();
			} catch (SQLException e) {
				println("Unabled to clean up the previous database.");
				if (isArgument("-debug")) { //$NON-NLS-1$
					e.printStackTrace();
				}
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
			StringBuilder extensions = new StringBuilder();
			if (isArgument("-fromx1")) { //$NON-NLS-1$
				extensions.append(" FROM_1X"); //$NON-NLS-1$
			}
			// Recreate the database connection:
			try (Connection cn = DriverManager.getConnection(url, connectionProperties)) {
				try {
					if ((pwd != null) && !pwd.isEmpty()) {
						try (PreparedStatement ps = cn.prepareStatement("runscript from ? compression deflate cipher AES password ?" + extensions.toString())) { //$NON-NLS-1$
							ps.setString(1, backupFile.getAbsolutePath());
							ps.setString(2, pwd);
							ps.execute();
						}
					} else {
						try (PreparedStatement ps = cn.prepareStatement("runscript from ? compression deflate" + extensions.toString())) { //$NON-NLS-1$
							ps.setString(1, backupFile.getAbsolutePath());
							ps.execute();
						}
					}
				} catch (SQLException e) {
					printError("Unable to restore the H2 database: " + e.getLocalizedMessage());
					printWarn("You may have to restore manually the previous backup.");
					if (isArgument("-debug")) { //$NON-NLS-1$
						e.printStackTrace();
					}
					return ERROR_INTERNAL_ERROR;
				}
			} catch (SQLException e) {
				printError("Unable to reconnect to the H2 database: " + e.getLocalizedMessage());
				printWarn("You may have to restore the previous backup.");
				if (isArgument("-debug")) { //$NON-NLS-1$
					e.printStackTrace();
				}
				return ERROR_INTERNAL_ERROR;
			}
			println("The data source \"" + dataSourceID + "\" [" + dataSourceType + "] is restored.");
		} else {
			println("The data source \"" + dataSourceID + "\" [" + dataSourceType + "] can not be proceeded by this tool, you have to make a manual backup.");
		}
		return 0;
	}
}
