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
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Date;
import java.util.Map;
import java.util.Properties;

import com.arcadsoftware.tool.cli.DataSourceCommand;

public final class DBH2Backup extends DataSourceCommand {

	public static void main(String[] args) {
		new DBH2Backup(args).exec();
	}
	
	public DBH2Backup() {
		super();
	}
	
	public DBH2Backup(String[] args) {
		super(args);
	}

	@Override
	protected String getVersion() {
		return "1.0.0"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandFullName() {
		return "h2backup"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandDescription() {
		return "This command allow to backup the application database.";
	}

	@Override
	protected Map<String, String> getArgumentsDescription() {
		Map<String, String> result = super.getArgumentsDescription();
		result.put("[-p|-password <password>]", //$NON-NLS-1$
				"Specify the password used to encrypt the backup file. By default the H2 database connection password is used.");
		result.put("[-f|-file <file path>]", //$NON-NLS-1$
				"Specify a destination backup file path. By default a time stamped file name will be used.");
		return result;
	}

	@Override
	protected int runDataSourceCommand(String dataSourceID, String dataSourceType, Connection connection, String url, Properties connectionProperties) {
		if (dataSourceType.toLowerCase().startsWith("h2")) { //$NON-NLS-1$
			String pwd = getArgumentValue(new String[] {"-p", "-password"}, connectionProperties.getProperty("password", null)); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			String file = getArgumentValue(new String[] {"-f", "-file"}, //$NON-NLS-1$ //$NON-NLS-2$
					String.format("./database/backup/%1$s_%2$tY%2$tm%2$td_%2$tH%2$tM%2$tS.zip", dataSourceID, new Date())); //$NON-NLS-1$
			File backupFile = new File(file);
			try {
				backupFile.getParentFile().mkdirs();
				if (backupFile.isFile()) {
					if (backupFile.delete()) {
						println("The previous backup file has been deleted.");
					} else {
						printError("Can not delete the previous backup file: " + backupFile.getAbsolutePath());
						return ERROR_FILESYSTEM_ACCESS;
					}
				}
				if ((pwd != null) && !pwd.isEmpty()) {
					try (PreparedStatement ps = connection.prepareStatement("script to ? compression deflate cipher AES password ?")) { //$NON-NLS-1$
						ps.setString(1, backupFile.getAbsolutePath());
						ps.setString(2, pwd);
						ps.execute();
					}
				} else {
					println("The Backup file is not secured by a password.");
					try (PreparedStatement ps = connection.prepareStatement("script to ? compression deflate")) { //$NON-NLS-1$
						ps.setString(1, backupFile.getAbsolutePath());
						ps.execute();
					}
				}
			} catch (SQLException e) {
				printError("Unable to backup the H2 database: " + e.getLocalizedMessage());
				if (isArgument("-debug")) { //$NON-NLS-1$
					e.printStackTrace();
				}
				return ERROR_INTERNAL_ERROR;
			}
			try {
				println("H2 Database backup created in file: " + backupFile.getCanonicalPath());
			} catch (IOException e) {
				println("H2 Database backup created in file: " + backupFile.getAbsolutePath());
			}
		} else {
			println("The data source \"" + dataSourceID + "\" [" + dataSourceType + "] can not be proceeded by this tool, you have to make a manual backup.");
		}
		return 0;
	}
	
}
