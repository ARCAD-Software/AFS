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

import java.lang.reflect.Constructor;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.crypt.RandomGenerator;
import com.arcadsoftware.tool.cli.DataSourceCommand;

public final class ChangeDBPassword extends DataSourceCommand {

	public static void main(String[] args) {
		System.exit(new ChangeDBPassword(args).exec());
	}

	private final HashMap<String, Object> confChanged = new HashMap<String, Object>();
	private char[] newPwd;
	
	public ChangeDBPassword() {
		super();
	}
	
	public ChangeDBPassword(String[] args) {
		super(args);
	}

	@Override
	protected String getVersion() {
		return "1.0.0"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandFullName() {
		return "dbpwd"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandDescription() {
		return "This command allow to change the password required to access to the database. It use the current password to connect to the database dans change it.";
	}

	@Override
	protected Map<String, String> getArgumentsDescription() {
		Map<String, String> result = super.getArgumentsDescription();
		result.put("[-pwd|-password <password>]", //$NON-NLS-1$
				"Specify the new password used to replace tu current one. If no -ds argument is used and the application define multiple data source the same password will be used for all of them. If not specified the password will be asked to the prompt command.");
		result.put("[-gen|-generate]", //$NON-NLS-1$
				"If used this parameter will generate a random password for the data sources connection, ignoring any provided password and disabling any prompt."); //$NON-NLS-1$
		return result;
	}

	@Override
	protected int run() {
		newPwd = getArgumentValue(new String[] {"-pwd", "-npwd", "-new.password", "-password"}, (char[]) null); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		try {
			return super.run();
		} finally {
			if (!confChanged.isEmpty()) {
				Hashtable<String, Object> properties = getOSGiConfiguration("com.arcadsoftware.database.sql"); //$NON-NLS-1$ 
				for (Entry<String, Object> e: confChanged.entrySet()) {
					properties.put(e.getKey(), e.getValue());
				}
				saveOSGiConfiguration();
				println("Application server configuration updated with new password.");
			}
		}
	}

	@Override
	protected int runDataSourceCommand(String dataSourceID, String dataSourceType, Connection connection, String url, Properties connectionProperties) {
		println("Connected to the data source: %s [%S].", dataSourceID, dataSourceType);
		String login = (String) connectionProperties.get("user"); //$NON-NLS-1$
		String pwd;
		if (isArgument("-gen", "-generate")) {
			pwd = new String(RandomGenerator.complexRandonPassword());
			if (isArgument("-debug")) { //$NON-NLS-1$
				println("New password randomly generated.");
			}
		} else if (newPwd == null) {
			char[] pwdc = readSecret("Enter the new password used for the Data Source \"" + dataSourceID + "\": ");
			if (pwdc == null) {
				printError("ERROR: no password provided !");
				return ERROR_MISSING_PARAMETER;
			}
			char[] pwd2 = readSecret("Confirm this new password: ");
			if (!Arrays.equals(pwdc, pwd2)) {
				printError("The given passwords do not matches."); 
				return ERROR_WRONG_PARAMETER;
			}
			pwd = new String(pwdc);
		} else {
			pwd = new String(newPwd);
			if (isArgument("-debug")) { //$NON-NLS-1$
				println("Using password given in command line arguments.");
			}
		}
		if (dataSourceType.startsWith("h2")) { //$NON-NLS-1$
			try (PreparedStatement ps = connection.prepareStatement("ALTER USER \"" + login + "\" SET PASSWORD ?")) { //$NON-NLS-1$ //$NON-NLS-2$
				ps.setString(1, pwd);
				ps.executeUpdate();
				if (isArgument("-debug")) { //$NON-NLS-1$
					println("Password updated in H2 database.");
				}
			} catch (SQLException e) {
				printError("ERROR Unable to change H2 database password: " + e.getLocalizedMessage());
				return ERROR_DATABASE_CORRUPTED;
			}
		} else if ("postgresql".equalsIgnoreCase(dataSourceType)) { //$NON-NLS-1$
			try (PreparedStatement ps = connection.prepareStatement("ALTER USER \"" + login + "\" WITH PASSWORD ?")) { //$NON-NLS-1$ //$NON-NLS-2$
				ps.setString(1, pwd);
				ps.executeUpdate();
				if (isArgument("-debug")) { //$NON-NLS-1$
					println("Password updated in PostgreSQL database.");
				}
			} catch (SQLException e) {
				printError("ERROR Unable to change PostGreSQL database password: " + e.getLocalizedMessage());
				return ERROR_DATABASE_CORRUPTED;
			}
		} else if ("jt400".equalsIgnoreCase(dataSourceType)) { //$NON-NLS-1$
			// call: new com.ibm.as400.access.AS400().changePassword(char[], char[])
			try {
				final Class<?> clazz = getClass().getClassLoader().loadClass("com.ibm.as400.access.AS400");
				final Object as400;
				if (url.startsWith("jdbc:jt400://localhost")) {
					as400 = clazz.getConstructor().newInstance();
				} else {
					String server = url.substring(13);
					int i = server.indexOf('/');
					if (i > -1) {
						server = server.substring(0, i);
					}
					i = server.indexOf(';');
					if (i > -1) {
						server = server.substring(0, i);
					}
					Constructor<?> cons = clazz.getConstructor(String.class, String.class, char[].class);
					as400 = cons.newInstance(server, login, ((String) connectionProperties.get("user")).toCharArray());
				}
				clazz.getMethod("changePassword", char[].class, char[].class).invoke(as400, ((String) connectionProperties.get("user")).toCharArray(), pwd.toCharArray());
				if (isArgument("-debug")) { //$NON-NLS-1$
					println("Password updated in IBMi.");
				}
			} catch (Exception e) {
				printError("ERROR Unable to change BD2i database password: " + e.getLocalizedMessage());
				return ERROR_DATABASE_CORRUPTED;
			}
		} else {
			printError("WARNING Unsupported Data Source type: " + dataSourceType);
			return ERROR_INTERNAL_ERROR;
		}
		println("Data source Password changed.");
		confChanged.put(dataSourceID + KEY_DATABASEPWD, Crypto.encrypt(pwd.toCharArray()));
		// TODO Try to reconnect with the new password !?!??
		return 0;
	}
}
