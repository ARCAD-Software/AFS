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

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.tool.cli.DataSourceCommand;

public class ConfigAdminUser extends DataSourceCommand {

	public static void main(String[] args) {
		System.exit(new ConfigAdminUser(args).exec());
	}
	
	public ConfigAdminUser() {
		super();
	}
	
	public ConfigAdminUser(String[] args) {
		super(args);
	}

	@Override
	protected String getVersion() {
		return "1.0.2"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandFullName() {
		return "setloginpwd"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandDescription() {
		return "This command allow to edit a login/password declaration into the database.";
	}

	@Override
	protected Map<String, String> getArgumentsDescription() {
		HashMap<String, String> result = new HashMap<String, String>();
		result.put("[-p|-password <pass phrase>]", //$NON-NLS-1$
				"The password to update into the database. This parameter is optional, if not given it will be asked in the console.");
		result.put("[-id <integer>]", //$NON-NLS-1$
				"The internal user ID in the database to update. This parameter is optional, the default value is 1 (one). This parameter is not used if a login is given.");
		result.put("[-l|-login <user login>]", //$NON-NLS-1$
				"The login to update in the database. This parameter is optional, if not given the ID is used.");
		return result;
	}

	@Override
	protected int runDataSourceCommand(String dataSourceID, String dataSourceType, Connection connection, String url, Properties connectionProperties) {
		String login = getArgumentValue(new String[] {"-l", "-login"}, ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		int id = getArgumentValue("-id", 1); //$NON-NLS-1$
		char[] pwd = getArgumentValue(new String[] {"-p", "-password"}, new char[0]); //$NON-NLS-1$ //$NON-NLS-2$
		if (pwd.length == 0) {
			pwd = readSecret("Enter the new password: ");
			if (pwd.length == 0) {
				printError("A new user password is required to launch the test."); 
				return ERROR_MISSING_PARAMETER;
			}
			char[] repwd = readSecret("Confirm the new password: ");
			if (!Arrays.equals(pwd, repwd)) {
				printError("The given passwords do not matches."); 
				return ERROR_WRONG_PARAMETER;
			}
		}
		String sql;
		if (login.isEmpty()) {
			sql = "update LOCALAUTH set LAU_PASSWORD = ? , LAU_LOCKED = 0 where LAU_USER = ?"; //$NON-NLS-1$
			if (isArgument("-debug")) { //$NON-NLS-1$
				println("Updated passowrd using User ID.");
			}
		} else {
			sql = "update LOCALAUTH set LAU_PASSWORD = ? , LAU_LOCKED = 0 where LAU_LOGIN = ?"; //$NON-NLS-1$
			if (isArgument("-debug")) { //$NON-NLS-1$
				println("Updated passowrd using Login.");
			}
		}
		try (PreparedStatement ps = connection.prepareStatement(sql)) {
			ps.setString(1, Crypto.hash(pwd));
			if (login.isEmpty()) {
				ps.setInt(2, id);
			} else {
				ps.setString(2, login);
			}
			ps.execute();
			if (ps.getUpdateCount() > 0) {
				print("The password has been updated in the database.");
				return 0;
			}
			printError("No corresponding lines into the database.");
			return ERROR_WRONG_PARAMETER;
		} catch (SQLException e) {
			printError("SQL Error when updating the database: " + e.getLocalizedMessage());
			if (isArgument("-debug")) { //$NON-NLS-1$
				e.printStackTrace();
			}
			return ERROR_INTERNAL_ERROR;
		}
	}
}
