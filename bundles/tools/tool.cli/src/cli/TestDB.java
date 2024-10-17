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
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;

import com.arcadsoftware.tool.cli.DataSourceCommand;

public class TestDB extends DataSourceCommand {

	public static void main(String[] args) {
		System.exit(new TestDB(args).exec());
	}

	public TestDB() {
		super();
	}

	public TestDB(String[] args) {
		super(args);
	}

	@Override
	protected int runDataSourceCommand(String dataSourceID, String dataSourceType, Connection connection, String url, Properties connectionProperties) {
		println("Testing Data Soure: " + dataSourceID + " [" + dataSourceType + "]:");
		long t = System.currentTimeMillis();
		try {
			try (Statement st = connection.createStatement()) {
				st.executeUpdate("create table TESTDBTEMP (test_col varchar(200))"); //$NON-NLS-1$
				println("  Table creation is working...");
			}
		} catch (SQLException e) {
			printError("  ERROR: Unable to create a Table in the DataSource.");
			return 0;
		}
		try {
			try (Statement st = connection.createStatement()) {
				st.executeUpdate("insert into TESTDBTEMP (test_col) values ('test string, you can drop this table.')"); //$NON-NLS-1$
				println("  Data Insertion is working...");
			}
		} catch (SQLException e) {
			printError("  ERROR: Unable to insert data in the DataSource.");
		}
		try {
			Statement st = connection.createStatement();
			try {
				st.executeUpdate("update TESTDBTEMP set test_col='you can drop this table.'"); //$NON-NLS-1$
				println("  Data Update is working...");
			} finally {
				st.close();
			}
		} catch (SQLException e) {
			printError("  ERROR: Unable to update data in the DataSource.");
		}
		try {
			Statement st = connection.createStatement();
			try {
				st.executeQuery("select * from TESTDBTEMP"); //$NON-NLS-1$
				println("  Data Selection is working...");
			} finally {
				st.close();
			}
		} catch (SQLException e) {
			printError("  ERROR: Unable to select data in the DataSource.");
		}
		try {
			Statement st = connection.createStatement();
			try {
				st.executeUpdate("delete from TESTDBTEMP"); //$NON-NLS-1$
				println("  Data Deletion is working...");
			} finally {
				st.close();
			}
		} catch (SQLException e) {
			printError("  ERROR: Unable to delete data in the DataSource.");
		}
		try {
			Statement st = connection.createStatement();
			try {
				st.executeUpdate("drop table TESTDBTEMP"); //$NON-NLS-1$
				println("  Table Deletion is working...");
			} finally {
				st.close();
			}
		} catch (SQLException e) {
			printError("  ERROR: Unable to drop table in the DataSource (the TESTDBTEMP table must be deleted manually).");
		}
		try {
			Statement st = connection.createStatement();
			try {
				ResultSet rs = st.executeQuery("select ABV_ID, ABV_NAME from ARCADDBV order by ABV_ID desc"); //$NON-NLS-1$
				if (rs.next()) {
					println(String.format("  DataSource Version %s (with ID: %d).", rs.getString(2), rs.getInt(1)));
				} else {
					println("  No DataSource version information available (Empty version).");
				}
				rs.close();
			} finally {
				st.close();
			}
		} catch (SQLException e) {
			printError("  No DataSource version information available (Unversioned).");
		}
		t = System.currentTimeMillis() - t;
		println(String.format("(Operations performed in %dms.", t));
		return 0;
	}

	@Override
	protected String getVersion() {
		return "1.0.0"; //$NON-NLS-1$
	}

	@Override
	protected String getCommandFullName() {
		return "testds";
	}

	@Override
	protected String getCommandDescription() {
		return "This command test the Data source connection.";
	}

}
