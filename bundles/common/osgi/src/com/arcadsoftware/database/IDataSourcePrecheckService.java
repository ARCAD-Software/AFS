package com.arcadsoftware.database;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Dictionary;

import javax.sql.DataSource;

/**
 * This OSGi service is used to validate a DataSource that is going to be registered as an OSGi service.
 * 
 * <p>
 * It van be use to perform any required operation to upgrade the database just before the server is going to use it, on just
 * validate the DataSource version, if an Exception is thrown by this service then the DataSource will not be registered.
 * 
 * <p>
 * Note that as this service is used when the server is starting it must be registered before the configuration of the 
 * "com.arcadsoftware.database.sql" bundle is loaded. Once the DataSource is registered this service is useless. 
 * 
 * @author ARCAD Software
 */
public interface IDataSourcePrecheckService {

	/**
	 * Get the version of the given database module in this datasource.
	 *  
	 * @param datasource
	 * @param module
	 * @return -1 if the module is not found, -2 if an error occurs, like the database is empty or there is no ARCADDBV table in it.
	 */
	public static int getDatabaseVersion(final DataSource datasource, final String module) {
		try (Connection c = datasource.getConnection()) {
			try (PreparedStatement s = c.prepareStatement("select max(DBV_VERSION) from ARCADDBV where DBV_CODE = ?")) {
				s.setString(1, module);
				try (ResultSet rs = s.executeQuery()) {
					if (rs.next()) {
						return rs.getInt(1);
					}
				}
			}
		} catch (SQLException e) {
			return -2;
		}
		return -1;
	}
	
	/**
	 * Check the state of the new Datasource just before it is registered as an OSGi Service.
	 * 
	 * <p>
	 * <strong>If an SQLException if thrown then the DataSource will not be registered as an OSGi service.
	 *  
	 * @param ds A non null Datasource Object.
	 * @param properties The properties that are going to be set to the OSGi service, these properties can be modified or completed by this service implementation.
	 * @throws SQLException If the datasource is invalid and should not be registered as an OSGi service. An Error log will be recorded, except is the Exception message is equals to "[ignore]".
	 */
	public void check(final DataSource ds, final Dictionary<String, Object> properties) throws SQLException;
}
