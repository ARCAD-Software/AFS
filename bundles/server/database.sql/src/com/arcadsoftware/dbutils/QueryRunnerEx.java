/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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
package com.arcadsoftware.dbutils;

import java.io.InputStream;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import javax.sql.DataSource;

import org.apache.commons.dbutils.QueryRunner;

/**
 * Executes SQL queries with pluggable strategies for handling 
 * <code>ResultSet</code>s.  This class is thread safe.
 * 
 * @see org.apache.commons.dbutils.QueryRunner
 * @see org.apache.commons.dbutils.ResultSetHandler
 */
public class QueryRunnerEx extends QueryRunner {

	/**
	 * 
	 */
	public QueryRunnerEx() {
		super();
	}

	/**
	 * @param ds
	 */
	public QueryRunnerEx(DataSource ds) {
		super(ds);
	}

	/**
	 * Get a blob object as an inputStream.
	 * 
	 * @param conn
	 * @param sql
	 * @param params
	 * @return
	 * @throws SQLException
	 */
	public InputStream getBlob(Connection conn, String sql, Object[] params) throws SQLException {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = prepareStatement(conn, sql);
            fillStatement(stmt, params);
            rs = wrap(stmt.executeQuery());
        	return rs.getBinaryStream(1);
        } catch (SQLException e) {
            rethrow(e, sql, params);
        } finally {
            try {
                close(rs);
            } finally {
                close(stmt);
            }
        }
        return null;
	}
	
	/**
	 * 
	 * @param sql
	 * @param params
	 * @return
	 * @throws SQLException
	 */
	public InputStream getBlob(String sql, Object[] params) throws SQLException {
        Connection conn = prepareConnection();
        try {
            return getBlob(conn, sql, params);
        } finally {
            close(conn);
        }
	}	
	
	/**
	 * This helper execute method perform a complex sql operation in which updates, insert and select can be done.
	 * A stocked procedure can be called. 
	 * 
	 * Only the first integer result corresponding to the "idSting" name is returned.
	 * 
	 * @param conn
	 * @param sql string
	 * @param params
	 * @param idString the id column name.
	 * @return the corresponding Id
	 * @throws SQLException
	 */
	public int insert(Connection conn, String sql, Object[] params, String idString) throws SQLException {
		boolean tryAK = false;
		try {
			DatabaseMetaData dmd = conn.getMetaData();
			tryAK = ((dmd != null) && (dmd.supportsGetGeneratedKeys()));
		} catch (SQLException e) {}
		if (tryAK) {
			PreparedStatement stmt = null;
			boolean usedID = false;
	        try {
	        	if (idString != null) {
		            try {
		            	stmt = conn.prepareStatement(sql, new String[] {idString});
		            	usedID = true;
		            } catch (SQLException e) {
		            	try  {
				            stmt = conn.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS);
		            	} catch (SQLException z) {
		            		throw e;
		            	}
		            }
	        	} else {
		            stmt = conn.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS);
	        	}
	        	try {
	        		fillStatement(stmt, params);
	        	} catch (SQLException e) {
	        		// In PostgreSQL the ID col generate an error here !
	        		if (usedID) {
	        			try {
	        				stmt.close();
	        			} catch (SQLException z) {}
	        			try {
				            stmt = conn.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS);
			        		fillStatement(stmt, params);
	        			} catch (SQLException z) {
	        				throw e;
	        			}
	        		} else {
	        			throw e;
	        		}
	        	}
	            boolean result = stmt.execute();
	            ResultSet rs = stmt.getGeneratedKeys();
	            try {
	            	if ((rs != null) && rs.next()) {
		           		int key = rs.getInt(1);
		           		if (key != 0) {
			           		return key;
		           		}
	               	}
	            	close(rs);
	            	if ((idString != null) && result) {
		           	   	rs = stmt.getResultSet();
		           	   	if ((rs != null) && rs.next()) {
			           		try {
			           			return rs.getInt(idString);
			           		} catch (SQLException e) {
			           			return rs.getInt(1);
			           		}
		           	   	}
	            	}
	            } catch (SQLException e) {
	            } finally {
	            	close(rs);
	            }
	        } catch (SQLException e) {
	            rethrow(e, sql, params);
	        } finally {
	            close(stmt);
	        }
		} else {
			// old school way...
			PreparedStatement stmt = null;
	        try {
	            stmt = this.prepareStatement(conn, sql);
	            fillStatement(stmt, params);
	            if (stmt.execute()) {
	                do { 
	            	   	ResultSet rs = stmt.getResultSet();
	            	   	if (rs != null) {
			            	try {
			            		if (rs.next()) {
			            			if (idString != null) {
			            				try {
			            					return rs.getInt(idString);
			            				} catch (SQLException e) {}
			            			}
			            			return rs.getInt(1);
			            		}
			            		return 0;
			            	} catch (SQLException e) {
			            	} finally {
			            		close(rs);
			            	}
	            	   	}
	                } while (stmt.getMoreResults() || (stmt.getUpdateCount() > -1));
	            }

	        } catch (SQLException e) {
	            rethrow(e, sql, params);
	        } finally {
	            close(stmt);
	        }
		}
        return 0;
	}

	/**
	 * This helper execute method perform a complex sql operation in which updates, insert and select can be done.
	 * A stocked procedure can be called. 
	 * 
	 * Only the first integer result corresponding to the "idSting" name is returned.
	 * 
	 * @param sql
	 * @param params
	 * @param idString the id column name.
	 * @return the corresponding Id
	 * @throws SQLException
	 */
	public int insert(String sql, Object[] params, String idString) throws SQLException {
        Connection conn = prepareConnection();
        try {
            return insert(conn, sql, params, idString);
        } finally {
            close(conn);
        }
	}

	/**
	 * This helper execute method perform a complex sql operation in which updates, insert and select can be done.
	 * A stocked procedure can be called. 
	 * 
	 * Only the first integer result corresponding to the "idSting" name is returned.
	 * 
	 * @param sql
	 * @param params
	 * @param idString the id column name.
	 * @return the corresponding Id
	 * @throws SQLException
	 */
	public int insert(String sql, String idString) throws SQLException {
		return insert(sql, null, idString);
	}

	public void updateBlob(Connection conn, String sql, int id, InputStream inputStream, int length) throws SQLException {
        PreparedStatement stmt = null;
        try {
            stmt = prepareStatement(conn, sql);
            stmt.setBinaryStream(1, inputStream, length);
            stmt.setInt(2, id);
            stmt.executeUpdate();
        } catch (SQLException e) {
            rethrow(e, sql, new Object[]{"...",id}); //$NON-NLS-1$
        } finally {
            close(stmt);
        }
	}

	public void updateBlob(String sql, int id, InputStream inputStream, int length) throws SQLException {
        Connection conn = prepareConnection();
        try {
            updateBlob(conn, sql, id, inputStream, length);
        } finally {
            close(conn);
        }
	}
}
