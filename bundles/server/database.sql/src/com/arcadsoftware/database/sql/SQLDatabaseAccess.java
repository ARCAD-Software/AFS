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
package com.arcadsoftware.database.sql;

import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

import org.apache.commons.dbutils.QueryRunner;
import org.apache.commons.dbutils.handlers.BeanHandler;
import org.apache.commons.dbutils.handlers.BeanListHandler;
import org.apache.commons.dbutils.handlers.ColumnListHandler;
import org.apache.commons.dbutils.handlers.MapListHandler;
import org.apache.commons.dbutils.handlers.ScalarHandler;

import com.arcadsoftware.database.DatabaseTracker;
import com.arcadsoftware.database.internal.Messages;
import com.arcadsoftware.dbutils.AliasesMapHandler;
import com.arcadsoftware.dbutils.AliasesRowProcessor;
import com.arcadsoftware.dbutils.PartialBeanListHandler;
import com.arcadsoftware.dbutils.QueryRunnerEx;
import com.arcadsoftware.osgi.AbstractActivator;

/**
 * This class provide a Database Access (i.e. track for a DataSource) and 
 * an API based on Jakarta Common DBUtils to access to the data.
 */
public class SQLDatabaseAccess {

	private AbstractActivator activator;
	private DatabaseTracker dbTracker;
	
	public SQLDatabaseAccess(AbstractActivator activator, String databaseID) {
		this(activator, databaseID, false);
	}
	
	public SQLDatabaseAccess(AbstractActivator activator, String databaseID, boolean keepit) {
		super();
		if (activator != null) {
			this.activator = activator;
			dbTracker = new DatabaseTracker(activator.getContext(), databaseID, keepit);
			dbTracker.open();
		}
	}

	protected AbstractActivator getActivator() {
		return activator;
	}
	
	protected void printError(String format,Object param,Throwable e) {
		if (activator == null) {
			if (param != null) {
				System.err.println(String.format(format, param));
			} else if (format != null) {
				System.err.println(format);
			}
			if (e != null) {
				System.err.println(e.getLocalizedMessage());
			}
		} else {
			activator.error(String.format(format,param),e);
		}
	}
	
	protected Object[] validateParams(Object[] params) {
		if (params == null) {
			return null;
		}
		for(int i = 0; i < params.length; i++) {
			if (params[i] instanceof Date) {
				params[i] = new Timestamp(((Date)params[i]).getTime());
			}
		}
		return params;
	}
	
	protected Object[] validateParam(Object param) {
		if (param instanceof Date) {
			return new Object[]{new Timestamp(((Date)param).getTime())};
		}
		return new Object[]{param}; 
	}
	
	/**
	 * Terminate the tracking of the datasource.
	 * Must be called when the Activator is stoped.
	 */
	public void close() {
		if (dbTracker != null) {
			dbTracker.close();
		}
	}

	/**
	 * Return a data source associated to this database access.
	 * 
	 * @return null if the DataSource is not ready.
	 */
	public DataSource getDataSource() {
		return getDataSource(true);
	}
	
	/**
	 * Return a data source associated to this database access.
	 * 
	 * @param verbose log an error if the Datasource is null.
	 * @return null if the DataSource is not ready.
	 */
	public DataSource getDataSource(boolean verbose) {
		if (dbTracker == null) {
			printError(Messages.SQLDatabaseAccess_DSClosed, null, null);
			return null; 
		}
		DataSource result = dbTracker.getDataSource();
		if (result == null) {
			printError(Messages.SQLDatabaseAccess_NoDSReady, null, null);
		}
		return result;
	}
	
	
	/**
	 * Run a query that return a list of objects.
	 * 
	 * @param query
	 * @param clazz
	 * @return
	 */
	public <T> List<T> queryList(String query,Class<T> clazz) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return null;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return run.query(query,new BeanListHandler<T>(clazz,AliasesRowProcessor.instance));
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_ListQuery,query,e);
			return null;
		} 
	}

	/**
	 * Run a query that return only a partial list of results.
	 * @param query
	 * @param clazz
	 * @param offset
	 * @param limit
	 * @return
	 */
	public <T> List<T> queryListPart(String query,Class<T> clazz, int offset, int limit) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return null;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return (List<T>) run.query(query,new PartialBeanListHandler<T>(clazz, AliasesRowProcessor.instance, offset, limit));
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_ListQuery,query,e);
			return null;
		} 
	}

	/**
	 * Run a query that return a single object. 
	 * 
	 * @param query
	 * @param clazz
	 * @return
	 */
	public <T> T query(String query,Class<T> clazz) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return null;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return run.query(query,new BeanHandler<T>(clazz,AliasesRowProcessor.instance));
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_SimgleQuery,query,e);
			return null;
		} 
	}

	/**
	 * Run a query that return a single element.
	 * 
	 * @param query
	 * @return a Map with aliases columns as keys.  
	 */
	@SuppressWarnings("rawtypes")
	public Map query(String query) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return null;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return (Map) run.query(query,AliasesMapHandler.instance);
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_MapQuery,query,e);
			return null;
		} 
	}
	/**
	 * Run a query that return a list of elements.
	 * 
	 * @param query
	 * @return a Map with aliases columns as keys.  
	 */
	@SuppressWarnings("rawtypes")
	public List queryList(String query) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return null;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return (List) run.query(query,new MapListHandler(AliasesRowProcessor.instance));
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_MapListQuery,query,e);
			return null;
		} 
	}
	
	/**
	 * Run a query that return a list of objects.
	 */
	public <T> List<T> queryList(String query,Class<T> clazz, Object param) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return null;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return (List<T>) run.query(query, new BeanListHandler<T>(clazz, AliasesRowProcessor.instance), validateParam(param));
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_ListQuery,query,e);
			return null;
		} 
	}

	/**
	 * Run a query add return only a partial list of results.
	 * 
	 * @param query
	 * @param clazz
	 * @param param
	 * @param offset
	 * @param limit
	 * @return
	 */
	public <T> List<T> queryListPart(String query,Class<T> clazz, int offset, int limit, Object param) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return null;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return run.query(query, new PartialBeanListHandler<T>(clazz, AliasesRowProcessor.instance, offset, limit), validateParam(param));
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_ListQuery,query,e);
			return null;
		} 
	}

	/**
	 * Run a query that return a single object. 
	 * 
	 * @param query
	 * @param clazz
	 * @return
	 */
	public <T> T query(String query,Class<T> clazz,Object param) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return null;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return run.query(query, new BeanHandler<T>(clazz, AliasesRowProcessor.instance), validateParam(param));
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_SimgleQuery,query,e);
			return null;
		} 
	}

	/**
	 * Run a query that return a single element.
	 * 
	 * @param queryID
	 * @return a Map with aliases columns as keys.  
	 */
	public Map<String,Object> query(String query, Object param) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return null;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return run.query(query, AliasesMapHandler.instance, validateParam(param));
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_MapQuery,query,e);
			return null;
		} 
	}
	/**
	 * Run a query that return a list of elements.
	 * 
	 * @param query
	 * @return a List of Maps with aliases columns as keys.  
	 */
	public List<Map<String, Object>> queryList(String query, Object param) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return null;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return run.query(query, new MapListHandler(AliasesRowProcessor.instance), validateParam(param));
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_MapListQuery,query,e);
			return null;
		} 
	}

	/**
	 * Run a query that return a list of objects.
	 */
	public <T> List<T> queryList(String query, Class<T> clazz, Object... params) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return null;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return run.query(query, new BeanListHandler<T>(clazz,AliasesRowProcessor.instance), validateParams(params));
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_ListQuery,query,e);
			return null;
		} 
	}

	public <T> List<T> queryListValue(String query, Class<T> clazz, Object... params) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return null;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return run.query(query, new ColumnListHandler<T>(), validateParams(params));
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_ListQuery,query,e);
			return null;
		} 
	}

	public <T> List<T> queryListPart(String query,Class<T> clazz, int offset, int limit, Object... params) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return null;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return run.query(query, new PartialBeanListHandler<T>(clazz, AliasesRowProcessor.instance, offset, limit), validateParams(params));
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_ListQuery,query,e);
			return null;
		} 
	}

	/**
	 * Run a query that return a single object. 
	 * 
	 * @param queryID
	 * @param clazz
	 * @return
	 */
	public <T> T query(String query,Class<T> clazz, Object... params) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return null;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return (T) run.query(query, new BeanHandler<T>(clazz,AliasesRowProcessor.instance), validateParams(params));
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_SimgleQuery,query,e);
			return null;
		} 
	}

	/**
	 * Run a query that return a single element.
	 * 
	 * @param query
	 * @return a Map with aliases columns as keys.  
	 */
	public Map<String, Object> query(String query, Object[] params) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return null;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return run.query(query, AliasesMapHandler.instance, validateParams(params));
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_MapQuery,query,e);
			return null;
		} 
	}
	
	/**
	 * Run a query that return a list of elements.
	 * 
	 * @param query
	 * @return a Map with aliases columns as keys.  
	 */
	public List<?> queryList(String query, Object... params) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return null;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return run.query(query, new MapListHandler(AliasesRowProcessor.instance), validateParams(params));
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_MapListQuery,query,e);
			return null;
		} 
	}
	
	/**
	 * Run a update.
	 * 
	 * @param query
	 * @return the number of updated rows  
	 */
	public int update(String query) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return 0;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return run.update(query);
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_Update, query, e);
			return -1;
		} 
	}
	
	/**
	 * Run a update.
	 * 
	 * @param query the sql update, insert or delete code.
	 * @param param the single parameter of this request
	 * @return the number of updated rows  
	 */
	public int update(String query, Object param) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return 0;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return run.update(query, validateParam(param));
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_Update,query,e);
			return -1;
		} 
	}
	
	/**
	 * Run a update.
	 * 
	 * @param query the sql update, insert or delete code.
	 * @param params the parameters of this request
	 * @return the number of updated rows  
	 */
	public int update(String query, Object... params) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return 0;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			return run.update(query, validateParams(params));
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_Update,query,e);
			return -1;
		} 
	}
	
	/**
	 * Run a count select on database.
	 * 
	 * @param sql the SQL "select count(*) ..." request.
	 * @return may return -1 if the result is <b>not</b> null and is not an integer.
	 */
	public int queryCount(String query) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return 0;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			Object o = run.query(query,new ScalarHandler<Object>());
			if (o == null) {
				return 0;
			}
			if (o instanceof Integer) {
				return (Integer)o;
			} 
			if (o instanceof Long) {
				return Math.round((Long)o);
			} 
			try {
				return Integer.valueOf(o.toString());
			} catch (NumberFormatException e) {
				return -1;
			}
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_SimgleQuery,query,e);
			return 0;
		} 
	}

	/**
	 * Run a count select on database.
	 * 
	 * @param query
	 * @param params
	 * @return
	 */
	public int queryCount(String query,Object... params) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return 0;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			Object o = run.query(query, new ScalarHandler<Object>(), validateParams(params));
			if (o == null) {
				return 0;
			}
			if (o instanceof Integer) {
				return (Integer)o;
			} 
			if (o instanceof Long) {
				return Math.round((Long)o);
			} 
			try {
				return Integer.valueOf(o.toString());
			} catch (NumberFormatException e) {
				return -1;
			}
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_MapQuery,query,e);
			return 0;
		} 
	}

	/**
	 * Run a count select on database.
	 * 
	 * Process a query and return the first column of the first line of the result as an int value.
	 * 
	 * @param query
	 * @param param
	 * @return
	 */
	public int queryCount(String query,Object param) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return 0;
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			Object o = run.query(query, new ScalarHandler<Object>(), validateParam(param));
			if (o == null) {
				return 0;
			}
			if (o instanceof Integer) {
				return (Integer)o;
			} 
			if (o instanceof Long) {
				return Math.round((Long)o);
			} 
			try {
				return Integer.valueOf(o.toString());
			} catch (NumberFormatException e) {
				return -1;
			}
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_MapQuery,query,e);
			return 0;
		} 
	}
	
	/**
	 * Get a single string from a request, this string is the first line result, first column.
	 * 
	 * @param query
	 * @param param
	 * @return
	 */
	public String queryString(String query, Object... params) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return ""; //$NON-NLS-1$
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			Object o = run.query(query, new ScalarHandler<Object>(), validateParam(params));
			if (o == null) {
				return ""; //$NON-NLS-1$
			}
			if (o instanceof String) {
				return (String)o;
			} 
			return o.toString();
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_MapQuery,query,e);
			return ""; //$NON-NLS-1$
		} 
	}
	
	/**
	 * Get a single string from a request, this string is the first line result, first column.
	 * 
	 * @param queryID
	 * @return
	 */
	public String queryString(String query) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return ""; //$NON-NLS-1$
		}
		QueryRunner run = new QueryRunner(ds);
		try {
			Object o = run.query(query, new ScalarHandler<Object>());
			if (o == null) {
				return ""; //$NON-NLS-1$
			}
			if (o instanceof String) {
				return (String)o;
			} 
			return o.toString();
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_MapQuery,query,e);
			return ""; //$NON-NLS-1$
		} 
	}

	/**
	 * Perform complex insert into database.
	 * 
	 * @param queryID
	 * @param params
	 * @return
	 */
	@Deprecated
	public int insert(String query,Object[] params) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return 0;
		}
		QueryRunnerEx run = new QueryRunnerEx(ds);
		try {
			return run.insert(query,validateParams(params),"j_id"); //$NON-NLS-1$
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_ComplexInsert,query,e);
			return 0;
		} 
	}

	/**
	 * Perform an insert into the database.
	 * 
	 * <p>
	 * If the database can use "auto incremented" primary key this
	 * method will use the <code>idCol</code> to retreive the inserted id row number (Primary key).
	 * This is compatible with MySQL and Oracle SGDB. If the
	 * database do not support this feature then the insert request must
	 * be in fact something like :
	 * 
	 * <p><code>
	 * select max(idcol) + 1 from XXX;
	 * insert into XXX (idcol,....) values ((select max(idcol)+1 from XXX),....);
	 * </code>
	 * 
	 * <p>Attention this sample is incorrect, if the table is empty the result is null.
	 * 
	 * @param query
	 * @param params
	 * @param idCol
	 * @return
	 */
	public int insert(String query,Object[] params, String idCol) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return 0;
		}
		QueryRunnerEx run = new QueryRunnerEx(ds);
		try {
			return run.insert(query,validateParams(params), idCol);
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_ComplexInsert,query,e);
			return 0;
		} 
	}

	/**
	 * Perform complex insert into database.
	 * 
	 * @param query
	 * @param params
	 * @return
	 */
	@Deprecated
	public int insert(String query) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return 0;
		}
		QueryRunnerEx run = new QueryRunnerEx(ds);
		try {
			return run.insert(query,"j_id"); //$NON-NLS-1$
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_ComplexInsert,query,e);
			return 0;
		} 
	}
	
	/**
	 * <p>
	 * If the database can use "auto incremented" primary key this
	 * method will use the <code>idCol</code> to retreive de inserted id row number (Primary key).
	 * This is compatible with MySQL and Oracle SGDB. If the
	 * database do not support this feature then the insert request must
	 * be in fact something like :
	 * 
	 * <p><code>
	 * select max(idcol) + 1 from XXX;
	 * insert into XXX (idcol,....) values ((select max(idcol)+1 from XXX),....);
	 * </code>
	 * 
	 * <p>Attention this sample is incorrect, if the table is empty the result is null.
	 *  
	 * @param query
	 * @param idCol
	 * @return
	 */
	public int insert(String query, String idCol) {
		DataSource ds = getDataSource();
		if ((ds == null) || (query == null)) {
			return 0;
		}
		QueryRunnerEx run = new QueryRunnerEx(ds);
		try {
			return run.insert(query,idCol);
		} catch (SQLException e) {
			printError(Messages.SQLDatabaseAccess_ComplexInsert,query,e);
			return 0;
		} 
	}

}
