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

import javax.sql.DataSource;

/**
 * OSGi service that allow to create DataSource for all supported Databases.
 * 
 * @author ARCAD Software
 */
public interface IDataSourceProvider {

	public static final String clazz = IDataSourceProvider.class.getName();
	
	/**
	 * @return true if the given Database Type is acceptable for this Provider.
	 * @see com.arcadsoftware.database.IDataSourceInformations
	 */
	public boolean acceptDatabaseType(String databaseType);
	
	/**
	 * Create a DataSource.
	 * 
	 * @param parameters The required parameters to create a Data Source.
	 * @return a non null DataSource implementation.
	 * @throws SQLException if some parameters are incorrects.
	 */
	public DataSource createDataSource(DataSourceParameters parameters) throws SQLException;
}
