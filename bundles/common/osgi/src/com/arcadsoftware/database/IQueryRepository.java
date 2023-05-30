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
package com.arcadsoftware.database;

import java.util.List;

/**
 * Query Repositories are a place to hold SQL queries. This allow to 
 * separate the JDBC program from the SQL Code and to be able to take 
 * into account different SQL dialects. 
 */
public interface IQueryRepository {

	/**
	 * Return the query corresponding to this ID.
	 * 
	 * @param id
	 * @return an SQL query string.
	 */
	public String getQuery(String id);
	
	/**
	 * @param prefix a non-null prefix string
	 * @return The list queries id that starts with the given prefix.
	 */
	public List<String> getQueriesList(String prefix);
}
