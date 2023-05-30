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
package com.arcadsoftware.metadata.sql.internal;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.Date;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

/**
 *
 * TODO Implement an Handler that directly transform a ResultSet into an XML or JSON String (use an XStream writer) ! 
 */
public class BeanMapListHandler extends BeanMapHandler {

	protected final BeanMapList list;
	protected final String type;

	/**
	 * Create the BeanMapListHandler from a empty BeanMapList.
	 * 
	 * @param type the type of the BeanMap to load.
	 */
	public BeanMapListHandler(String type) {
		super();
		list = new BeanMapList();
		this.type = type;
	}

	/**
	 * Create the BeanMapListHandler from a empty BeanMapList.
	 * 
	 * @param type the type of the BeanMap to load.
	 * @param date the last changed list date.
	 */
	public BeanMapListHandler(String type, Date date) {
		super();
		list = new BeanMapList();
		list.setDate(date);
		this.type = type;
	}

	/**
	 * Add the loaded BeanMap to an existing list.
	 * Do not check for redundant elements.
	 * 
	 * @param type the type of the BeanMap to load.
	 * @param list the list to update.
	 */
	public BeanMapListHandler(String type, BeanMapList list) {
		super();
		this.list = list;
		this.type = type;
	}

	/**
	 * Create and handler for prefixed list of columns.
	 * 
	 * <p>The prefix is used to convert columns like "prefix.name" into "name".
	 * 
	 * <p>the prefix is also used as and id substitute.
	 * 
	 * @param type
	 * @param prefix
	 */
	public BeanMapListHandler(String type, String prefix) {
		this(type);
		setPrefix(prefix);
	}

	@Override
	public Object handle(ResultSet rs) throws SQLException {
        final ResultSetMetaData rsmd = rs.getMetaData();
        final int cols = rsmd.getColumnCount();
		while (rs.next()) {
			list.add(processBeanMap(new BeanMap(type), rs, cols, rsmd));
		}
		return list;
	}

}
