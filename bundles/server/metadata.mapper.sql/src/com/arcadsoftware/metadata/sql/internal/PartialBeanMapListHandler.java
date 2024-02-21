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
package com.arcadsoftware.metadata.sql.internal;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.Date;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

/**
 *
 */
public class PartialBeanMapListHandler extends BeanMapListHandler {

	private final int offset;
	private final int limit;


	/**
	 * Create the BeanMapListHandler from a empty BeanMapList.
	 * 
	 * @param type the type of the BeanMap to load.
	 */
	public PartialBeanMapListHandler(String type, int offset, int limit) {
		super(type);
		this.offset = offset;
		this.limit = offset + limit;
	}

	/**
	 * Create the BeanMapListHandler from a empty BeanMapList.
	 * 
	 * @param type the type of the BeanMap to load.
	 * @param date the last changed list date.
	 */
	public PartialBeanMapListHandler(String type, Date date, int offset, int limit) {
		super(type, date);
		this.offset = offset;
		this.limit = offset + limit;
	}

	/**
	 * Add the loaded BeanMap to an existing list.
	 * Do not check for redundant elements.
	 * 
	 * @param type the type of the BeanMap to load.
	 * @param list the list to update.
	 */
	public PartialBeanMapListHandler(String type, BeanMapList list, int offset, int limit) {
		super(type, list);
		this.offset = offset;
		this.limit = offset + limit;
	}

	@Override
	public Object handle(ResultSet rs) throws SQLException {
        ResultSetMetaData rsmd = rs.getMetaData();
        int cols = rsmd.getColumnCount();
        int i = offset;
        if (i > 0) {
        	rs.absolute(i);
        }
        if (limit < offset) {
			while (rs.next()) {
				list.add(processBeanMap(new BeanMap(type), rs, cols, rsmd));
			}
        } else {
			while (rs.next()) {
				list.add(processBeanMap(new BeanMap(type), rs, cols, rsmd));
		    	if (++i >= limit) {
		    		return list;
		    	}
			}
		}
		return list;
	}


}
