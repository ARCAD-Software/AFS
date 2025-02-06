/*******************************************************************************
 * Copyright (c) 2025 ARCAD Software.
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

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.dbutils.BasicRowProcessor;
import org.apache.commons.dbutils.ResultSetHandler;
import org.apache.commons.dbutils.RowProcessor;

/**
 * 
 */
public class PartialBeanListHandler<T> implements ResultSetHandler<List<T>> {

    static final RowProcessor ROW_PROCESSOR = new BasicRowProcessor();
	
	private int offset;
	private int limit;
	private Class<T> type = null;
    private RowProcessor convert = ROW_PROCESSOR;
	
	public PartialBeanListHandler(Class<T> type, int offset, int limit) {
		super();
		this.type = type;
		this.offset = offset;
		this.limit = offset + limit;
	}

	public PartialBeanListHandler(Class<T> type, RowProcessor convert, int offset, int limit) {
		super();
		this.type = type;
		this.convert = convert;
		this.offset = offset;
		this.limit = offset + limit;
	}

	/* (non-Javadoc)
	 * @see org.apache.commons.dbutils.ResultSetHandler#handle(java.sql.ResultSet)
	 */
	public List<T> handle(ResultSet rs) throws SQLException {
		List<T> rows = new ArrayList<T>();
        int i = offset;
        if (offset > 0) {
        	rs.absolute(offset);
        }
        while (rs.next()) {
       		rows.add(handleRow(rs));
       		if (++i >= limit) {
       			return rows;
       		}
        }
        return rows;
	}

    protected T handleRow(ResultSet rs) throws SQLException {
        return convert.toBean(rs, type);
    }
}
