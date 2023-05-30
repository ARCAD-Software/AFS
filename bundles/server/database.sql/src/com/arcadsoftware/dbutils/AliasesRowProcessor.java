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
package com.arcadsoftware.dbutils;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.Date;
import java.util.Map;

import org.apache.commons.dbutils.BasicRowProcessor;
import org.apache.commons.dbutils.BeanProcessor;

/**
 *
 */
public class AliasesRowProcessor extends BasicRowProcessor {

	static final public AliasesRowProcessor instance = new AliasesRowProcessor();
	
	public AliasesRowProcessor() {
		super(AliasesBeanProcessor.instance);
	}

	public AliasesRowProcessor(BeanProcessor convert) {
		super(convert);
	}

	/* (non-Javadoc)
	 * @see org.apache.commons.dbutils.BasicRowProcessor#toMap(java.sql.ResultSet)
	 */
	@Override
    public Map<String, Object> toMap(ResultSet rs) throws SQLException {
		Map<String, Object> result = new CaseInsensitiveHashMap();
        ResultSetMetaData rsmd = rs.getMetaData();
        int cols = rsmd.getColumnCount();
        for (int i = 1; i <= cols; i++) {
        	Object obj = null;
        	// Convert timestamp to Date (see search bundle)
        	// TODO convert MS SQLServer and Oracle special Date types !
        	if (rsmd.getColumnType(i) == Types.TIMESTAMP) {
        		Timestamp t = rs.getTimestamp(i);
        		if (t == null) {
        			obj = null;
        		} else {
        			obj = new Date(t.getTime());
        		}
        	} else {
        		obj = rs.getObject(i);
        	}
            result.put(AliasesBeanProcessor.aliasColumn(rsmd.getColumnLabel(i)), obj);
        }
        return result;
    }
	
}