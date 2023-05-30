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

import java.beans.PropertyDescriptor;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Date;

import org.apache.commons.dbutils.BeanProcessor;

/**
 * 
 */
public class AliasesBeanProcessor extends BeanProcessor {

	public final static AliasesBeanProcessor instance = new AliasesBeanProcessor();
	
	/**
	 * Process to aliases...
	 * 
	 * @param columnName
	 * @return
	 */
	public static String aliasColumn(String columnName) {
		if ((columnName == null) || (columnName.length() < 2)) {
			return columnName;
		}
        // Special mapping : j_*** or z_***
		columnName = columnName.toLowerCase(); // = Interdit l'utilisation de majuscule dans les noms d'attributes !
		// [ML] lowerCase, sans doute, à cause des valeurs renvoyées par les Driver JDBC.
		// [ML] 2016: le prefix z peut aussi être utilisé comme un substitue à j.
		if (((columnName.charAt(0) == 'j') || (columnName.charAt(0) == 'z')) && (columnName.charAt(1) == '_')) {
			return columnName.substring(2).replace('_', '.');
		} 
        // Current mapping with 3 char prefix : XXX_***
		if ((columnName.length() > 4) && (columnName.charAt(3) == '_')) {
			return columnName.substring(4).replace('_', '.');
		}
		return columnName.replace('_', '.');
	}
	
	@Override
	protected int[] mapColumnsToProperties(ResultSetMetaData rsmd, PropertyDescriptor[] props) throws SQLException {
        int cols = rsmd.getColumnCount();
        int columnToProperty[] = new int[cols + 1];
        Arrays.fill(columnToProperty, PROPERTY_NOT_FOUND);
        for (int col = 1; col <= cols; col++) {
            String columnName = aliasColumn(rsmd.getColumnLabel(col));
            for (int i = 0; i < props.length; i++) {
                if (columnName.equalsIgnoreCase(props[i].getName())) {
                    columnToProperty[col] = i;
                    break;
                }
            }
        }
        return columnToProperty;
	}

	@Override
	protected Object processColumn(ResultSet rs, int index, @SuppressWarnings("rawtypes") Class propType) throws SQLException {
		if (propType.equals(Date.class)) {
			if (rs.getTimestamp(index) == null) {
				return null;
			}
			return new Date(rs.getTimestamp(index).getTime());
		}
		return super.processColumn(rs, index, propType);
	}
	
}
