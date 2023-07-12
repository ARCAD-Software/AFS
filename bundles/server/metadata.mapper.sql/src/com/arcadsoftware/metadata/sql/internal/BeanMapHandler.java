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
import java.sql.Timestamp;
import java.sql.Types;
import java.util.Date;

import org.apache.commons.dbutils.ResultSetHandler;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.dbutils.AliasesBeanProcessor;

/**
 * Direct (fast) Handler from Resultset to BeanMap...
 */
public class BeanMapHandler implements ResultSetHandler<Object> {

	private BeanMap bean;
	private String prefix;
	private String startwith;
	private int startwithlen;
	private final boolean returnBeanMap;

	public BeanMapHandler() {
		super();
		returnBeanMap = true;
	}
	
	/**
	 * @param type
	 */
	public BeanMapHandler(String type) {
		super();
		bean = new BeanMap(type);
		returnBeanMap = true;
	}
	
	/**
	 * @param type
	 */
	public BeanMapHandler(String type, boolean returnBeanMap) {
		super();
		bean = new BeanMap(type);
		this.returnBeanMap = returnBeanMap;
	}

	/**
	 * @param bean
	 */
	public BeanMapHandler(BeanMap bean) {
		super();
		this.bean = bean;
		returnBeanMap = bean != null;
	}

	/* (non-Javadoc)
	 * @see org.apache.commons.dbutils.ResultSetHandler#handle(java.sql.ResultSet)
	 */
	public Object handle(ResultSet rs) throws SQLException {
		if (rs.next()) {
			ResultSetMetaData rsmd = rs.getMetaData();
			return processBeanMap(bean, rs, rsmd.getColumnCount(), rsmd);
		}
		if (returnBeanMap) {
			return bean;
		}
		return null;
	}

	protected BeanMap processBeanMap(BeanMap bean, ResultSet rs, int cols, ResultSetMetaData rsmd) throws SQLException {
		boolean notid = true;
		for (int i = 1; i <= cols; i++) {
			final String label = rsmd.getColumnLabel(i);
			String name = AliasesBeanProcessor.aliasColumn(label);
			if ((name == null) ||//
					("z".equalsIgnoreCase(name))) { //$NON-NLS-1$
				// z est utilisé comme colonne de travail dans les requetes complexes. //$NON-NLS-1$
				continue;
			}
			if (prefix != null) {
				if (prefix.equals(name)) {
					bean.forceId(rs.getInt(i));
					notid = false;
					continue;
				} else if (name.startsWith(startwith)) {
					name = name.substring(startwithlen);
				}
			}
			if ("id".equals(name) && notid) { //$NON-NLS-1$
				bean.forceId(rs.getInt(i));
			} else if ("deleted".equals(name)) { //$NON-NLS-1$
				bean.setDeleted(rs.getInt(i) != 0);
			} else if ("date".equals(name)) { //$NON-NLS-1$
				Timestamp t = rs.getTimestamp(i);
				if (t != null) {
					bean.setDate(new Date(t.getTime()));
				}
			} else if (rsmd.getColumnType(i) == Types.TIMESTAMP) {
				// Convert timestamp to Date
				Timestamp t = rs.getTimestamp(i);
				if (t == null) {
					bean.put(name, null);
				} else {
					bean.put(name, new Date(t.getTime()));
				}
			} else if ((label.length() > 2) && (label.charAt(1) == '_') && ((label.charAt(0) == 'z') || (label.charAt(0) == 'Z'))) {
				// @see MapperSQLService#SQL_JAVA_CRYPT_PREFIX
				String crp = rs.getString(i);
				if (crp != null) {
					bean.put(name, new String(Crypto.decrypt(crp)));
				}
			} else {
				bean.put(name, rs.getObject(i));
			}
		}
		return bean;
	}

	/**
	 * The prefix is used to convert columns like "prefix.name" into "name".
	 * 
	 * <p>The prefix is also used as and id substitute.
	 * 
	 * @param prefix
	 */
	public void setPrefix(String prefix) {
		if ((prefix == null) || (prefix.length() == 0)) {
			this.prefix = null;
			startwith = null;
			startwithlen = 0;
		} else {
			this.prefix = prefix.toLowerCase();
			startwith = this.prefix + '.';
			startwithlen = startwith.length();
		}
	}

}
