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
package com.arcadsoftware.metadata.criteria;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * This a fake contextual criteria here the SQL clause is hard-coded.
 * 
 * <p>
 * <b>Theses kind of request should not be mapped to XML formats or even used with BeanMap.</b>
 */
public class PreGeneratedCriteria extends AbstractSearchCriteria implements Cloneable {

	private String sql;
	
	/**
	 * @param sql
	 */
	public PreGeneratedCriteria(String sql) {
		super();
		this.sql = sql;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new PreGeneratedCriteria(sql);
	}

	@Override
	public boolean equals(Object obj) {
		try {
			return (obj instanceof PreGeneratedCriteria) && (((PreGeneratedCriteria)obj).sql.equals(sql));
		} catch (NullPointerException e) {
			return false;
		}
	}

	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		return false;
	}

	public String getSql() {
		return sql;
	}

	@Override
	public String toString() {
		return sql;
	}
	
}
