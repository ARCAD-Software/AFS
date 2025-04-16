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
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Test id the internal Identifier (ID) of the selected data is equal to the given value.
 */
public class IdGreaterThanCriteria extends AbstractSearchCriteria implements Cloneable {

	private String id;
	
	/**
	 * 
	 */
	public IdGreaterThanCriteria() {
		super();
	}

	public IdGreaterThanCriteria(int id) {
		super();
		this.id = Integer.toString(id);
	}

	public IdGreaterThanCriteria(String id) {
		super();
		this.id = id;
	}

	@Override
	public IdGreaterThanCriteria clone() {
		return new IdGreaterThanCriteria(id);
	}

	@Override
	public boolean equals(Object obj) {
		try  {
			return (obj instanceof IdGreaterThanCriteria) && (((IdGreaterThanCriteria)obj).id.equals(id));
		} catch (NullPointerException e) {
			return false;
		}
	}
	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		try {
			return bean.getId() >= Integer.parseInt(id);
		} catch (Throwable e) {
			return false;
		}
	}

	public String getId() {
		return id;
	}
	
	public void setId(String id) {
		this.id = id;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(Messages.Criteria_Id);
		sb.append(Messages.Criteria_GreaterOrEqual);
		sb.append(id);
		return sb.toString();
	}

}
