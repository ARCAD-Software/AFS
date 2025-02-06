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
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 *
 */
public class IsNullCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria {

	private String attribute;

	/**
	 * 
	 */
	public IsNullCriteria() {
		super();
	}

	/**
	 * @param attribute
	 */
	public IsNullCriteria(String attribute) {
		super();
		this.attribute = attribute;
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		ReferenceLine attributeRef = context.getEntity().getAttributeLine(attribute);
		if (attributeRef != null) {
			context.useReference(attributeRef);
			return this;
		}
		return ConstantCriteria.FALSE;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new IsNullCriteria(attribute);
	}

	@Override
	public boolean equals(Object obj) {
		try {
			return (obj instanceof IsNullCriteria) &&
			attribute.equals(((IsNullCriteria)obj).attribute);
		} catch (NullPointerException e) {
			return false;
		}
	}

	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		return bean.get(attribute) == null;
	}

	public String getAttribute() {
		return attribute;
	}

	public void setAttribute(String code) {
		attribute = code;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(attribute);
		sb.append(Messages.Criteria_IsNull);
		return sb.toString();
	}
}
