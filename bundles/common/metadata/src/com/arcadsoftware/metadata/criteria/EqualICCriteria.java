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
package com.arcadsoftware.metadata.criteria;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Test if the String value of the attribute is equals to the given text, ignoring case.
 */
public class EqualICCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria {

	private String attribute;
	private String value;

	/**
	 */
	public EqualICCriteria() {
		super();
	}
	
	/**
	 * @param attribute
	 * @param value
	 */
	public EqualICCriteria(String attribute, String value) {
		this();
		this.attribute = attribute;
		this.value = value;
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		ReferenceLine attributeRef = context.getEntity().getAttributeLine(attribute);
		if (attributeRef != null) {
			context.useReference(attributeRef);
			if (value == null) {
				return new IsNullCriteria(attribute);
			}
			return this;
		}
		return ConstantCriteria.FALSE;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new EqualICCriteria(attribute,value);
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof EqualICCriteria) &&
		nullsOrEquals(attribute,((EqualICCriteria)obj).attribute) &&
		nullsOrEquals(value,((EqualICCriteria)obj).value);
	}

	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		if (value == null) {
			return bean.get(attribute) == null;
		}
		return value.equalsIgnoreCase(bean.getString(attribute));
	}

	public String getAttribute() {
		return attribute;
	}

	public String getValue() {
		return value;
	}

	public void setAttribute(String code) {
		attribute = code;
	}

	public void setValue(String value) {
		this.value = value;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(attribute);
		if (value  == null) {
			sb.append(Messages.Criteria_IsNull);
		} else {
			sb.append(Messages.Criteria_Equal);
			sb.append(Messages.Criteria_CaseSensitive);
			sb.append('"');
			sb.append(value);
			sb.append('"');
		}
		return sb.toString();
	}

}
