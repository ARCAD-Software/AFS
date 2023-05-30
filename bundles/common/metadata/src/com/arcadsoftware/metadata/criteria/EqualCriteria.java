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
 * This class represent and equality test between and Entity's Attribute and a constant value.
 * 
 * <p>
 * This value can be an integer or another value type in its String representation. String representations
 * of other type depends on the Storage and the Mapper technologie.
 * 
 */
public class EqualCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria {

	private String attribute;
	private String value;
	private Integer intval;

	/**
	 * 
	 */
	public EqualCriteria() {
		super();
	}
	
	/**
	 * Equality test with string or integer value.
	 * @param attribute
	 * @param value
	 */
	public EqualCriteria(String attribute, String value) {
		this();
		this.attribute = attribute;
		this.value = value;
	}

	/**
	 * Equality test with integer value. 
	 * @param attribute
	 * @param value
	 */
	public EqualCriteria(String attribute, int value) {
		this();
		this.attribute = attribute;
		this.intval = value;
	}
	
	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		ReferenceLine attributeRef = context.getEntity().getAttributeLine(attribute);
		if (attributeRef != null) {
			context.useReference(attributeRef);
			if ((value == null) && (intval == null)) {
				return new IsNullCriteria(attribute);
			}
			return this;
		}
		return ConstantCriteria.FALSE;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		if (intval != null) {
			return new EqualCriteria(attribute,intval);
		}
		return new EqualCriteria(attribute,value);
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof EqualCriteria) &&
			nullsOrEquals(attribute,((EqualCriteria)obj).attribute) &&
			nullsOrEquals(intval,((EqualCriteria)obj).intval) &&
			nullsOrEquals(value,((EqualCriteria)obj).value);
	}

	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		if (intval != null) {
			return intval.equals(bean.getInt(attribute));
		}
		if ((value == null) || (value.length() == 0)) {
			return bean.get(attribute) == null;
		}
		return value.equals(bean.getString(attribute));
	}

	public String getAttribute() {
		return attribute;
	}

	public String getValue() {
		return value;
	}

	public Integer getIntval() {
		return intval;
	}

	public void setAttribute(String code) {
		attribute = code;
	}
	
	public void setValue(String value) {
		this.value = value;
	}
	
	public void setIntval(Integer value) {
		intval = value;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if (attribute != null) {
			sb.append(attribute);
		}
		if (intval != null) {
			sb.append(Messages.Criteria_Equal);
			sb.append(intval);
		} else if (value != null) {
			sb.append(Messages.Criteria_EqualString);
			sb.append(value);
			sb.append('"');
		} else {
			sb.append(Messages.Criteria_IsNull);
		}
		return sb.toString();
	}
}
