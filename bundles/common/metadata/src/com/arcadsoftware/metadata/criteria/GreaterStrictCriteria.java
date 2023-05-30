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

import java.text.ParseException;
import java.util.Calendar;
import java.util.Date;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.osgi.ISODateFormater;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Test if the attribute value is greater than the given value.
 */
public class GreaterStrictCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria {

	private String attribute;
	private String value;

	/**
	 * 
	 */
	public GreaterStrictCriteria() {
		super();
	}

	/**
	 * @param attribute
	 * @param string
	 */
	public GreaterStrictCriteria(String attribute, String value) {
		super();
		this.attribute = attribute;
		this.value = value;
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		ReferenceLine attributeRef = context.getEntity().getAttributeLine(attribute);
		if (attributeRef != null) {
			context.useReference(attributeRef);
			if (value == null) {
				return new NotCriteria(new IsNullCriteria(attribute));
			}
			return this;
		}
		return ConstantCriteria.FALSE;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new GreaterStrictCriteria(attribute,value);
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof GreaterStrictCriteria) &&
		attribute .equals(((GreaterStrictCriteria)obj).attribute) &&
		value .equals(((GreaterStrictCriteria)obj).value);
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(attribute);
		if (value  == null) {
			sb.append(Messages.Criteria_IsNotNull);
		} else {
			sb.append(Messages.Criteria_Greater);
			sb.append('"');
			sb.append(value);
			sb.append('"');
		}
		return sb.toString();
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		Object o = bean.get(attribute);
		if (o == null) {
			return (value == null) || (value.length() == 0);
		}
		if (o instanceof Integer) {
			try {
				return Integer.decode(value) < (Integer)o;
			} catch (NumberFormatException e) {
				return false;
			}
		}
		if (o instanceof Long) {
			try {
				return Long.decode(value) < (Long)o;
			} catch (NumberFormatException e) {
				return false;
			}
		}
		if (o instanceof Short) {
			try {
				return Short.decode(value) < (Short)o;
			} catch (NumberFormatException e) {
				return false;
			}
		}
		if (o instanceof Double) {
			try {
				return Double.parseDouble(value) < (Double)o;
			} catch (NumberFormatException e) {
				return false;
			}
		}
		if (o instanceof Calendar) {
			o = ((Calendar)o).getTime();
		}
		if (o instanceof Date) {
			try {
				return ISODateFormater.toDate(value).getTime() < ((Date)o).getTime();
			} catch (ParseException e) {
				return false;
			}
		}
		if (o instanceof Comparable) {
			return ((Comparable)o).compareTo(value) > 0;
		}
		return o.toString().compareTo(value) > 0;
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
	
}
