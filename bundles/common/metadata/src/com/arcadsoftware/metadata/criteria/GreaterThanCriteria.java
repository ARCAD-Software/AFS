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

import java.text.ParseException;
import java.util.Calendar;
import java.util.Date;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.osgi.ISODateFormater;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Test if the value of an attribute id greater than the given value.
 */
public class GreaterThanCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria {

	private String attribute;
	private String value;

	/**
	 * 
	 */
	public GreaterThanCriteria() {
		super();
	}

	/**
	 * @param attribute
	 * @param string
	 */
	public GreaterThanCriteria(String attribute, String value) {
		super();
		this.attribute = attribute;
		this.value = value;
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		ReferenceLine refline = context.getEntity().getReferenceLine(attribute);
		if (refline == null) {
			return ConstantCriteria.FALSE;
		}			
		if (refline.isMultiLinkList()) {
			Activator.getInstance().warn("\"Greater Than\" Criteria with multi-link references is not supported: " + toString());
			return ConstantCriteria.FALSE;
		}
		if (refline.isLinkList()) {
			String preLinkCode = refline.getPreLinkCodes();
			if (preLinkCode.isEmpty()) {
				preLinkCode = null;
			}
			String postLinkCode = refline.getPostLinkCodes();
			if (postLinkCode.isEmpty()) {
				Activator.getInstance().warn("Invalid \"Greater Than\" Criteria with terminal link reference: " + toString());
			}
			return new LinkGreaterThanCriteria(preLinkCode, refline.getFirstLinkCode(), postLinkCode, value).reduce(context);
		}
		context.useReference(refline);
		if (value == null) {
			return new NotCriteria(new IsNullCriteria(attribute));
		}
		return this;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new GreaterThanCriteria(attribute,value);
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof GreaterThanCriteria) &&
		attribute .equals(((GreaterThanCriteria)obj).attribute) &&
		value .equals(((GreaterThanCriteria)obj).value);
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })

	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		Object o = bean.get(attribute);
		if (o == null) {
			return (value == null) || (value.length() == 0);
		}
		if (o instanceof Integer) {
			try {
				return Integer.decode(value) <= (Integer)o;
			} catch (NumberFormatException e) {
				return false;
			}
		}
		if (o instanceof Long) {
			try {
				return Long.decode(value) <= (Long)o;
			} catch (NumberFormatException e) {
				return false;
			}
		}
		if (o instanceof Short) {
			try {
				return Short.decode(value) <= (Short)o;
			} catch (NumberFormatException e) {
				return false;
			}
		}
		if (o instanceof Double) {
			try {
				return Double.parseDouble(value) <= (Double)o;
			} catch (NumberFormatException e) {
				return false;
			}
		}
		if (o instanceof Calendar) {
			o = ((Calendar)o).getTime();
		}
		if (o instanceof Date) {
			try {
				return ISODateFormater.toDate(value).getTime() <= ((Date)o).getTime();
			} catch (ParseException e) {
				return false;
			}
		}
		if (o instanceof Comparable) {
			return ((Comparable)o).compareTo(value) >= 0;
		}
		return o.toString().compareTo(value) >= 0;
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
			sb.append(Messages.Criteria_IsNotNull);
		} else {
			sb.append(Messages.Criteria_GreaterOrEqual);
			sb.append('"');
			sb.append(value);
			sb.append('"');
		}
		return sb.toString();
	}

}
