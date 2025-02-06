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

import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.osgi.ISODateFormater;

/**
 * Test if the attribute value of linked items (through linkCode), is greater than the given number (value).
 * 
 * <p>
 * The <code>linkcode</code> is applied to the selected entity from witch a column is tested. 
 * If <code>reference</code> is use the this entity is the type of the last attribute of the reference line.
 * 
 * @author ARCAD Software
 */
public class LinkGreaterStrictCriteria extends AbstractLinkTestCriteria implements Cloneable {

	/**
	 * 
	 * @param linkCode a non null link code of the current entity.
	 * @param attribute an attribute reference line starting from the link target entity. 
	 * @param value the constant value to test.
	 */
	public LinkGreaterStrictCriteria(String linkCode, String attribute, String value) {
		super(linkCode, attribute, value);
	}

	/**
	 * 
	 * @param reference a attribute reference line from which the link code is applicable, may be null.
	 * @param linkCode a non null link code.
	 * @param attribute an attribute reference line starting from the link target entity. 
	 * @param value the constant value to test.
	 */
	public LinkGreaterStrictCriteria(String reference, String linkCode, String attribute, String value) {
		super(reference, linkCode, attribute, value);
	}

	/**
	 * 
	 * @param reference a attribute reference line from which the link code is applicable, may be null.
	 * @param linkCode a non null link code.
	 * @param attribute an attribute reference line starting from the link target entity. 
	 * @param value the constant value to test.
	 * @param ignoreSubdivision if true the subdivision included in the link chain will be ignored.
	 * @param deleted if true the soft-deleted links and inner entity items will be taken into account.
	 */
	public LinkGreaterStrictCriteria(String reference, String linkCode, String attribute, String value, boolean ignoreSubdivision, boolean deleted) {
		super(reference, linkCode, attribute, value, ignoreSubdivision, deleted);
	}

	public LinkGreaterStrictCriteria() {
		super();
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof LinkGreaterStrictCriteria) && super.equals(obj);
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new LinkGreaterStrictCriteria(getReference(), getLinkCode(), getAttribute(), getValue(), isIgnoreSubdivision(), isDeletedLinks());
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	protected boolean test(Object attributeValue, Object value) {
		if (attributeValue == null) {
			return (value == null) || (value.toString().length() == 0);
		}
		if (attributeValue instanceof Integer) {
			try {
				return Integer.decode(value.toString()) < (Integer) attributeValue;
			} catch (NumberFormatException e) {
				return false;
			}
		}
		if (attributeValue instanceof Long) {
			try {
				return Long.decode(value.toString()) < (Long) attributeValue;
			} catch (NumberFormatException e) {
				return false;
			}
		}
		if (attributeValue instanceof Short) {
			try {
				return Short.decode(value.toString()) < (Short) attributeValue;
			} catch (NumberFormatException e) {
				return false;
			}
		}
		if (attributeValue instanceof Double) {
			try {
				return Double.parseDouble(value.toString()) < (Double) attributeValue;
			} catch (NumberFormatException e) {
				return false;
			}
		}
		if (attributeValue instanceof Calendar) {
			attributeValue = ((Calendar) attributeValue).getTime();
		}
		if (attributeValue instanceof Date) {
			try {
				return ISODateFormater.toDate(value.toString()).getTime() < ((Date)attributeValue).getTime();
			} catch (ParseException e) {
				return false;
			}
		}
		if (attributeValue instanceof Comparable) {
			return ((Comparable) attributeValue).compareTo(value) > 0;
		}
		return attributeValue.toString().compareTo(value.toString()) > 0;
	}

	@Override
	protected String getTestString() {
		return Messages.Criteria_Greater;
	}
}
