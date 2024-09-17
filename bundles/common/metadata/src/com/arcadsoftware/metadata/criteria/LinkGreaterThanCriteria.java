/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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
 * Test an attribute of linked items.
 * 
 * <p>The <code>linkcode</code> is applied to the selected entity from witch a column is tested.  
 */
public class LinkGreaterThanCriteria extends AbstractLinkTestCriteria implements Cloneable {

	/**
	 * 
	 * @param linkCode a non null link code of the current entity.
	 * @param attribute an attribute reference line starting from the link target entity. 
	 * @param value the constant value to test.
	 */
	public LinkGreaterThanCriteria(String linkCode, String attribute, String value) {
		super(linkCode, attribute, value);
	}

	/**
	 * 
	 * @param reference a attribute reference line from which the link code is applicable, may be null.
	 * @param linkCode a non null link code.
	 * @param attribute an attribute reference line starting from the link target entity. 
	 * @param value the constant value to test.
	 */
	public LinkGreaterThanCriteria(String reference, String linkCode, String attribute, String value) {
		super(reference, linkCode, attribute, value);
	}

	public LinkGreaterThanCriteria() {
		super();
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof LinkGreaterThanCriteria) && super.equals(obj);
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new LinkGreaterThanCriteria(getReference(), getLinkCode(), getAttribute(), getValue());
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	protected boolean test(Object attributeValue, Object value) {
		if (attributeValue == null) {
			return (value == null) || (value.toString().length() == 0);
		}
		if (attributeValue instanceof Integer) {
			try {
				return Integer.decode(value.toString()) <= (Integer)attributeValue;
			} catch (NumberFormatException e) {
				return false;
			}
		}
		if (attributeValue instanceof Long) {
			try {
				return Long.decode(value.toString()) <= (Long)attributeValue;
			} catch (NumberFormatException e) {
				return false;
			}
		}
		if (attributeValue instanceof Short) {
			try {
				return Short.decode(value.toString()) <= (Short)attributeValue;
			} catch (NumberFormatException e) {
				return false;
			}
		}
		if (attributeValue instanceof Double) {
			try {
				return Double.parseDouble(value.toString()) <= (Double)attributeValue;
			} catch (NumberFormatException e) {
				return false;
			}
		}
		if (attributeValue instanceof Calendar) {
			attributeValue = ((Calendar)attributeValue).getTime();
		}
		if (attributeValue instanceof Date) {
			try {
				return ISODateFormater.toDate(value.toString()).getTime() <= ((Date)attributeValue).getTime();
			} catch (ParseException e) {
				return false;
			}
		}
		if (attributeValue instanceof Comparable) {
			return ((Comparable)attributeValue).compareTo(value) >= 0;
		}
		return attributeValue.toString().compareTo(value.toString()) >= 0;
	}

	@Override
	protected String getTestString() {
		return Messages.Criteria_Equal;
	}
}
