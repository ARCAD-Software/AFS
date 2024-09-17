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

import com.arcadsoftware.metadata.internal.Messages;

/**
 * Test an attribute of linked items.
 * 
 * <p>The <code>linkcode</code> is applied to the selected entity from witch a column is tested.  
 */
public class LinkEndCriteria extends AbstractLinkTestCriteria implements Cloneable {

	private boolean casesensitive;

	/**
	 * 
	 * @param linkCode a non null link code of the current entity.
	 * @param attribute an attribute reference line starting from the link target entity. 
	 * @param value the constant value to test.
	 */
	public LinkEndCriteria(String linkCode, String attribute, String value) {
		super(linkCode, attribute, value);
	}

	/**
	 * 
	 * @param reference a attribute reference line from which the link code is applicable, may be null.
	 * @param linkCode a non null link code.
	 * @param attribute an attribute reference line starting from the link target entity. 
	 * @param value the constant value to test.
	 */
	public LinkEndCriteria(String reference, String linkCode, String attribute, String value, boolean casesensitive) {
		super(reference, linkCode, attribute, value);
		this.casesensitive = casesensitive;
	}

	/**
	 * 
	 * @param reference a attribute reference line from which the link code is applicable, may be null.
	 * @param linkCode a non null link code.
	 * @param attribute an attribute reference line starting from the link target entity. 
	 * @param value the constant value to test.
	 */
	public LinkEndCriteria(String reference, String linkCode, String attribute, String value) {
		super(reference, linkCode, attribute, value);
	}

	public LinkEndCriteria() {
		super();
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof LinkEndCriteria) && super.equals(obj) && (((LinkEndCriteria) obj).casesensitive == casesensitive);
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new LinkEndCriteria(getReference(), getLinkCode(), getAttribute(), getValue(), casesensitive);
	}

	@Override
	protected boolean test(Object attributeValue, Object value) {
		if (attributeValue == null) {
			return value.toString().length() == 0;
		}
		if (casesensitive) {
			return attributeValue.toString().endsWith(value.toString());
		}
		return attributeValue.toString().toLowerCase().endsWith(value.toString().toLowerCase());
	}

	@Override
	protected String getTestString() {
		if (casesensitive) {
			return Messages.Criteria_EndWith + Messages.Criteria_CaseSensitive;
		}
		return Messages.Criteria_EndWith;
	}

	public boolean isCasesensitive() {
		return casesensitive;
	}

	public void setCasesensitive(boolean casesensitive) {
		this.casesensitive = casesensitive;
	}
}
