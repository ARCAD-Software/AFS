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

import com.arcadsoftware.metadata.internal.Messages;

/**
 * Test if the attribute value of linked items (through linkCode), starts with the given text fragment (value).
 * 
 * <p>
 * The <code>linkcode</code> is applied to the selected entity from witch a column is tested. 
 * If <code>reference</code> is use the this entity is the type of the last attribute of the reference line.
 * 
 * @author ARCAD Software
 */
public class LinkStartCriteria extends AbstractLinkTestCriteria implements Cloneable {

	private boolean casesensitive;

	/**
	 * 
	 * @param linkCode a non null link code of the current entity.
	 * @param attribute an attribute reference line starting from the link target entity. 
	 * @param value the constant value to test.
	 */
	public LinkStartCriteria(String linkCode, String attribute, String value) {
		super(linkCode, attribute, value);
	}

	/**
	 * 
	 * @param reference a attribute reference line from which the link code is applicable, may be null.
	 * @param linkCode a non null link code.
	 * @param attribute an attribute reference line starting from the link target entity. 
	 * @param value the constant value to test.
	 */
	public LinkStartCriteria(String reference, String linkCode, String attribute, String value, boolean casesensitive) {
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
	public LinkStartCriteria(String reference, String linkCode, String attribute, String value) {
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
	 * @param casesensitive true if the test is case sensitive
	 */
	public LinkStartCriteria(String reference, String linkCode, String attribute, String value, boolean ignoreSubdivision, boolean deleted, boolean casesensitive) {
		super(reference, linkCode, attribute, value, ignoreSubdivision, deleted);
		this.casesensitive = casesensitive;
	}

	public LinkStartCriteria() {
		super();
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof LinkStartCriteria) && super.equals(obj) && (((LinkStartCriteria) obj).casesensitive == casesensitive);
	}

	@Override
	public LinkStartCriteria clone() {
		return new LinkStartCriteria(getReference(), getLinkCode(), getAttribute(), getValue(), isIgnoreSubdivision(), isDeletedLinks(), casesensitive);
	}

	@Override
	protected boolean test(Object attributeValue, Object value) {
		if (attributeValue == null) {
			return value.toString().length() == 0;
		}
		if (casesensitive) {
			return attributeValue.toString().startsWith(value.toString());
		}
		return attributeValue.toString().toLowerCase().startsWith(value.toString().toLowerCase());
	}

	@Override
	protected String getTestString() {
		if (casesensitive) {
			return Messages.Criteria_StartWith + Messages.Criteria_CaseSensitive;
		}
		return Messages.Criteria_StartWith;
	}

	public boolean isCasesensitive() {
		return casesensitive;
	}

	public void setCasesensitive(boolean casesensitive) {
		this.casesensitive = casesensitive;
	}
}
