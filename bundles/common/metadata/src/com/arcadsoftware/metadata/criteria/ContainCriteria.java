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
 * Test if a String attribute value contains the given text.
 */
public class ContainCriteria extends AbstractStringSearchCriteria {

	/**
	 * Default constructor
	 */
	public ContainCriteria() {
		super();
	}
	
	/**
	 * @param attribute 
	 * @param value
	 */
	public ContainCriteria(String attribute, String value) {
		super(attribute, value);
	}
	
	/**
	 * @param attribute 
	 * @param value
	 * @param casesensitive
	 */
	public ContainCriteria(String attribute, String value, boolean cs) {
		super(attribute, value, cs);
	}

	@Override
	public ContainCriteria clone() {
		return new ContainCriteria(getAttribute(), getValue(), isCasesensitive());
	}

	@Override
	protected AbstractLinkTestCriteria getLinkBasedCriteria(String preLinkCode, String firstLinkCode,
			String postLinkCode) {
		return new LinkContainCriteria(preLinkCode, firstLinkCode, postLinkCode, getValue(), isCasesensitive());
	}

	@Override
	protected boolean test(String toTestValue, String againstValue) {
		return toTestValue.contains(againstValue);
	}

	@Override
	protected String getCriteriaLabel() {
		return Messages.Criteria_Contain;
	}	
}
