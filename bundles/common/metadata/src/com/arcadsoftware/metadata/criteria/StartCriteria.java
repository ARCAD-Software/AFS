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
 * Test if the String value of an attribute start with the given text. 
 */
public class StartCriteria extends AbstractStringSearchCriteria {

	/**
	 * Default constructor
	 */
	public StartCriteria() {
		super();
	}
	
	/**
	 * 
	 * @param attribute 
	 * @param string
	 */
	public StartCriteria(String attribute, String value) {
		super(attribute, value);
	}

	/**
	 * 
	 * @param attribute
	 * @param value
	 * @param casesensitive
	 */
	public StartCriteria(String attribute, String value, boolean casesensitive) {
		super(attribute, value, casesensitive);
	}

	@Override
	public StartCriteria clone() {
		return new StartCriteria(getAttribute(), getValue(), isCasesensitive());
	}

	@Override
	protected AbstractLinkTestCriteria getLinkBasedCriteria(String preLinkCode, String firstLinkCode,
			String postLinkCode) {
		return new LinkStartCriteria(preLinkCode, firstLinkCode, postLinkCode, getValue(), isCasesensitive());
	}

	@Override
	protected boolean test(String toTestValue, String againstValue) {
		return toTestValue.startsWith(againstValue);
	}

	@Override
	protected String getCriteriaLabel() {
		return Messages.Criteria_StartWith;
	}
}
