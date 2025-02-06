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
 * Test if the attribute values is terminated with the given text.
 */
public class EndCriteria extends AbstractStringSearchCriteria {

	/**
	 * Default constructor
	 */
	public EndCriteria() {
		super();
	}
	
	/**
	 * @param attribute 
	 * @param string
	 */
	public EndCriteria(String attribute, String value) {
		super(attribute, value);
	}
	
	/**
	 * @param attribute 
	 * @param string
	 * @param caseSensitive
	 */
	public EndCriteria(String attribute, String value, boolean cs) {
		super(attribute, value, cs);
	}

	@Override
	public EndCriteria clone() {
		return new EndCriteria(getAttribute(), getValue(), isCasesensitive());
	}

	@Override
	protected AbstractLinkTestCriteria getLinkBasedCriteria(String preLinkCode, String firstLinkCode,
			String postLinkCode) {
		return new LinkEndCriteria(preLinkCode, firstLinkCode, postLinkCode, getValue(), isCasesensitive());
	}

	@Override
	protected boolean test(String toTestValue, String againstValue) {
		return toTestValue.endsWith(againstValue);
	}

	@Override
	protected String getCriteriaLabel() {
		return Messages.Criteria_EndWith;
	}
}
