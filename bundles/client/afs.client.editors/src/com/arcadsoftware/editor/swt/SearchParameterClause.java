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
package com.arcadsoftware.editor.swt;

/**
 * This singleton allows to build clause for the search engine
 * <p>
 * This is a limited parser class, for a more complete one use com.arcadsoftware.client.utils.criteria.XCriteriaStream.
 * 
 * @author ARCAD Software
 * @deprecated Use com.arcadsoftware.client.utils.criteria.XCriteriaStream instead.
 */
@Deprecated
public class SearchParameterClause {

	public final static String CRITERIA_LABEL_EQUAL = "="; //$NON-NLS-1$
	public final static String CRITERIA_LABEL_LOWER = "<"; //$NON-NLS-1$
	public final static String CRITERIA_LABEL_GREATER = ">"; //$NON-NLS-1$
	public final static String CRITERIA_LABEL_LIKE = "like"; //$NON-NLS-1$
	public final static String CRITERIA_TAG_OPEN_BEGIN = "<"; //$NON-NLS-1$
	public final static String CRITERIA_TAG_OPEN_END = ">"; //$NON-NLS-1$
	public final static String CRITERIA_TAG_CLOSE_BEGIN = "</"; //$NON-NLS-1$
	public final static String CRITERIA_TAG_CLOSE_END = "/>"; //$NON-NLS-1$
	public final static String CRITERIA_AND_OPEN = CRITERIA_TAG_OPEN_BEGIN + "and" + CRITERIA_TAG_OPEN_END; //$NON-NLS-1$
	public final static String CRITERIA_AND_CLOSE = CRITERIA_TAG_CLOSE_BEGIN + "and" + CRITERIA_TAG_OPEN_END; //$NON-NLS-1$
	public final static String CRITERIA_ATTRIBUTE = " attribute=\""; //$NON-NLS-1$
	public final static String CRITERIA_VALUE_ATTRIBUTE = " value=\""; //$NON-NLS-1$
	public final static String CRITERIA_VALUE_ELEMENT_OPEN = CRITERIA_TAG_OPEN_BEGIN + "value" + CRITERIA_TAG_OPEN_END; //$NON-NLS-1$
	public final static String CRITERIA_VALUE_ELEMENT_CLOSE = CRITERIA_TAG_CLOSE_BEGIN + "value" //$NON-NLS-1$
			+ CRITERIA_TAG_OPEN_END;
	public final static String CRITERIA_EQUALS = CRITERIA_TAG_OPEN_BEGIN + "equals "; //$NON-NLS-1$
	public final static String CRITERIA_CONTAINS_OPEN = CRITERIA_TAG_OPEN_BEGIN + "contains "; //$NON-NLS-1$
	public final static String CRITERIA_CONTAINS_CLOSE = CRITERIA_TAG_CLOSE_BEGIN + "contains" + CRITERIA_TAG_OPEN_END; //$NON-NLS-1$
	public final static String CRITERIA_LOWER_THAN = CRITERIA_TAG_OPEN_BEGIN + "lowerthan "; //$NON-NLS-1$
	public final static String CRITERIA_GREATER_THAN = CRITERIA_TAG_OPEN_BEGIN + "greaterthan "; //$NON-NLS-1$
	public final static String CRITERIA_IS_NULL = CRITERIA_TAG_OPEN_BEGIN + "isnull "; //$NON-NLS-1$

	private static final SearchParameterClause instance = new SearchParameterClause();

	private SearchParameterClause() {
	}

	public static SearchParameterClause getInstance() {
		return instance;
	}

	/**
	 * This method adds the equals clause to the xml query when <param>value</param> is a NOT String. When the method is
	 * called, <param>value</param> has to be cast into a String.
	 *
	 * @param fieldName
	 * @param value
	 * @return the clause under a xml form
	 */
	public StringBuilder addEqualsClause(String fieldName, String value) {
		final StringBuilder xmlClause = new StringBuilder();
		xmlClause.append(CRITERIA_AND_OPEN)
				.append(CRITERIA_EQUALS)
				.append(CRITERIA_ATTRIBUTE).append(fieldName + "\"") //$NON-NLS-1$
				.append(CRITERIA_VALUE_ATTRIBUTE).append(value).append("\"") //$NON-NLS-1$
				.append(CRITERIA_TAG_CLOSE_END)
				.append(CRITERIA_AND_CLOSE);
		return xmlClause;
	}

	/**
	 * This method adds the equals clause to the xml query when <param>value</param> is a String.
	 *
	 * @param fieldName
	 * @param value
	 * @return the clause under a xml form
	 */
	public StringBuilder addEqualsClauseForString(String fieldName, String value) {
		final StringBuilder xmlClause = new StringBuilder();
		xmlClause.append(CRITERIA_AND_OPEN)
				.append(CRITERIA_CONTAINS_OPEN)
				.append(CRITERIA_ATTRIBUTE).append(fieldName + "\"").append(CRITERIA_TAG_OPEN_END) //$NON-NLS-1$
				.append(CRITERIA_VALUE_ELEMENT_OPEN).append(value).append(CRITERIA_VALUE_ELEMENT_CLOSE)
				.append(CRITERIA_CONTAINS_CLOSE)
				.append(CRITERIA_AND_CLOSE);
		return xmlClause;
	}

	/**
	 * This method adds the lower than (<=) clause to the xml query.
	 *
	 * @param fieldName
	 * @param value
	 * @return the clause under a xml form
	 */
	public StringBuilder addLowerThanClause(String fieldName, String value) {
		final StringBuilder xmlClause = new StringBuilder();
		xmlClause.append(CRITERIA_AND_OPEN)
				.append(CRITERIA_LOWER_THAN)
				.append(CRITERIA_ATTRIBUTE).append(fieldName + "\"") //$NON-NLS-1$
				.append(CRITERIA_VALUE_ATTRIBUTE).append(value).append("\"") //$NON-NLS-1$
				.append(CRITERIA_TAG_CLOSE_END)
				.append(CRITERIA_AND_CLOSE);
		return xmlClause;
	}

	/**
	 * This method adds the greater than (>=) clause to the xml query.
	 *
	 * @param fieldName
	 * @param value
	 * @return the clause under a xml form
	 */
	public StringBuilder addGreaterThanClause(String fieldName, String value) {
		final StringBuilder xmlClause = new StringBuilder();
		xmlClause.append(CRITERIA_AND_OPEN)
				.append(CRITERIA_GREATER_THAN)
				.append(CRITERIA_ATTRIBUTE).append(fieldName + "\"") //$NON-NLS-1$
				.append(CRITERIA_VALUE_ATTRIBUTE).append(value).append("\"") //$NON-NLS-1$
				.append(CRITERIA_TAG_CLOSE_END)
				.append(CRITERIA_AND_CLOSE);
		return xmlClause;
	}

	public StringBuilder addIsNullClause(String fieldName) {
		final StringBuilder xmlClause = new StringBuilder();
		xmlClause.append(CRITERIA_AND_OPEN)
				.append(CRITERIA_IS_NULL)
				.append(CRITERIA_ATTRIBUTE).append(fieldName + "\"") //$NON-NLS-1$
				.append(CRITERIA_TAG_CLOSE_END)
				.append(CRITERIA_AND_CLOSE);
		return xmlClause;
	}
}