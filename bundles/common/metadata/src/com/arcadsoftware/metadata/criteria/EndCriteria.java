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

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Test if the attribute values is terminated with the given text.
 */
public class EndCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria {

	private String attribute;
	private String value;
	private boolean casesensitive;

	/**
	 * Default constructor
	 */
	public EndCriteria() {
		super();
	}
	
	/**
	 * @param attribute 
	 * @param string
	 * @param generator associated generator.
	 */
	public EndCriteria(String attribute, String value) {
		super();
		this.attribute = attribute;
		this.value = value;
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		if (value == null) {
			return ConstantCriteria.TRUE;
		}
		ReferenceLine attributeRef = context.getEntity().getAttributeLine(attribute);
		if (attributeRef != null) {
			context.useReference(attributeRef);
			return this;
		}
		return ConstantCriteria.FALSE;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new EndCriteria(attribute,value);
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof EndCriteria) &&
		attribute.equals(((EndCriteria)obj).attribute) &&
		value.equals(((EndCriteria)obj).value) &&
		(casesensitive == ((EndCriteria)obj).casesensitive);
	}

	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		String s = bean.getString(attribute);
		if (s == null) {
			return false;
		}
		if (casesensitive) {
			return s.endsWith(value);
		}
		return s.toLowerCase().endsWith(value.toLowerCase());
	}

	public String getAttribute() {
		return attribute;
	}

	public String getValue() {
		return value;
	}

	public boolean isCasesensitive() {
		return casesensitive;
	}

	public void setAttribute(String code) {
		attribute = code;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public void setCasesensitive(boolean casesensitive) {
		this.casesensitive = casesensitive;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(attribute);
		sb.append(Messages.Criteria_EndWith);
		if (casesensitive) {
			sb.append(Messages.Criteria_CaseSensitive);
		}
		sb.append('"');
		sb.append(value);
		sb.append('"');
		return sb.toString();
	}
}
