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

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public abstract class AbstractStringSearchCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria {

	private String attribute;
	private String value;
	private boolean casesensitive;

	/**
	 * Default constructor
	 */
	public AbstractStringSearchCriteria() {
		super();
	}
	
	/**
	 * 
	 * @param attribute 
	 * @param string
	 */
	public AbstractStringSearchCriteria(String attribute, String value) {
		super();
		this.attribute = attribute;
		this.value = value;
	}

	/**
	 * 
	 * @param attribute
	 * @param value
	 * @param casesensitive
	 */
	public AbstractStringSearchCriteria(String attribute, String value, boolean casesensitive) {
		super();
		this.attribute = attribute;
		this.value = value;
		this.casesensitive = casesensitive;
	}

	@Override
	public final ISearchCriteria reduce(ICriteriaContext context) {
		if (value == null) {
			return ConstantCriteria.TRUE;
		}
		ReferenceLine refline = context.getEntity().getReferenceLine(attribute);
		if (refline == null) {
			return ConstantCriteria.FALSE;
		}			
		if (refline.isMultiLinkList()) {
			Activator.getInstance().warn("\"" + getCriteriaLabel() + "\" criteria with multi-link references is not supported: " + toString());
			return ConstantCriteria.FALSE;
		}
		if (!refline.isFinal()) {
			OrCriteria or = new OrCriteria();
			MetaDataEntity entity = refline.getLastAttribute().getRefEntity();
			for (MetaDataAttribute att: entity.getAttributes().values()) {
				if (att.isString()) {
					AbstractStringSearchCriteria c = this.clone();
					c.setAttribute(attribute + '.' + att.getCode());
					or.add(c);
				}
			}
			return or.reduce(context);
		}
		if (refline.isLinkList()) {
			String preLinkCode = refline.getPreLinkCodes();
			if (preLinkCode.isEmpty()) {
				preLinkCode = null;
			}
			String postLinkCode = refline.getPostLinkCodes();
			if (postLinkCode.isEmpty()) {
				Activator.getInstance().warn("Invalid \"" + getCriteriaLabel() + "\" Criteria with terminal link reference: " + toString());
			}
			return getLinkBasedCriteria(preLinkCode, refline.getFirstLinkCode(), postLinkCode).reduce(context);
		}
		context.useReference(refline);
		return this;
	}

	protected abstract AbstractLinkTestCriteria getLinkBasedCriteria(String preLinkCode, String firstLinkCode,	String postLinkCode);

	@Override
	public final boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		String s = bean.getString(attribute);
		if (s == null) {
			return false;
		}
		if (casesensitive) {
			return test(s, value);
		}
		return test(s.toLowerCase(), value.toLowerCase());
	}

	protected abstract boolean test(String toTestValue, String againstValue);

	public String getAttribute() {
		return attribute;
	}

	public void setAttribute(String attribute) {
		this.attribute = attribute;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public boolean isCasesensitive() {
		return casesensitive;
	}

	public void setCasesensitive(boolean casesensitive) {
		this.casesensitive = casesensitive;
	}

	@Override
	public boolean equals(Object obj) {
		return (this.getClass().isInstance(obj)) && //
		attribute.equals(((AbstractStringSearchCriteria) obj).attribute) && //
		value.equals(((AbstractStringSearchCriteria) obj).value) && //
		(casesensitive == ((AbstractStringSearchCriteria) obj).casesensitive);
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(attribute);
		sb.append(getCriteriaLabel());
		if (casesensitive) {
			sb.append(Messages.Criteria_CaseSensitive);
		}
		sb.append('"');
		sb.append(value);
		sb.append('"');
		return sb.toString();
	}

	protected abstract String getCriteriaLabel();

	@Override
	public AbstractStringSearchCriteria clone() {
		return null;
	}

}
