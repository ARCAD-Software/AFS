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
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * This class represent and equality test between and Entity's Attribute and a constant value.
 * 
 * <p>
 * This value can be an integer or another value type in its String representation. String representations
 * of other type depends on the Storage and the Mapper technologie.
 * 
 */
public class EqualCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria {

	private String attribute;
	private String value;
	private Integer intval;
	private Boolean casesensitive; // Note that this option is not compatible with the intval one.

	/**
	 * 
	 */
	public EqualCriteria() {
		super();
	}
	
	/**
	 * Equality test with string or integer value.
	 * @param attribute
	 * @param value
	 */
	public EqualCriteria(String attribute, String value) {
		this();
		this.attribute = attribute;
		this.value = value;
	}

	/**
	 * Equality test with integer value. 
	 * @param attribute
	 * @param value
	 */
	public EqualCriteria(String attribute, int value) {
		this();
		this.attribute = attribute;
		this.intval = value;
		this.casesensitive = false;
	}
	
	public EqualCriteria(String attribute, Integer intval, String value, Boolean casesensitive) {
		this();
		this.attribute = attribute;
		this.intval = intval;
		this.value = value;
		this.casesensitive = casesensitive;
	}
	
	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		ReferenceLine refline = context.getEntity().getReferenceLine(attribute);
		if (refline == null) {
			return ConstantCriteria.FALSE;
		}
		if (refline.isMultiLinkList()) {
			Activator.getInstance().warn("Equals Criteria with multi-link references is not supported: " + toString());
			return ConstantCriteria.FALSE;
		}
		if (refline.isLinkList()) {
			String preLinkCode = refline.getPreLinkCodes();
			if (preLinkCode.isEmpty()) {
				preLinkCode = null;
			}
			String postLinkCode = refline.getPostLinkCodes();
			if (postLinkCode.isEmpty()) {
				int i = 0;
				if (intval != null) {
					i = intval;
				} else if (value != null) {
					try {
						i = Integer.parseInt(value);
					} catch (NumberFormatException e) {
						Activator.getInstance().warn("Invalid Equals Criteria with terminal link reference: " + toString());
					}
				}
				return new LinkCriteria(i, refline.getFirstLinkCode(), preLinkCode).reduce(context);
			}
			LinkEqualCriteria lec;
			if (intval != null) {
				lec = new LinkEqualCriteria(preLinkCode, refline.getFirstLinkCode(), postLinkCode, null, intval.toString(), (casesensitive == null) || casesensitive);
			} else {
				lec = new LinkEqualCriteria(preLinkCode, refline.getFirstLinkCode(), postLinkCode, null, value, (casesensitive == null) || casesensitive);
			}
			return lec.reduce(context);
		}
		context.useReference(refline);
		if ((value == null) && (intval == null)) {
			return new IsNullCriteria(attribute);
		}
		return this;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new EqualCriteria(attribute, intval, value, casesensitive);
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof EqualCriteria) &&
			nullsOrEquals(attribute, ((EqualCriteria) obj).attribute) &&
			nullsOrEquals(intval, ((EqualCriteria) obj).intval) &&
			nullsOrEquals(value, ((EqualCriteria) obj).value) &&
			(isCasesensitive() == ((EqualCriteria) obj).isCasesensitive());
	}
	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		if (intval != null) {
			return intval.equals(bean.getInt(attribute));
		}
		if ((value == null) || (value.length() == 0)) {
			return bean.get(attribute) == null;
		}
		if (isCasesensitive()) {
			return value.equals(bean.getString(attribute));
		}
		return value.equalsIgnoreCase(bean.getString(attribute));
	}

	@Override
	public String getAttribute() {
		return attribute;
	}

	public String getValue() {
		return value;
	}

	public Integer getIntval() {
		return intval;
	}

	@Override
	public void setAttribute(String code) {
		attribute = code;
	}
	
	public void setValue(String value) {
		this.value = value;
	}
	
	public void setIntval(Integer value) {
		intval = value;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if (attribute != null) {
			sb.append(attribute);
		}
		if (intval != null) {
			sb.append(Messages.Criteria_Equal);
			sb.append(intval);
		} else if (value != null) {
			sb.append(Messages.Criteria_EqualString);
			sb.append(value);
			sb.append('"');
			if (isCasesensitive()) {
				sb.append(Messages.Criteria_CaseSensitive);
			}
		} else {
			sb.append(Messages.Criteria_IsNull);
		}
		return sb.toString();
	}

	public boolean isCasesensitive() {
		return (casesensitive == null) || casesensitive;
	}

	public void setCasesensitive(boolean casesensitive) {
		this.casesensitive = casesensitive;
	}
}
