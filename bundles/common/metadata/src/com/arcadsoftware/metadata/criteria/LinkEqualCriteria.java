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

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Test an attribute of linked items.
 * 
 * <p>The <code>linkcode</code> is applied to the selected entity from witch a column is tested.  
 */
public class LinkEqualCriteria extends AbstractLinkTestCriteria implements Cloneable, IAttributesCriteria {

	private Boolean casesensitive;
	private String secondAttribute;

	/**
	 * 
	 * @param linkCode a non null link code of the current entity.
	 * @param attribute an attribute reference line starting from the link target entity. 
	 * @param value the constant value to test.
	 */
	public LinkEqualCriteria(String linkCode, String attribute, String value) {
		super(linkCode, attribute, value);
	}

	/**
	 * 
	 * @param reference a attribute reference line from which the link code is applicable, may be null.
	 * @param linkCode a non null link code.
	 * @param attribute an attribute reference line starting from the link target entity. 
	 * @param value the constant value to test.
	 */
	public LinkEqualCriteria(String reference, String linkCode, String attribute, String value) {
		super(reference, linkCode, attribute, value);
	}

	/**
	 * 
	 * @param reference a attribute reference line from which the link code is applicable, may be null.
	 * @param linkCode a non null link code.
	 * @param attribute an attribute reference line starting from the link target entity.
	 * @param secondAttribute another attribure reference line (from the tested entity) used to be compared with the 
	 * @param value the constant value to test.
	 * @param casesensitive
	 */
	public LinkEqualCriteria(String reference, String linkCode, String attribute, String secondAttribute, String value, Boolean casesensitive) {
		super(reference, linkCode, attribute, value);
		this.secondAttribute = secondAttribute;
		this.casesensitive = casesensitive;
	}

	public LinkEqualCriteria() {
		super();
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof LinkEqualCriteria) && (((LinkEqualCriteria) obj).casesensitive == casesensitive) && nullsOrEquals(((LinkEqualCriteria) obj).secondAttribute, secondAttribute) && super.equals(obj);
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new LinkEqualCriteria(getReference(), getLinkCode(), getAttribute(), secondAttribute, getValue(), casesensitive);
	}
	
	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		ReferenceLine secondRef = null;
		if (secondAttribute != null) {
			secondRef = context.getEntity().getAttributeLine(secondAttribute);
			if (secondRef == null) {
				return ConstantCriteria.FALSE;
			}
		}
		ISearchCriteria result = super.reduce(context);
		if ((result == this) && (secondAttribute != null)) {
			context.useReference(secondRef);
		}
		return result;
	}

	@Override
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		if (secondAttribute == null) {
			return super.test(bean, currentUser);
		}
		if (getReference() != null) {
			Object o = bean.get(getReference());
			if (o instanceof BeanMap) {
				bean = (BeanMap) o;
			} else {
				return false;
			}
		}
		Object o = bean.get(getLinkCode());
		if (o instanceof BeanMapList) {
			Object val = bean.get(secondAttribute);
			for (BeanMap b: (BeanMapList) o) {
				if (val == null) {
					if (b.getString(getAttribute()) == null) {
						return true;
					}
				} else if (test(b.getString(getAttribute()), val)) {
					return true;
				}
			}
		}
		return false;
	}

	@Override
	protected boolean test(Object attributeValue, Object value) {
		if (attributeValue == null) {
			return value.toString().length() == 0;
		}
		if (isCasesensitive()) {
			return value.equals(attributeValue);
		}
		return value.toString().equalsIgnoreCase(attributeValue.toString());
	}
	
	@Override
	public String toString() {
		if (secondAttribute == null) {
			return super.toString();
		}
		StringBuilder sb = new StringBuilder();
		if (getReference() != null) {
			sb.append(getReference());
			sb.append(' ');
		}
		sb.append(String.format(Messages.Criteria_LinkedThrough, getLinkCode()));
		sb.append(getAttribute());
		sb.append(getTestString());
		sb.append(secondAttribute);
		return sb.toString();
	}

	@Override
	protected String getTestString() {
		if (isCasesensitive()) {
			return Messages.Criteria_Equal + Messages.Criteria_CaseSensitive;
		}
		return Messages.Criteria_Equal;
	}

	public boolean isCasesensitive() {
		return (casesensitive == null) || casesensitive.booleanValue();
	}

	public void setCasesensitive(boolean casesensitive) {
		this.casesensitive = casesensitive;
	}
	
	public String getSecondAttribute() {
		return secondAttribute;
	}

	public void setSecondAttribute(String secondAttribute) {
		this.secondAttribute = secondAttribute;
	}
}
