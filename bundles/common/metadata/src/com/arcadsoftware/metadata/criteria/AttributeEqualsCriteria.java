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
 * This class produce a test of the values of two attributes. 
 * <p>
 * If one of the reference code do not correspond to any declared attribute then
 * the test result is always FALSE.
 * <p>
 * This kind of test possess some limitation about multi-domain references. See the
 * interface <code>IAttributesCritria</code> for details.
 *  
 * Creation Date: 4 oct. 2011
 * @see IAttributesCriteria
 */
public class AttributeEqualsCriteria extends AbstractSearchCriteria implements IAttributesCriteria {

	private String attribute;
	private String secondAttribute;
	private Boolean casesensitive;

	public AttributeEqualsCriteria() {
		super();
	}

	public AttributeEqualsCriteria(String attribute1, String attribute2) {
		this();
		this.attribute = attribute1;
		this.secondAttribute = attribute2;
	}

	public AttributeEqualsCriteria(String attribute1, String attribute2, boolean casesensitive) {
		this(attribute1, attribute2);
		this.casesensitive = casesensitive;
	}
	
	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		if ((attribute == null) || (secondAttribute == null)) {
			return ConstantCriteria.FALSE;
		}
		if (attribute.equals(secondAttribute)) {
			return ConstantCriteria.TRUE;
		}
		ReferenceLine attributeRef = context.getEntity().getReferenceLine(attribute);
		if (attributeRef == null) {
			return ConstantCriteria.FALSE;
		}
		ReferenceLine secondRef = context.getEntity().getReferenceLine(secondAttribute);
		if (secondRef == null) {
			return ConstantCriteria.FALSE;
		}
		if (attributeRef.isMultiLinkList() || secondRef.isMultiLinkList()) {
			Activator.getInstance().warn("Equals Criteria between multi-link references is not supported: " + toString());
			return ConstantCriteria.FALSE;
		}
		if (attributeRef.isLinkList()) {
			if (secondRef.isLinkList()) {
				Activator.getInstance().warn("Equals Criteria using two link references is not supported: " + toString());
				return ConstantCriteria.FALSE;
			}
			String preLinkCode = attributeRef.getPreLinkCodes();
			if (preLinkCode.isEmpty()) {
				preLinkCode = null;
			}
			String postLinkCode = attributeRef.getPostLinkCodes();
			if (postLinkCode.isEmpty()) {
				Activator.getInstance().warn("Equals Criteria using direct link references compared to an attribute value is not supported: " + toString());
				return ConstantCriteria.FALSE;
			}
			return new LinkEqualCriteria(preLinkCode, attributeRef.getFirstLinkCode(), postLinkCode, secondAttribute, null, (casesensitive != null) && casesensitive).reduce(context);
		}
		if (secondRef.isLinkList()) {
			String preLinkCode = secondRef.getPreLinkCodes();
			if (preLinkCode.isEmpty()) {
				preLinkCode = null;
			}
			String postLinkCode = secondRef.getPostLinkCodes();
			if (postLinkCode.isEmpty()) {
				Activator.getInstance().warn("Equals Criteria using direct link references compared to an attribute value is not supported: " + toString());
				return ConstantCriteria.FALSE;
			}
			return new LinkEqualCriteria(preLinkCode, secondRef.getFirstLinkCode(), postLinkCode, attribute, null, (casesensitive == null) || casesensitive).reduce(context);
		}		
		context.useReference(attributeRef);
		context.useReference(attributeRef);
		return this;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new AttributeEqualsCriteria(attribute, secondAttribute);
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof AttributeEqualsCriteria) && //
				((nullsOrEquals(attribute, ((AttributeEqualsCriteria) obj).attribute) && //
				nullsOrEquals(casesensitive, ((AttributeEqualsCriteria) obj).casesensitive) &&
				  nullsOrEquals(secondAttribute, ((AttributeEqualsCriteria) obj).secondAttribute)) ||
				 (nullsOrEquals(secondAttribute, ((AttributeEqualsCriteria) obj).attribute) && //
				  nullsOrEquals(attribute, ((AttributeEqualsCriteria) obj).secondAttribute)));
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(attribute);
		sb.append(Messages.Criteria_Equal);
		if ((casesensitive == null) || casesensitive) {
			sb.append(Messages.Criteria_CaseSensitive);
		}
		sb.append(secondAttribute);
		return sb.toString();
	}

	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		Object v1 = bean.get(attribute);
		Object v2 = bean.get(secondAttribute);
		if (v1 == null) {
			return v2 == null;
		}
		if ((casesensitive == null) || casesensitive) {
			return v1.equals(v2);
		} else {
			return (v2 != null) && v1.toString().equalsIgnoreCase(v2.toString());
		}
	}

	public void setAttribute(String code) {
		attribute = code;
	}

	public String getAttribute() {
		return attribute;
	}

	public void setSecondAttribute(String code) {
		secondAttribute = code;
	}

	public String getSecondAttribute() {
		return secondAttribute;
	}

	public boolean isCasesensitive() {
		return (casesensitive == null) || casesensitive;
	}

	public void setCasesensitive(boolean casesensitive) {
		this.casesensitive = casesensitive;
	}

}
