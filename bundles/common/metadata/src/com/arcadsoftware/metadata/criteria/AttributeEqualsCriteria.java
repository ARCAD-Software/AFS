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
import com.arcadsoftware.metadata.ReferenceLine;
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

	public AttributeEqualsCriteria() {
		super();
	}

	public AttributeEqualsCriteria(String attribute1, String attribute2) {
		super();
		this.attribute = attribute1;
		this.secondAttribute = attribute2;
	}
	
	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		if ((attribute == null) || (secondAttribute == null)) {
			return ConstantCriteria.FALSE;
		}
		if (attribute.equals(secondAttribute)) {
			return ConstantCriteria.TRUE;
		}
		ReferenceLine attributeRef = context.getEntity().getAttributeLine(attribute);
		if (attributeRef == null) {
			return ConstantCriteria.FALSE;
		}
		context.useReference(attributeRef);
		attributeRef = context.getEntity().getAttributeLine(secondAttribute);
		if (attributeRef == null) {
			return ConstantCriteria.FALSE;
		}
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
				((nullsOrEquals(attribute, ((AttributeEqualsCriteria)obj).attribute) && //
				  nullsOrEquals(secondAttribute, ((AttributeEqualsCriteria)obj).secondAttribute)) ||
				 (nullsOrEquals(secondAttribute, ((AttributeEqualsCriteria)obj).attribute) && //
				  nullsOrEquals(attribute, ((AttributeEqualsCriteria)obj).secondAttribute)));
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(attribute);
		sb.append(Messages.Criteria_Equal);
		sb.append(secondAttribute);
		return sb.toString();
	}

	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		return nullsOrEquals(bean.get(attribute), bean.get(secondAttribute));
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

}
