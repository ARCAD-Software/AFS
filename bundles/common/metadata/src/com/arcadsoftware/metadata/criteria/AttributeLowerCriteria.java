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
 * Test if the value of an attributes is lower than the value of another one. 
 */
public class AttributeLowerCriteria extends AbstractSearchCriteria implements IAttributesCriteria {

	private String attribute;
	private String secondAttribute;

	public AttributeLowerCriteria() {
		super();
	}

	public AttributeLowerCriteria(String attribute1, String attribute2) {
		super();
		this.attribute = attribute1;
		this.secondAttribute = attribute2;
	}
	
	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
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
		return new AttributeLowerCriteria(attribute, secondAttribute);
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof AttributeLowerCriteria) && //
				nullsOrEquals(attribute, ((AttributeLowerCriteria)obj).attribute) && //
				nullsOrEquals(secondAttribute, ((AttributeLowerCriteria)obj).secondAttribute);
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(attribute);
		sb.append(Messages.Criteria_Equal);
		sb.append(secondAttribute);
		return sb.toString();
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		Object o1 = bean.get(attribute);
		Object o2 = bean.get(secondAttribute);
		if (o1 instanceof Comparable) {
			return ((Comparable)o1).compareTo(o2) < 0;
		}
		if (o2 instanceof Comparable) {
			return ((Comparable)o2).compareTo(o1) >= 0;
		}
		return false;
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
