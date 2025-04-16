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
import com.arcadsoftware.beanmap.IDeletableBean;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Test if the current entity or the referenced one is deleted or not.
 * 
 * The attribute must be a non final attribute or <code>null</code> to test the currently selected data.
 */
public class DeletedCriteria extends AbstractSearchCriteria implements IAttributeCriteria {

	private String attribute; 
	
	public DeletedCriteria(String attribute) {
		super();
		this.attribute = attribute;
	}

	public DeletedCriteria() {
		super();
	}
	
	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		if ((attribute == null) || (attribute.length() == 0)) {
			return this;
		}
		ReferenceLine attributeRef = context.getEntity().getAttributeLine(attribute);
		if ((attributeRef == null) || attributeRef.isEmpty()) {
			return ConstantCriteria.TRUE;
		}
		if (attributeRef.isFinal()) {
			return ConstantCriteria.FALSE;
		}
		MetaDataEntity refEntity = attributeRef.getLastAttribute().getRefEntity();
		if ((refEntity == null) || (refEntity.getMetadata().get("deleteCol") == null)) {
			return ConstantCriteria.FALSE;
		}
		context.useReference(attributeRef);
		return this;
	}
	
	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		if ((attribute == null) || (attribute.length() == 0)) {
			return bean.isDeleted();
		}
		Object o = bean.get(attribute);
		return (o == null) || ((o instanceof IDeletableBean) && (((IDeletableBean) o).isDeleted()));
	}
	@Override
	public DeletedCriteria clone() {
		return new DeletedCriteria(attribute);
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof DeletedCriteria) && nullsOrEquals(attribute, ((DeletedCriteria)obj).attribute);
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if ((attribute == null) || (attribute.length() == 0)) {
			sb.append("The item");
		} else {
			sb.append(attribute);
		}
		sb.append(" is deleted");
		return sb.toString();
	}
	
	public String getAttribute() {
		return attribute;
	}

	public void setAttribute(String attribute) {
		this.attribute = attribute;
	}

}
