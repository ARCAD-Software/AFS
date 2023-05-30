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
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Test an attribute of linked items.
 * <p>The <code>linkcode</code> is applied to the selected entity from witch a column is tested.  
 */
public class LinkEqualCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria {

	private String linkCode;
	private String reference;
	private String attribute;
	private String value;

	/**
	 * @param linkCode
	 * @param attribute
	 * @param value
	 */
	public LinkEqualCriteria(String linkCode, String attribute, String value) {
		super();
		this.linkCode = linkCode;
		this.attribute = attribute;
		this.value = value;
	}

	public LinkEqualCriteria(String reference, String linkCode, String attribute, String value) {
		this(linkCode, attribute, value);
		this.reference = reference;
	}

	public LinkEqualCriteria() {
		super();
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		MetaDataEntity entity;
		ReferenceLine referenceRef = null;
		if (reference == null) {
			entity = context.getEntity();
		} else {
			referenceRef = context.getEntity().getAttributeLine(attribute);
			if ((referenceRef != null) && (referenceRef.size() > 0)) {
				entity = referenceRef.getLastAttribute().getRefEntity();
			} else {
				return ConstantCriteria.FALSE;
			}
		}
		MetaDataLink link = entity.getLink(linkCode);
		if (link != null) {
			MetaDataEntity e = link.getRefEntity();
			if (e != null) {
				ReferenceLine ref = e.getAttributeLine(attribute);
				if (ref != null) {
					context.useLinkReference(link, ref);
					if (referenceRef != null) {
						context.useReference(referenceRef);
					}
					return this;
				}
			}
		}
		return ConstantCriteria.FALSE;
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof LinkEqualCriteria) && // 
				nullsOrEquals(value, ((LinkEqualCriteria) obj).value) && //
				nullsOrEquals(attribute, ((LinkEqualCriteria) obj).attribute) && //
				nullsOrEquals(reference, ((LinkEqualCriteria) obj).reference) && //
				nullsOrEquals(linkCode, ((LinkEqualCriteria) obj).linkCode);
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new LinkEqualCriteria(reference, linkCode, attribute, value);
	}

	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		if ((value == null) || (value.length() == 0)) {
			return false;
		}
		if (reference != null) {
			Object o = bean.get(reference);
			if (o instanceof BeanMap) {
				bean = (BeanMap)o;
			} else {
				return false;
			}
		}
		Object o = bean.get(linkCode);
		if (o instanceof BeanMapList) {
			for(BeanMap b:(BeanMapList)o) {
				if (value.equals(b.getString(attribute))) {
					return true;
				}
			}
		}
		return false;
	}

	public String getLinkCode() {
		return linkCode;
	}

	public String getAttribute() {
		return attribute;
	}

	public String getValue() {
		return value;
	}

	public String getReference() {
		return reference;
	}

	public void setAttribute(String code) {
		attribute = code;
	}
	
	public void setLinkCode(String linkCode) {
		this.linkCode = linkCode;
	}

	public void setReference(String reference) {
		this.reference = reference;
	}

	public void setValue(String value) {
		this.value = value;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if (reference != null) {
			sb.append(reference);
			sb.append(' ');
		}
		sb.append(String.format(Messages.Criteria_LinkedThrough,linkCode));
		sb.append(attribute);
		sb.append(Messages.Criteria_Equal);
		sb.append('"');
		sb.append(value);
		sb.append('"');
		return sb.toString();
	}
}
