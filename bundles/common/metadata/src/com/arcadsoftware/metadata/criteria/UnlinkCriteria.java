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

import java.util.List;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.IIdentifiedBean;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Provide a search criteria able to find some external reference from a linked table.
 * 
 * <p>Usable for N to N linked tables, with intermediates links.
 * 
 * 
 */
public class UnlinkCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria {

	private String attribute;
	private String linkCode;
	private int id;
	
	public UnlinkCriteria(int id, String type) {
		this();
		this.id = id;
		this.linkCode = type;
	}
	
	public UnlinkCriteria(int id, String type, String attribute) {
		this(id, type);
		this.attribute = attribute;
	}
	
	public UnlinkCriteria() {
		super();
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new UnlinkCriteria(id, linkCode, attribute);
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		if ((attribute == null) || (attribute.length() == 0)) {
			MetaDataLink link = context.getEntity().getLink(linkCode);
			if (link != null) {
				context.useLinkReference(link, null);
				return this;
			}
		} else {
			ReferenceLine attributeRef = context.getEntity().getAttributeLine(attribute);
			if ((attributeRef != null) && (attributeRef.size() > 0)) {
				MetaDataEntity e = attributeRef.getLastAttribute().getRefEntity();
				if (e != null) {
					MetaDataLink link = e.getLink(linkCode);
					if (link != null) {
						context.useReference(attributeRef);
						context.useLinkReference(link, null);
						return this;
					}
				}
			}
		}
		return ConstantCriteria.FALSE;
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof UnlinkCriteria) && // 
			(id == ((UnlinkCriteria) obj).id) && //
			nullsOrEquals(linkCode, ((UnlinkCriteria) obj).linkCode) && //
			nullsOrEquals(attribute, ((UnlinkCriteria) obj).attribute);
	}

	@SuppressWarnings("rawtypes")

	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		Object o;
		if ((attribute == null) || (attribute.length() == 0)) {
			o = bean.get(linkCode);
		} else {
			o = bean.get(attribute);
		}
		if (o instanceof List) {
			for (Object x: ((List) o)) {
				if ((id <= 0) || ((x instanceof IIdentifiedBean) && (((IIdentifiedBean) x).getId() == id))) {
					return false;
				}
			}
		}
		return true;
	}

	public String getAttribute() {
		return attribute;
	}

	public String getLinkCode() {
		return linkCode;
	}

	public int getId() {
		return id;
	}

	public void setAttribute(String code) {
		attribute = code;
	}

	public void setLinkCode(String linkCode) {
		this.linkCode = linkCode;
	}

	public void setId(int id) {
		this.id = id;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if (attribute != null) {
			sb.append(attribute);
			sb.append(' ');
		}
		sb.append(String.format(Messages.Criteria_UnLinkedThrough, linkCode));
		if (id > 0) {
			sb.append('#');
			sb.append(id);
		} else {
			sb.append(Messages.Criteria_Any);
		}
		return sb.toString();
	}

}
