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
 * <p>If the <code>attribute</code> is used then the link is relative to the attribute value. The attribute must
 * be a reference and the linkcode is then relative to the attribute type.
 * 
 * <p>This criteria can provide multiples identical results.
 */
public class LinkCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria {

	private String attribute;
	private String linkCode;
	private int id;
	
	public LinkCriteria(int id, String type) {
		this();
		this.id = id;
		this.linkCode = type;
	}
	
	public LinkCriteria(int id, String type, String attribute) {
		this(id, type);
		this.attribute = attribute;
	}

	public LinkCriteria() {
		super();
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new LinkCriteria(id, linkCode, attribute);
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		if (id > 0) {
			if ((attribute == null) || 
					(attribute.length() == 0)) {
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
		}
		return ConstantCriteria.FALSE;
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof LinkCriteria) && // 
			(id == ((LinkCriteria)obj).id) && //
			nullsOrEquals(linkCode, ((LinkCriteria)obj).linkCode) && //
			nullsOrEquals(attribute, ((LinkCriteria)obj).attribute);
	}

	@SuppressWarnings("rawtypes")

	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		Object o = bean.get(attribute);
		if (o instanceof List) {
			for(Object x:((List)o)) {
				if ((x instanceof IIdentifiedBean) && (((IIdentifiedBean)x).getId() == id)) {
					return true;
				}
			}
		}
		return false;
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
		sb.append(String.format(Messages.Criteria_LinkedThrough,linkCode));
		sb.append('#');
		sb.append(id);
		return sb.toString();
	}

}
