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
public class UnlinkCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria, ILinkCriteria {

	private String attribute;
	private String linkCode;
	private int id;
	private boolean ignoreSubdivision;
	private boolean deleted;
	
	public UnlinkCriteria(int id, String type) {
		this();
		this.id = id;
		this.linkCode = type;
	}
	
	public UnlinkCriteria(int id, String type, String attribute) {
		this(id, type);
		this.attribute = attribute;
	}
	
	public UnlinkCriteria(int id, String type, String attribute, boolean ignoreSubdivision, boolean deleted) {
		this(id, type, attribute);
		this.ignoreSubdivision = ignoreSubdivision;
		this.deleted = deleted;
	}
	
	public UnlinkCriteria() {
		super();
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new UnlinkCriteria(id, linkCode, attribute, ignoreSubdivision, deleted);
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		if ((attribute == null) || (attribute.length() == 0)) {
			List<MetaDataLink> links = context.getEntity().getLinkChain(getLinkCodes());
			if (links != null) {
				context.useLinks(linkCode, links);
				return this;
			}
		} else {
			ReferenceLine attributeRef = context.getEntity().getAttributeLine(attribute);
			if ((attributeRef != null) && (attributeRef.size() > 0)) {
				MetaDataEntity e = attributeRef.getLastAttribute().getRefEntity();
				if (e != null) {
					List<MetaDataLink> links = e.getLinkChain(getLinkCodes());
					if (links != null) {
						context.useReference(attributeRef);
						context.useLinks(attribute + '/' + linkCode, links);
						return this;
					}
				}
			}
		}
		return ConstantCriteria.FALSE;
	}

	@Override
	public String[] getLinkCodes() {
		if (linkCode == null) {
			return new String[0];
		}
		return linkCode.split("\\."); //$NON-NLS-1$
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof UnlinkCriteria) && // 
			(id == ((UnlinkCriteria) obj).id) && //
			(ignoreSubdivision == ((UnlinkCriteria) obj).ignoreSubdivision) && //
			(deleted == ((UnlinkCriteria) obj).deleted) && //
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

	@Override
	public String getAttribute() {
		return attribute;
	}

	@Override
	public String getLinkCode() {
		return linkCode;
	}

	public int getId() {
		return id;
	}

	@Override
	public void setAttribute(String code) {
		attribute = code;
	}

	@Override
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
		if (ignoreSubdivision) {
			sb.append(Messages.Criteria_NoSubdivision);
		}
		if (deleted) {
			sb.append(Messages.Criteria_EvenDeleted);
		}
		if (id > 0) {
			sb.append('#');
			sb.append(id);
		} else {
			sb.append(Messages.Criteria_Any);
		}
		return sb.toString();
	}

	@Override
	public boolean isIgnoreSubdivision() {
		return ignoreSubdivision;
	}

	@Override
	public void setIgnoreSubdivision(boolean ignoreSubdivision) {
		this.ignoreSubdivision = ignoreSubdivision;
	}

	@Override
	public boolean isDeletedLinks() {
		return deleted;
	}

	@Override
	public void setDeletedLinks(boolean deleted) {
		this.deleted = deleted;
	}
}
