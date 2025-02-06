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
 * 
 * <p>
 * <b>Note that the negation of this criteria</b> is not the common meaning of "Not linked to" it does not 
 * return all the data not linked to the given element, but the data linked to other elements, this
 * include any data which may be linked with the designated element but which is also linked to another one, and 
 * exclude any data with no link at all.
 * 
 */
public class LinkCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria, ILinkCriteria {

	private String attribute;
	private String linkCode;
	private int id;
	private boolean ignoreSubdivision;
	private boolean deleted;
	
	public LinkCriteria(int id, String linkCode) {
		this();
		this.id = id;
		this.linkCode = linkCode;
	}
	
	public LinkCriteria(int id, String linkCode, String attribute) {
		this(id, linkCode);
		this.attribute = attribute;
	}
	
	public LinkCriteria(int id, String linkCode, String attribute, boolean ignoreSubdivision) {
		this(id, linkCode, attribute);
		this.ignoreSubdivision = ignoreSubdivision;
	}
	
	public LinkCriteria(int id, String linkCode, String attribute, boolean ignoreSubdivision, boolean deleted) {
		this(id, linkCode, attribute, ignoreSubdivision);
		this.deleted = deleted;
	}

	public LinkCriteria() {
		super();
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new LinkCriteria(id, linkCode, attribute, ignoreSubdivision);
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		if (id > 0) {
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
						if ((links != null) && !links.isEmpty()) {
							context.useReference(attributeRef);
							context.useLinks(attribute + '/' + linkCode, links);
							return this;
						}
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
		return (obj instanceof LinkCriteria) && // 
			(id == ((LinkCriteria) obj).id) && //
			(ignoreSubdivision == ((LinkCriteria) obj).ignoreSubdivision) && //
			(deleted == ((LinkCriteria) obj).deleted) && //
			nullsOrEquals(linkCode, ((LinkCriteria) obj).linkCode) && //
			nullsOrEquals(attribute, ((LinkCriteria) obj).attribute);
	}

	@SuppressWarnings("rawtypes")

	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		if (id > 0) {
			Object o;
			if ((attribute == null) || (attribute.length() == 0)) {
				o = bean.get(linkCode);
			} else {
				o = bean.get(attribute);
			}
			if (o instanceof List) {
				for(Object x:((List) o)) {
					if ((x instanceof IIdentifiedBean) && (((IIdentifiedBean) x).getId() == id)) {
						return true;
					}
				}
			}
		}
		return false;
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
		sb.append(String.format(Messages.Criteria_LinkedThrough, linkCode));
		if (ignoreSubdivision) {
			sb.append(Messages.Criteria_NoSubdivision);
		}
		if (deleted) {
			sb.append(Messages.Criteria_EvenDeleted);
		}
		sb.append('#');
		sb.append(id);
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
