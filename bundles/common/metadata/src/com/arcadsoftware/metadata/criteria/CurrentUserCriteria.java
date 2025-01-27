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
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Test attribute value according to current user properties.
 * 
 * <p>
 * Note for Mapper developers : This criteria is a metadata criteria, it always reduce to a simpler criteria. In some
 * case it can perform direct database request to select data (during reduction process).
 */
public class CurrentUserCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria {

	private String linkCode;
	private String attribute;
	private String userAttribute;

	public CurrentUserCriteria() {
		super();
	}

	public CurrentUserCriteria(String attribute) {
		super();
		this.attribute = attribute;
	}

	public CurrentUserCriteria(String attribute, String userAttribute) {
		super();
		this.attribute = attribute;
		this.userAttribute = userAttribute;
	}

	public CurrentUserCriteria(String attribute, String linkCode, String userAttribute) {
		super();
		this.attribute = attribute;
		this.userAttribute = userAttribute;
		this.linkCode = linkCode;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new CurrentUserCriteria(attribute, linkCode, userAttribute);
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		if (context.getCurrentUser() == null) {
			return ConstantCriteria.TRUE;
		}
		int i = 0;
		if (userAttribute == null) {
			i = context.getCurrentUser().getId();
		} else if ("principal".equalsIgnoreCase(userAttribute)) { //$NON-NLS-1$
			i = context.getCurrentUser().getPrincipal();
		} else {
			MetaDataEntity uentity = context.getEntity("user"); //$NON-NLS-1$
			if (uentity != null) {
				BeanMap ub = uentity.getMapper().selection(uentity, context.getCurrentUser().getId(),
						userAttribute, false);
				if (ub != null) {
					i = ub.getInt(userAttribute);
				}
			}
		}
		if (linkCode != null) {
			return new LinkCriteria(i, linkCode, attribute).reduce(context);
		}
		if (isTestId()) {
			return new IdEqualCriteria(i);
		}
		ReferenceLine attributeRef = context.getEntity().getAttributeLine(attribute);
		if (attributeRef != null) {
			context.useReference(attributeRef);
			return new EqualCriteria(attribute, i);
		}
		return ConstantCriteria.FALSE;
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof CurrentUserCriteria) && //
				nullsOrEquals(attribute, ((CurrentUserCriteria) obj).attribute) && //
				nullsOrEquals(userAttribute, ((CurrentUserCriteria) obj).userAttribute);
	}


	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		if ((currentUser == null) || (bean == null)) {
			return true;
		}
		if (linkCode != null) {
			Object o = bean.get(linkCode);
			if (o instanceof BeanMapList) {
				for (BeanMap b: (BeanMapList) o) {
					if (currentUser.getId() == b.getInt(attribute)) {
						return true;
					}
				}
			}
			return false;
		}
		if (userAttribute == null) {
			if (isTestId()) {
				return bean.getId() == currentUser.getId();
			}
			return bean.getInt(attribute) == currentUser.getId();
		}
		if ("principal".equalsIgnoreCase(userAttribute)) { //$NON-NLS-1$
			return bean.getInt(attribute) == currentUser.getPrincipal();
		}
		return false;
	}

	public boolean isTestId() {
		return (attribute == null) || (attribute.length() == 0) || (".".equals(attribute)); //$NON-NLS-1$
	}
	
	public String getAttribute() {
		return attribute;
	}

	public void setAttribute(String code) {
		attribute = code;
	}

	public String getUserAttribute() {
		return userAttribute;
	}

	public void setUserAttribute(String userAttribute) {
		this.userAttribute = userAttribute;
	}

	public String getLinkCode() {
		return linkCode;
	}

	public void setLinkCode(String linkCode) {
		this.linkCode = linkCode;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(attribute);
		if (linkCode != null) {
			if (userAttribute == null) {
				sb.append(String.format(" is linked with \"%s\" to current user", linkCode));
			} else {
				sb.append(String.format(" is linked with \"%s\" to current user's %s", linkCode, userAttribute));
			}
		} else if (userAttribute == null) {
			sb.append(Messages.Criteria_IsCurrentUser);
		} else {
			sb.append(String.format(Messages.Criteria_IsCurrentUserAttribute, userAttribute));
		}
		return sb.toString();
	}

}
