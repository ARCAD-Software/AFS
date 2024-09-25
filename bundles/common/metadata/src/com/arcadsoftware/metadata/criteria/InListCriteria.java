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

import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Test if the internal Identifier (ID) or the given reference line of the selected data is member of the given set of values.
 */
public class InListCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria {

	private String attribute;
	private Set<Integer> ids;
	
	public InListCriteria(int... id) {
		super();
		ids = new TreeSet<Integer>();
		for(int i: id) {
			ids.add(i);
		}
	}
	
	public InListCriteria(String attribute, int... id) {
		this(id);
		this.attribute = attribute;
	}

	public InListCriteria(Collection<Integer> ids) {
		super();
		this.ids = new TreeSet<Integer>();
		for(int i: ids) {
			this.ids.add(i);
		}
	}

	public InListCriteria(String attribute, Collection<Integer> ids) {
		this(ids);
		this.attribute = attribute;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new InListCriteria(attribute, ids);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof InListCriteria) {
			if (!nullsOrEquals(attribute, ((InListCriteria) obj).attribute)) {
				return false;
			}
			if ((ids == null) || ids.isEmpty()) {
				return (((InListCriteria) obj).ids == null) || ((InListCriteria) obj).ids.isEmpty();
			}
			if ((((InListCriteria) obj).ids == null) || ((InListCriteria) obj).ids.isEmpty()) { 
				return false;
			}
			return ((InListCriteria) obj).ids.equals(ids);
		}
		return false;
	}
	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		return (ids != null) && (bean != null) && ids.contains(bean.getId());
	}

	public Set<Integer> getIds() {
		return ids;
	}

	public String getIds(String separator) {
		if (ids.size() == 0) {
			return ""; //$NON-NLS-1$
		}
		StringBuilder sb = new StringBuilder(ids.size() * (separator.length() + 1));
		boolean first = true;
		for (Integer i: ids) {
			if (first) {
				first = false;
			} else {
				sb.append(separator);
			}
			sb.append(i);
		}
		return sb.toString();
	}
	
	public void add(int... id) {
		if (ids == null) {
			ids = new TreeSet<Integer>();
		}
		for(int i: id) {
			ids.add(i);
		}
	}
	
	public ISearchCriteria reduce(ICriteriaContext context) {
		if ((ids == null) || ids.isEmpty()) {
			return ConstantCriteria.TRUE;
		}
		if (attribute != null) {
			ReferenceLine rl = context.getEntity().getAttributeLine(attribute);
			if (rl == null) {
				return ConstantCriteria.FALSE;
			}
			context.useReference(rl);
		}
		if (ids.size() == 1) {
			if (attribute == null) {
				return new IdEqualCriteria(ids.iterator().next());
			}
			return new EqualCriteria(attribute, ids.iterator().next());
		}
		if (ids.size() <= 20) {
			OrCriteria or = new OrCriteria();
			for (int i: ids) {
				if (attribute == null) {
					or.add(new IdEqualCriteria(i));
				} else {
					or.add(new EqualCriteria(attribute, i));
				}
			}
			return or;
		}
		// Bigger set must be handler in another way !
		return this;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if (attribute != null) {
			sb.append(attribute);
		} else {
			sb.append(Messages.Criteria_Id);
		}
		sb.append(Messages.Criteria_Equal);
		sb.append(ids);
		return sb.toString();
	}

	@Override
	public void setAttribute(String code) {
		attribute = code;
	}

	@Override
	public String getAttribute() {
		return attribute;
	}

}
