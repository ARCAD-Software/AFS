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
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Test if the internal Identifier (ID) of the selected data is member of the given set of values.
 */
public class IdInListCriteria extends AbstractSearchCriteria implements Cloneable {

	private Set<Integer> ids;
	
	public IdInListCriteria(int... id) {
		super();
		ids = new TreeSet<Integer>();
		for(int i: id) {
			ids.add(i);
		}
	}

	public IdInListCriteria(Collection<Integer> ids) {
		super();
		this.ids = new TreeSet<Integer>();
		for(int i: ids) {
			this.ids.add(i);
		}
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new IdInListCriteria(ids);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof IdInListCriteria)  {
			if ((ids == null) || ids.isEmpty()) {
				return (((IdInListCriteria)obj).ids == null) || ((IdInListCriteria)obj).ids.isEmpty();
			}
			if ((((IdInListCriteria)obj).ids == null) || ((IdInListCriteria)obj).ids.isEmpty()) { 
				return false;
			}
			return ((IdInListCriteria)obj).ids.equals(ids);
		}
		return false;
	}
	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		return (ids != null) && (bean != null) && ids.contains(bean.getId());
	}

	public Set<Integer> getIds() {
		return ids;
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
		if (ids.size() == 1) {
			return new IdEqualCriteria(ids.iterator().next());
		}
		OrCriteria or = new OrCriteria();
		for(int i: ids) {
			or.add(new IdEqualCriteria(i));
		}
		return or;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(Messages.Criteria_Id);
		sb.append(Messages.Criteria_Equal);
		sb.append(ids);
		return sb.toString();
	}

}
