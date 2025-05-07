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

import java.util.ArrayList;
import java.util.Collection;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Disjunction of any criteria
 */
public class OrCriteria extends AbstractSearchCriteria implements Cloneable, ISubCriteria {

	private final ArrayList<ISearchCriteria> criterias;
	
	/**
	 * 
	 */
	public OrCriteria() {
		super();
		criterias = new ArrayList<ISearchCriteria>();
	}

	public OrCriteria(ISearchCriteria... val) {
		super();
		criterias = new ArrayList<ISearchCriteria>(val.length);
		for (ISearchCriteria v:val) {
			if (v != null) {
				criterias.add(v);
			}
		}
	}

	/**
	 * @param values
	 */
	public OrCriteria(Collection<? extends ISearchCriteria> values) {
		super();
		if (values != null) {
			criterias = new ArrayList<ISearchCriteria>(values.size());
			criterias.addAll(values);
		} else {
			criterias = new ArrayList<ISearchCriteria>();
		}
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		// Reduction rules :
		// T or x -> T
		// F or x -> x
		// TODO x or x -> x
		// x or not(x) -> T
		// (x or (y or z)) -> (x or y or z)
		// TODO (x or (y and x)) -> x
		ArrayList<ISearchCriteria> reduced = new ArrayList<ISearchCriteria>();
		for(ISearchCriteria criteria: criterias) {
			if (criteria != null) {
				ISearchCriteria reduce = criteria.reduce(context);
				if ((reduce != null) && !ConstantCriteria.FALSE.equals(reduce)) {
					if (ConstantCriteria.TRUE.equals(reduce)) {
						return reduce;
					} else if (reduce instanceof OrCriteria) {
						reduced.addAll(((OrCriteria)reduce).criterias);
					} else {
						reduced.add(reduce);
					}
				}
			}
		}
		switch(reduced.size()) {
			case 0: return new ConstantCriteria(false);
			case 1: return reduced.get(0);
			case 2: 
				if (reduced.get(0) instanceof NotCriteria) {
					if (((NotCriteria)reduced.get(0)).getCriteria().equals(reduced.get(1))) {
						return ConstantCriteria.TRUE;
					}
				} else if (reduced.get(1) instanceof NotCriteria) {
					if (((NotCriteria)reduced.get(1)).getCriteria().equals(reduced.get(0))) {
						return ConstantCriteria.TRUE;
					}
				}
		}
		return new OrCriteria(reduced);
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		OrCriteria result = new OrCriteria();
		for(ISearchCriteria criteria: criterias) {
			if (criteria != null) {
				result.criterias.add((ISearchCriteria)criteria.clone());
			}
		}
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if ((obj instanceof OrCriteria) && (((OrCriteria)obj).criterias.size() == criterias.size())) {
			for(ISearchCriteria criteria: criterias) {
				if (criteria != null) {
					boolean stop = true;
					for(ISearchCriteria objcriteria:((OrCriteria)obj).criterias) {
						if (criteria.equals(objcriteria)) {
							stop = false;
							break;
						}
					}
					if (stop) {
						return false;
					}
				}
			}
			return true;
		}
		return false;
	}
	
	@Override
	public void add(ISearchCriteria criteria) {
		if (criteria != null) {
			criterias.add(criteria);
		}
	}

	@Override
	public ArrayList<ISearchCriteria> getCriterias() {
		return criterias;
	}

	@Override
	public boolean isEmpty() {
		return (criterias == null) || criterias.isEmpty();
	}
	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		for(ISearchCriteria c:criterias) {
			if ((c != null) && c.test(bean, currentUser)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder("("); //$NON-NLS-1$
		boolean first = true;
		for(ISearchCriteria ci:criterias) {
			if (first) {
				first = false;
			} else {
				sb.append(Messages.Criteria_Or);
			}
			sb.append(ci);
		}
		sb.append(')');
		return sb.toString();
	}
}
