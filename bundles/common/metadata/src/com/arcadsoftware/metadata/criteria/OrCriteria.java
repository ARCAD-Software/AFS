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
	 * Create an empty disjunction of criteria.
	 */
	public OrCriteria() {
		super();
		criterias = new ArrayList<ISearchCriteria>();
	}

	/**
	 * Disjunction of the given criteria.
	 * 
	 * @param val
	 */
	public OrCriteria(ISearchCriteria... val) {
		super();
		criterias = new ArrayList<ISearchCriteria>(val.length);
		for (ISearchCriteria v: val) {
			if (v != null) {
				criterias.add(v);
			}
		}
	}

	/**
	 * Disjunction of the given criteria.
	 * 
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
		if ((criterias == null) || (criterias.size() == 0)) {
			return ConstantCriteria.TRUE;
		}
		// Reduction rules : (where "..." stand " and w [ and v [...]]"
		// T or x ... -> T
		// F or x ... -> x ...
		// x or x ... -> x ...
		// x or not(x) ... -> T
		// (x or (y or z ...) ...) -> (x or y or z ...)
		// TODO (x or (y and x) ...) -> x ...
		ArrayList<ISearchCriteria> reduced = new ArrayList<ISearchCriteria>();
		for (ISearchCriteria criteria: criterias) {
			if (criteria != null) {
				ISearchCriteria reduce = criteria.reduce(context);
				if ((reduce != null) && !ConstantCriteria.FALSE.equals(reduce)) {
					if (ConstantCriteria.TRUE.equals(reduce)) {
						return ConstantCriteria.TRUE;
					} else if (reduce instanceof OrCriteria) {
						reduced.addAll(((OrCriteria) reduce).criterias);
					} else {
						boolean toAdd = true;
						ISearchCriteria notreduce = null;
						if (reduce instanceof NotCriteria) {
							notreduce = ((NotCriteria) reduce).getCriteria();
						}
						for (ISearchCriteria c: reduced) {
							if (c.equals(reduce)) {
								toAdd = false;
								break;
							}
							if ((notreduce != null) && c.equals(notreduce)) {
								return ConstantCriteria.TRUE;
							}
							if ((c instanceof NotCriteria) && ((NotCriteria) c).getCriteria().equals(reduce)) {
								return ConstantCriteria.TRUE;
							}
						}
						if (toAdd) {
							reduced.add(reduce);
						}
					}
				}
			}
		}
		switch(reduced.size()) {
			case 0: return ConstantCriteria.FALSE;
			case 1: return reduced.get(0);
		}
		return new OrCriteria(reduced);
	}

	@Override
	public OrCriteria clone() {
		OrCriteria result = new OrCriteria();
		if (criterias != null) {
			for (ISearchCriteria criteria: criterias) {
				if (criteria != null) {
					try {
						result.criterias.add((ISearchCriteria) criteria.clone());
					} catch (CloneNotSupportedException e) {}
				}
			}
		}
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof OrCriteria) { 
			if ((criterias == null) || criterias.isEmpty()) {
				return (((OrCriteria) obj).criterias == null) || ((OrCriteria) obj).criterias.isEmpty(); 
			}
			if ((((OrCriteria) obj).criterias == null) || ((OrCriteria) obj).criterias.isEmpty()) {
				return false;
			}
			if (((OrCriteria)obj).criterias.size() == criterias.size()) {
				for (ISearchCriteria criteria: criterias) {
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
		}
		return false;
	}
	
	@Override
	public void add(ISearchCriteria criteria) {
		if ((criteria != null) && (criterias != null)) {
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
		if (criterias != null) {
			for (ISearchCriteria ci: criterias) {
				if (first) {
					first = false;
				} else {
					sb.append(Messages.Criteria_Or);
				}
				sb.append(ci);
			}
		} else {
			sb.append('!');
		}
		sb.append(')');
		return sb.toString();
	}
}
