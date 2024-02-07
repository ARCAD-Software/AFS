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

import java.util.ArrayList;
import java.util.Collection;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Conjunction of any search criteria.
 */
public class AndCriteria extends AbstractSearchCriteria implements Cloneable {

	private ArrayList<ISearchCriteria> criterias;
	
	public AndCriteria() {
		super();
		criterias = new ArrayList<ISearchCriteria>();
	}
	
	public AndCriteria(ISearchCriteria... val) {
		super();
		criterias = new ArrayList<ISearchCriteria>(val.length);
		for (ISearchCriteria v: val) {
			if (v != null) {
				criterias.add(v);
			}
		}
	}
	
	public AndCriteria(Collection<? extends ISearchCriteria> values) {
		super();
		if (values != null) {
			criterias = new ArrayList<ISearchCriteria>(values.size());
			criterias.addAll(values);
		} else {
			criterias = new ArrayList<ISearchCriteria>();
		}
	}

	/**
	 * Build an conjunction around the to criteria.
	 * 
	 * Deal with automatic reduction :
	 * - if one or the other are AndCriteria.
	 * - if one or the other are ConstantCriteria
	 * 
	 * @param criteriaToClone if this criteria is an AndCriteria it will be cloned. 
	 * @param criteria the criteria to add. It may be changed and will not be cloned.
	 * @return a conjunction of the to criteria.
	 * @throws CloneNotSupportedException
	 */
	public static ISearchCriteria and(ISearchCriteria criteriaToClone,ISearchCriteria criteria) throws CloneNotSupportedException {
		if (ConstantCriteria.TRUE.equals(criteriaToClone)) {
			return criteria;
		}
		if (ConstantCriteria.TRUE.equals(criteria)) {
			return criteriaToClone;
		}
		if (criteriaToClone instanceof AndCriteria) {
			AndCriteria result = (AndCriteria)criteriaToClone.clone();
			if (criteria instanceof AndCriteria) {
				result.criterias.addAll(((AndCriteria)criteria).criterias);
			} else if (criteria instanceof ConstantCriteria) {
				if (!((ConstantCriteria)criteria).isValue()) {
					return criteria;
				}// else ignore the criteria
			} else if (criteria != null) {
				result.criterias.add(criteria);
			}
			return result;
		} 
		if (criteria instanceof AndCriteria) {
			if (criteriaToClone instanceof ConstantCriteria) {
				if (!((ConstantCriteria)criteriaToClone).isValue()) {
					return ConstantCriteria.FALSE;
				}// else ignore the criteria
			} else if (criteriaToClone != null) {
				((AndCriteria)criteria).criterias.add((ISearchCriteria)criteriaToClone.clone());
			}
			return (AndCriteria)criteria;
		}
		AndCriteria result = new AndCriteria();
		if (criteriaToClone != null) {
			result.criterias.add(criteriaToClone); // We don't clone here (because we don't change the contain of the criteria.
		}
		if (criteria != null) {
			result.criterias.add(criteria);
		}
		return result;
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		if ((criterias == null) || (criterias.size() == 0)) {
			return ConstantCriteria.TRUE;
		}
		// Reduction rules :
		// T and x -> x
		// F and x -> F
		// TODO x and x -> x
		// x and not(x) -> F
		// (x and (y and z)) -> (x and y and z)
		// TODO (x and (y or x)) -> x
		ArrayList<ISearchCriteria> reduced = new ArrayList<ISearchCriteria>();
		for(ISearchCriteria criteria: criterias) {
			if (criteria != null) {
				ISearchCriteria reduce = criteria.reduce(context);
				if ((reduce != null) && !ConstantCriteria.TRUE.equals(reduce)) {
					if (ConstantCriteria.FALSE.equals(reduce)) {
						return reduce; // Return False.
					} else if (reduce instanceof AndCriteria) {
						reduced.addAll(((AndCriteria)reduce).criterias);
					} else {
						reduced.add(reduce);
					}
				}
			}
		}
		switch(reduced.size()) {
			case 0: return ConstantCriteria.TRUE;
			case 1: return reduced.get(0);
			case 2: 
				if (reduced.get(0) instanceof NotCriteria) {
					if (((NotCriteria)reduced.get(0)).getCriteria().equals(reduced.get(1))) {
						return ConstantCriteria.FALSE;
					}
				} else if (reduced.get(1) instanceof NotCriteria) {
					if (((NotCriteria)reduced.get(1)).getCriteria().equals(reduced.get(0))) {
						return ConstantCriteria.FALSE;
					}
				}
		}
		return new AndCriteria(reduced);
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		AndCriteria result = new AndCriteria();
		for(ISearchCriteria criteria: criterias) {
			if (criteria != null) {
				result.criterias.add((ISearchCriteria)criteria.clone());
			}
		}
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if ((obj instanceof AndCriteria) && (((AndCriteria)obj).criterias.size() == criterias.size())) {
			for(ISearchCriteria criteria: criterias) {
				if (criteria != null) {
					boolean stop = true;
					for(ISearchCriteria objcriteria:((AndCriteria)obj).criterias) {
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
	
	/**
	 * Add a new criteria to the conjunction.
	 * 
	 * @param criteria
	 */
	public void add(ISearchCriteria criteria) {
		criterias.add(criteria);
	}

	/**
	 * Return true if this conjunction is empty.
	 * @return
	 */
	public boolean isEmpty() {
		return (criterias == null) || criterias.isEmpty();
	}
	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		for(ISearchCriteria ci:criterias) {
			if ((ci != null) && !ci.test(bean, currentUser)) {
				return false;
			}
		}
		return true;
	}

	public ArrayList<ISearchCriteria> getCriterias() {
		return criterias;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder("("); //$NON-NLS-$ //$NON-NLS-1$
		boolean first = true;
		for(ISearchCriteria ci:criterias) {
			if (first) {
				first = false;
			} else {
				sb.append(Messages.Criteria_And);
			}
			sb.append(ci);
		}
		sb.append(')');
		return sb.toString();
	}

}