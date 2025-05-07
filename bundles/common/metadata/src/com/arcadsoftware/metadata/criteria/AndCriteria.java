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
 * Conjunction of any search criteria.
 */
public class AndCriteria extends AbstractSearchCriteria implements Cloneable, ISubCriteria {

	private final ArrayList<ISearchCriteria> criterias;
	
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
	 * @param criteriaToClone if this criteria is an AndCriteria it will be cloned, if not it is <strong>not</strong> cloned ! 
	 * @param criteria the criteria to add. It may be changed and will not be cloned.
	 * @return a conjunction of the to criteria.
	 * @throws CloneNotSupportedException
	 */
	public static ISearchCriteria and(ISearchCriteria criteriaToClone, ISearchCriteria criteria) {
		if ((criteriaToClone == null) || (ConstantCriteria.TRUE.equals(criteriaToClone))) {
			return criteria;
		}
		if ((criteria == null) || (ConstantCriteria.TRUE.equals(criteria))) {
			return criteriaToClone;
		}
		if (ConstantCriteria.FALSE.equals(criteriaToClone) || ConstantCriteria.FALSE.equals(criteria)) {
			return ConstantCriteria.FALSE;
		}
		if (criteriaToClone instanceof AndCriteria) {
			AndCriteria result = ((AndCriteria) criteriaToClone).clone();
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
				if (!((ConstantCriteria) criteriaToClone).isValue()) {
					return ConstantCriteria.FALSE;
				}// else ignore the criteria
			} else if (criteriaToClone != null) {
				try {
					((AndCriteria) criteria).criterias.add((ISearchCriteria) criteriaToClone.clone());
				} catch (CloneNotSupportedException e) {}
			}
			return (AndCriteria) criteria;
		}
		return new AndCriteria(criteriaToClone, criteria);
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		if ((criterias == null) || (criterias.size() == 0)) {
			return ConstantCriteria.TRUE;
		}
		// Reduction rules : (where "..." stand " and w [ and v [...]]"
		// T and x ... -> x ...
		// F and x ... -> F
		// (x and (y and z ...) ...) -> (x and y and z ...)
		// x and x ... -> x
		// x and not(x) ... -> F
		// TODO x and (y or x) ... -> x ...
		ArrayList<ISearchCriteria> reduced = new ArrayList<ISearchCriteria>();
		for (ISearchCriteria criteria: criterias) {
			if (criteria != null) {
				ISearchCriteria reduce = criteria.reduce(context);
				if ((reduce != null) && !ConstantCriteria.TRUE.equals(reduce)) {
					if (ConstantCriteria.FALSE.equals(reduce)) {
						return ConstantCriteria.FALSE;
					} 
					if (reduce instanceof AndCriteria) {
						reduced.addAll(((AndCriteria) reduce).criterias);
						// FIXME should test the reduction rules below to the whole list of sub-criteria...
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
								return ConstantCriteria.FALSE;
							}
							if ((c instanceof NotCriteria) && ((NotCriteria) c).getCriteria().equals(reduce)) {
								return ConstantCriteria.FALSE;
							}
						}
						if (toAdd) {
							reduced.add(reduce);
						}
					}
				}
			}
		}
		switch (reduced.size()) {
			case 0: return ConstantCriteria.TRUE;
			case 1: return reduced.get(0);
		}
		return new AndCriteria(reduced);
	}

	@Override
	public AndCriteria clone() {
		AndCriteria result = new AndCriteria();
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
		if (obj instanceof AndCriteria) {
			if ((criterias == null) || criterias.isEmpty()) {
				return (((AndCriteria) obj).criterias == null) || ((AndCriteria) obj).criterias.isEmpty(); 
			}
			if ((((AndCriteria) obj).criterias == null) || ((AndCriteria) obj).criterias.isEmpty()) {
				return false;
			}
			if (((AndCriteria) obj).criterias.size() == criterias.size()) {
				for (ISearchCriteria criteria: criterias) {
					if (criteria != null) {
						boolean stop = true;
						for (ISearchCriteria objcriteria: ((AndCriteria) obj).criterias) {
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
	public boolean isEmpty() {
		return (criterias == null) || criterias.isEmpty();
	}
	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		if (criterias != null) {
			for (ISearchCriteria ci: criterias) {
				if ((ci != null) && !ci.test(bean, currentUser)) {
					return false;
				}
			}
		}
		return true;
	}

	@Override
	public ArrayList<ISearchCriteria> getCriterias() {
		return criterias;
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
					sb.append(Messages.Criteria_And);
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