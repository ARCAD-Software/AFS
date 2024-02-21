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
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Negate the criteria result.
 */
public class NotCriteria extends AbstractSearchCriteria implements Cloneable {

	private ISearchCriteria criteria;

	public NotCriteria() {
		super();
	}
	
	public NotCriteria(ISearchCriteria criteria) {
		this();
		this.criteria = criteria;
	}
	
	public ISearchCriteria getCriteria() {
		return criteria;
	}
	
	public void setCriteria(ISearchCriteria criteria) {
		this.criteria = criteria;
	}
	
	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		// Reduction rules :
		// not(not()) -> T
		// not(not(x)) -> x
		if (criteria instanceof NotCriteria) {
			ISearchCriteria result = ((NotCriteria) criteria).getCriteria();
			if (result == null) {
				return ConstantCriteria.TRUE; 
			}
			result = result.reduce(context);
			if (result == null) { // Blindage
				return ConstantCriteria.TRUE;
			}
			return result; 
		}
		// Reduction rules :
		// not() -> F
		// not(T) -> F
		// not(F) -> T
		if (criteria == null) {
			return ConstantCriteria.FALSE; 
		}
		ISearchCriteria result = criteria.reduce(context);
		if ((result == null) || ConstantCriteria.TRUE.equals(result)) {
				return ConstantCriteria.FALSE; 
			}
		if (ConstantCriteria.FALSE.equals(result)) {
			return ConstantCriteria.TRUE;
		}
		return new NotCriteria(result);
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		if (criteria == null) {
			return new NotCriteria();
		}
		return new NotCriteria((ISearchCriteria) criteria.clone());
	}

	@Override
	public boolean equals(Object obj) {
		// Rules:
		// Not() = Not()
		// Not() = FALSE
		// Not() = Not(True);
		// Not(X) = Not(X)
		if (criteria == null) {
			return (ConstantCriteria.FALSE.equals(obj)) || //
				(obj instanceof NotCriteria) && ((((NotCriteria)obj).getCriteria() == null) || //
						ConstantCriteria.TRUE.equals(((NotCriteria)obj).getCriteria()));
		}
		return (obj instanceof NotCriteria) && criteria.equals(((NotCriteria)obj).getCriteria());
	}

	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		return (criteria != null) && !criteria.test(bean, currentUser);
	}

	@Override
	public String toString() {
		if (criteria == null) {
			return Messages.Criteria_False;
		}
		StringBuilder sb = new StringBuilder(Messages.Criteria_Not);
		sb.append(criteria.toString());
		sb.append(')');
		return sb.toString();
	}
}
