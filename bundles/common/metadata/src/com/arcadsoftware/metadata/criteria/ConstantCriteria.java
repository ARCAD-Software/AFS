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

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * The Constant Search Criteria is always applicable and has a constant boolean value.
 * 
 * Use the static constant TRUE and FALSE to compare any criteria to a constant result.
 */
public class ConstantCriteria extends AbstractSearchCriteria implements Cloneable {

	/**
	 * DO NOT USE THIS CONSTANT AS REDUCTION RESULTS.
	 */
	public static final ConstantCriteria TRUE = new ConstantCriteria(true);

	/**
	 * DO NOT USE THIS CONSTANT AS REDUCTION RESULTS.
	 */
	public static final ConstantCriteria FALSE = new ConstantCriteria(false);
	
	private boolean value;
	
	/**
	 * Build a constant search criteria without SQL generator.
	 * (Do not use this constructor for reduction).
	 * 
	 * @param value
	 */
	public ConstantCriteria(boolean value) {
		this();
		this.value = value;
	}
	
	/**
	 * Standard constructor.
	 * (Do not use this constructor for reduction).
	 * 
	 */
	public ConstantCriteria() {
		super();
	}
	
	@Override
	public ConstantCriteria clone() {
		return new ConstantCriteria(value);
	}

	@Override
	public boolean equals(Object obj) {
		// Symmetry rules...
		// FALSE = Not()
		// True = Not(Not());
		if (obj instanceof ConstantCriteria) {
			return ((ConstantCriteria)obj).isValue() == value;
		}
		if (obj instanceof NotCriteria) {
			if (((NotCriteria)obj).getCriteria() == null) {
				return !value;
			}
			if (((NotCriteria)obj).getCriteria() instanceof ConstantCriteria) {
				return ((ConstantCriteria)((NotCriteria)obj).getCriteria()).value != value;
			}
		}
		return false;
	}

	/**
	 * @return the boolean value of this constant.
	 */
	public boolean isValue() {
		return value;
	}
	
	public void setValue(boolean value) {
		this.value = value;
	}

	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		return value;
	}

	@Override
	public String toString() {
		if (value) {
			return Messages.Criteria_True;
		}
		return Messages.Criteria_False;
	}

}