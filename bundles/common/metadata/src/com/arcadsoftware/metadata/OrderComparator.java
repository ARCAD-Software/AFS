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
package com.arcadsoftware.metadata;

import java.util.Comparator;
import java.util.List;

import com.arcadsoftware.beanmap.BeanMap;

/**
 * This class can be used to sort a BeanMapList according to a list of references.
 * 
 * <p>
 * The specified order references are assumed to be contained into the BeanMap to compare.
 * 
 * <p>
 * Usage:<br/>
 * <code>Collections.sort(aBeanMapList,new OrderComparator(orders));</code>
 * 
 */
public class OrderComparator implements Comparator<BeanMap> {

	private List<ReferenceLine> orders;
	
	public OrderComparator() {
		super();
	}
	
	public OrderComparator(List<ReferenceLine> orders) {
		super();
		this.orders = orders;
	}

	
	public int compare(BeanMap bean1, BeanMap bean2) {
		if (orders == null) {
			return bean1.compareTo(bean2);
		}
		for(ReferenceLine ref:orders) {
			int i = bean1.compareTo(bean2,ref.getCode());
			if (i != 0) {
				return i;
			}
		}
		return 0;
	}

}
