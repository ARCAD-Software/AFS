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
package com.arcadsoftware.beanmap;

import java.util.Comparator;
import java.util.List;

/**
 * Compare two BeanMap.
 * 
 * <p>
 * This class may use a list of attributes keys that will tested in the given order.
 * 
 * <p>
 * If the list of keys is empty or if, and only if, all the attributes values are equals then the type an the ID will be tested.
 *
 */
public class BeanMapComparator implements Comparator<BeanMap> {

	private final String[] keys;
	private final boolean[] reversed;
	
	public BeanMapComparator(String key) {
		super();
		keys = key.split(" "); //$NON-NLS-1$
		reversed = getOrders(keys);
	}
	
	public BeanMapComparator(String... key) {
		super();
		keys = key;
		reversed = getOrders(keys);
	}
	
	public BeanMapComparator(List<String> key) {
		super();
		keys = key.toArray(new String[key.size()]);
		reversed = getOrders(keys);
	}

	private boolean[] getOrders(String[] keys) {
		boolean [] orders = new boolean[keys.length];
		for (int i = 0; i < keys.length; i++) {
			String k = keys[i];
			if (k == null) {
				keys[i] = ""; //$NON-NLS-1$
			} else if (!k.isEmpty() && (k.charAt(0) == '!')) {
				orders[i] = true;
				keys[i] = k.substring(1);
			}
		}
		return orders;
	}

	@Override
	public int compare(BeanMap o1, BeanMap o2) {
		for (int i = 0; i < keys.length; i++) {
			int c = compareTo(o1, o2, keys[i]);
			if (c != 0) {
				if (reversed[i]) {
					return -c;
				}
				return c;
			}
		}
		// Compare beanmap type and id...
		return o1.compareTo(o2);
	}

	/**
	 * Compare the value on the key from data1 to the one into data2.
	 * 
	 * @param data1 a non null BeanMap.
	 * @param data2 a non null BeanMap.
	 * @param key the code to compare.
	 * @return a negative integer, zero, or a positive integer as the value is data1 is less than, equal to, or greater than the value in data2.
	 */
	protected int compareTo(BeanMap data1, BeanMap data2, String key) {
		return data1.compareTo(data2, key);
	}

}
