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
package com.arcadsoftware.beanmap;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;

/**
 * This class is an HashMap of BeanMap that allow to use a secondary map key.
 */
public class BeanMapTable implements Serializable {

	private static final long serialVersionUID = 2871759286699301777L;

	private final HashMap<Integer, IBeanMap> map;
	private HashMap<Object, IBeanMap> st;
	private String secKeyCode;
	
    /**
     * Constructs an empty <tt>BeanMapTable</tt> with the default initial capacity
     * (16) and the default load factor (0.75).
     */
	public BeanMapTable() {
		super();
		map = new HashMap<Integer, IBeanMap>();
	}
	
	/**
     * Constructs an empty <tt>BeanMapTable</tt> with the specified initial capacity
     * and the default load factor (0.75).
	 * 
	 * @param initialCapacity the initial capacity. 
	 */
	public BeanMapTable(int initialCapacity) {
		super();
		map = new HashMap<Integer, IBeanMap>(initialCapacity);
	}

	/**
     * Constructs an empty <tt>BeanMapTable</tt> with the default initial capacity
     * (16) and the default load factor (0.75).
	 * 
	 * @param secKeyCode Define the secondary Key mapping code.
	 */
	public BeanMapTable(String secKeyCode) {
		this();
		this.secKeyCode = secKeyCode;
		createSecTable(16);
	}
	
	/**
     * Constructs an empty <tt>BeanMapTable</tt> with the specified initial capacity
     * and the default load factor (0.75).
	 * 
	 * @param secKeyCode Define the secondary Key mapping code.
	 * @param initialCapacity the initial capacity. 
	 */
	public BeanMapTable(String secKeyCode,int initialCapacity) {
		this(initialCapacity);
		this.secKeyCode = secKeyCode;
		createSecTable(initialCapacity);
	}

	private void createSecTable(int initialCapacity) {
		st = new HashMap<Object, IBeanMap>(initialCapacity);
	}
	
	/**
	 * @param id
	 * @return the corresponding BeanMap.
	 */
	public IBeanMap get(int id) {
		return map.get(id);
	}
	
	/**
	 * Return the corresponding BeanMap from the table or, if there is no BeanMap with same ID, the 
	 * <code>bean</code> parameter.
	 * 
	 * @param bean
	 * @return never return null if the parameter is not null.
	 */
	public IBeanMap get(IBeanMap bean) {
		IBeanMap result = map.get(bean.getId());
		if (result != null) {
			return result;
		}
		return bean;
	}
	
	/**
	 * Get a BeanMap using the secondary key.
	 * 
	 * @param value
	 * @return
	 */
	public IBeanMap getSec(Object value) {
		return st.get(value);
	}
	
	/**
	 * @param bean
	 * @return true if the table contain this BeanMap.
	 */
	public boolean contain(IBeanMap bean) {
		return map.get(bean.getId()) != null;
	}

	/**
	 * 
	 * @param id
	 * @return true if the table contain this BeanMap.
	 */
	public boolean contain(int id) {
		return map.get(id) != null;
	}

	/**
	 * @param value
	 * @return true if the table contain a BeanMap that correspond to the given value.
	 */
	public boolean containsec(Object value) {
		return (st != null) && (st.get(value) != null);
	}
	
	/**
	 * Add or replace an existing BeanMap into the Table.
	 * 
	 * @param bean
	 * @return the replaced BeanMap if any.
	 */
	public IBeanMap put(IBeanMap bean) {
		IBeanMap result = map.put(bean.getId(), bean);
		if (st != null) {
			if (result != null) {
				st.remove(result.get(secKeyCode));
			}
			Object o = bean.get(secKeyCode);
			if (o != null) {
				st.put(o,bean);
			}
		}
		return result;
	}
	
	/**
	 * Remove a BeanMap from the table.
	 * 
	 * @param bean the BeanMap to remove.
	 * @return the effectively removed BeanMap if any.
	 */
	public IBeanMap remove(IBeanMap bean) {
		IBeanMap result = map.remove(bean.getId());
		if (st != null) {
			if (result != null) {
				st.remove(result.get(secKeyCode));
			}
			st.remove(bean.get(secKeyCode));
		}
		return result;
	}

	/**
	 * Remove a BeanMap from the table.
	 * 
	 * @param id the Id of the BeanMap to be removed.
	 * @return the removed BeanMap if any.
	 */
	public IBeanMap remove(int id) {
		IBeanMap result = map.remove(id);
		if ((st != null) && (result != null)) {
			st.remove(result.get(secKeyCode));
		}
		return result;
	}
	
	/**
	 * Add all the BeanMap of the collection into the table.
	 * 
	 * @param collection
	 */
	public void addAll(Collection<IBeanMap> collection) {
		for (IBeanMap bean:collection) {
			put(bean);
		}
	}

	/**
	 * Removes all mappings from this table.
	 */
	public void clear() {
		map.clear();
		if (st != null) {
			st.clear();
		}
	}
}
