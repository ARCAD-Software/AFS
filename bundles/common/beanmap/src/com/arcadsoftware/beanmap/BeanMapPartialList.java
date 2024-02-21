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
package com.arcadsoftware.beanmap;

import java.util.Collection;

/**
 * A BeanMapList used to exchange partial result set.
 * 
 * <p>
 * This list is a kind of a sub-list. It work just like a normal list plus some
 * information to retreive the position of this sub list into the encmpassing list. 
 * 
 */
public class BeanMapPartialList extends BeanMapList {

	private static final long serialVersionUID = 7288767312461587015L;

	private int total = 0;
	private int rank = 0;
	
	/**
	 * Construct an empty list with an initial capacity of 12.
	 */
	public BeanMapPartialList() {
		super();
	}

	/**
	 * Construct an empty list with an initial capacity of 12.
	 * 
	 * @param firstValue
	 */
	public BeanMapPartialList(BeanMap firstValue) {
		super(firstValue);
		setTotal(1);
	}

	/**
	 * Construct an empty list with the specified capacity.
	 * 
	 * @param initialCapacity
	 */
	public BeanMapPartialList(int initialCapacity) {
		super(initialCapacity);
	}

	/**
	 * Construct an empty list with the specified capacity and parameters.
	 * 
	 * @param rank the actual rank of the list in the global result list.
	 * @param size the size of this list.
	 * @param total the global result list size.
	 */
	public BeanMapPartialList(int rank, int size, int total) {
		super(size);
		this.total = total;
		this.rank = rank;
	}

	/**
	 * Constructs a list containing the elements of the specified collection, in the order they are returned by the
	 * collection's iterator. The <tt>ArrayList</tt> instance has an initial capacity of 110% the size of the specified
	 * collection.
	 * 
	 * @param c
	 *            the collection whose elements are to be placed into this list.
	 * @throws NullPointerException
	 *             if the specified collection is null.
	 */
	public BeanMapPartialList(Collection<? extends BeanMap> c) {
		super(c);
	}

	/**
	 * Constructs a list containing the elements of the specified collection, in the order they are returned by the
	 * collection's iterator. The <tt>ArrayList</tt> instance has an initial capacity of 110% the size of the specified
	 * collection.
	 * 
	 * @param c
	 *            the collection whose some elements are to be placed into this list.
	 * @param firstPosition the first element position to store in this list.
	 * @param lastPosition the latest element position to store in the list (<b>included</b>), -1 stand for the end of the encompassing list.
	 * @throws NullPointerException
	 *             if the specified collection is null.
	 */
	public BeanMapPartialList(Collection<? extends BeanMap> c, int firstPosition, int lastPosition) {
		super();
		if (c != null) {
			total = c.size();
			if (firstPosition >= 0) {
				rank = firstPosition;
				if ((lastPosition == -1) || (lastPosition >= total)) {
					int i = 0;
					for (BeanMap b: c) {
						if (firstPosition <= (i++)) {
							add(b);
						}
					}
				} else if (firstPosition <= lastPosition) {
					int i = 0;
					for (BeanMap b: c) {
						if (firstPosition <= i) {
							add(b);
						}
						i++;
						if (lastPosition < i) {
							break;
						}
					}
				}
			}
		}
	}

	/**
	 * Define the total count of element that should have been included if the list.
	 * @param total the new total list elements
	 */
	public void setTotal(int total) {
		this.total = total;
	}

	/**
	 * Get the total count of element that should have been included if the list.
	 * 
	 * <p>
	 * Note that this value should always be greater than <code>size()</code>.
	 * 
	 * @return the total
	 */
	public int getTotal() {
		return total;
	}

	/**
	 * Increment the total count of elements.
	 * @param increment
	 */
	public void incrementTotal(int increment) {
		total += increment;
	}

	/**
	 * Define the position of the first element of this list into the encompassing list.
	 * 
	 * @param rank the rank to set
	 */
	public void setRank(int rank) {
		this.rank = rank;
	}

	/**
	 * Get the rank, i.e. the position of the first element of this list into the encompassing list.
	 * 
	 * @return the rank
	 */
	public int getRank() {
		return rank;
	}

	/**
	 * Decrement the total count of elements.
	 * 
	 * @param increment
	 */
	public void decrementTotal(int increment) {
		total -= increment;
	}
}
