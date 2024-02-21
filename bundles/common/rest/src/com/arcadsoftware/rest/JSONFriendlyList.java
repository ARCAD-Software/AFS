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
package com.arcadsoftware.rest;

import java.util.Collections;
import java.util.List;

/**
 * Class used to serialize generic lists in a friendly JSON format. It contains the items and the total count of the list.
 * Does not support deserializing.
 * 
 * @author ARCAD Software
 * @param <T> the type of Objects contained in the Friendly List
 */
public class JSONFriendlyList<T> {

	private final List<T> items;
	private final int count;
	
	/**
	 * Constructor for the FriendlyList
	 * @param list : List of elements to be added to the Friendly List
	 */
	public JSONFriendlyList(List<T> list) {
		super();
		if (list != null) {
			items = list;
			count = list.size();
		} else {
			items = Collections.emptyList();
			count = 0;
		}
	}

	/**
	 * Getter for the elements contained in the Friendly List
	 * @return a list containing all the elements in the Friendly List
	 */
	public List<T> getItems() {
		return items;
	}

	/**
	 * Getter for the number of elements contained in the Friendly List
	 * @return Integer containing the number of elements in the list
	 */
	public int getCount() {
		return count;
	}
	
	/**
	 * Generates the current object in JSON format as a String
	 * @return a string in JSON format containing the items and total count of the Friendly List
	 */
	public String toJson() {
		JsonStreamCompact x = new JsonStreamCompact(JSONFriendlyList.class.getClassLoader(), true, false, false);
		x.alias("list", JSONFriendlyList.class);
		return x.toXML(this);
	}
}
