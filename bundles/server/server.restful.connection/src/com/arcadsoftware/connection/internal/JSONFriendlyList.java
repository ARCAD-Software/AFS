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
package com.arcadsoftware.connection.internal;

import java.util.ArrayList;

public class JSONFriendlyList {

	private final ArrayList<String> items;
	private final int count;
	
	public JSONFriendlyList(ArrayList<String> list) {
		super();
		items = list;
		if (list != null) {
			count = list.size();
		} else {
			count = 0;
		}
	}

	public ArrayList<String> getItems() {
		return items;
	}

	public int getCount() {
		return count;
	}

}
