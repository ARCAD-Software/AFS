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
package com.arcadsoftware.rest.console;

import java.util.ArrayList;

public class Category implements Comparable<Category> {

	private String label;
	private ArrayList<SectionId> list = new ArrayList<SectionId>();
	private String[] keywords;
	
	public String getLabel() {
		return label;
	}
	
	public void setLabel(String label) {
		this.label = label;
	}
	
	public ArrayList<SectionId> getList() {
		return list;
	}
	
	public void setList(ArrayList<SectionId> list) {
		this.list = list;
	}
	
	public int compareTo(Category o) {
		return label.compareTo(o.label);
	}

	public String[] getKeywords() {
		return keywords;
	}

	public void setKeywords(String[] keywords) {
		this.keywords = keywords;
	}
}
