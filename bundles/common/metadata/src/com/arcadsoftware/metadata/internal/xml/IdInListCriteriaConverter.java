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
package com.arcadsoftware.metadata.internal.xml;

import java.util.ArrayList;

import com.arcadsoftware.metadata.criteria.IdInListCriteria;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;

public class IdInListCriteriaConverter implements Converter {
	
	public boolean canConvert(@SuppressWarnings("rawtypes") Class type) {
		return IdInListCriteria.class.equals(type);
	}

	public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
		StringBuilder value = new StringBuilder();
		if (((IdInListCriteria) source).getIds() != null) {
			boolean first = true; 
			for(int i: ((IdInListCriteria) source).getIds()) {
				if (first) {
					first = false;
				} else {
					value.append(',');
				}
				value.append(i);
			}
		}
		writer.addAttribute("ids", value.toString()); //$NON-NLS-1$
	}
	
	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		String value = reader.getAttribute("ids"); //$NON-NLS-1$
		if ((value == null) || value.isEmpty()) {
			return new IdInListCriteria();
		}
		ArrayList<Integer> ids = new ArrayList<Integer>();
		for(String s: value.split(",")) { //$NON-NLS-1$
			try {
				ids.add(Integer.valueOf(s));
			} catch (NumberFormatException e) {}
		}
		return new IdInListCriteria(ids);
	}

}