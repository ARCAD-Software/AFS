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
package com.arcadsoftware.metadata.internal.xml;

import java.util.ArrayList;

import com.arcadsoftware.metadata.criteria.IdInListCriteria;
import com.arcadsoftware.metadata.criteria.InListCriteria;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;

@SuppressWarnings("deprecation")
public class InListCriteriaConverter implements Converter {
	
	public boolean canConvert(@SuppressWarnings("rawtypes") Class type) {
		return InListCriteria.class.equals(type) || IdInListCriteria.class.equals(type);
	}

	public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
		if (((InListCriteria) source).getAttribute() != null) {
			writer.addAttribute("attribute", ((InListCriteria) source).getAttribute()); //$NON-NLS-1$
		}
		StringBuilder value = new StringBuilder();
		if (((InListCriteria) source).getIds() != null) {
			boolean first = true; 
			for(int i: ((InListCriteria) source).getIds()) {
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
		String att = reader.getAttribute("attribute"); //$NON-NLS-1$
		if ((value == null) || value.isEmpty()) {
			value = reader.getValue();
		}
		if ((value == null) || value.isEmpty()) {
			return new InListCriteria();
		}
		ArrayList<Integer> ids = new ArrayList<Integer>();
		for(String s: value.split(",")) { //$NON-NLS-1$
			try {
				ids.add(Integer.valueOf(s.trim()));
			} catch (NumberFormatException e) {}
		}
		return new InListCriteria(att, ids);
	}

}