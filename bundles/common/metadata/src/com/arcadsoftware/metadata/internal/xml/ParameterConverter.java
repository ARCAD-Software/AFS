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

import java.util.HashMap;
import java.util.Map.Entry;

import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;

public class ParameterConverter implements Converter {

	public ParameterConverter() {
		super();
	}

	@SuppressWarnings("rawtypes")
	public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
		for(Object e: ((HashMap)source).entrySet()) {
			String key = ((Entry)e).getKey().toString();
			Object value = ((Entry)e).getValue();
			writer.startNode("param"); //$NON-NLS-1$
			writer.addAttribute("code", key); //$NON-NLS-1$
			if (value != null) {
				writer.setValue(value.toString());
			}
			writer.endNode();
		}
	}

	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		// not implemented...
		return null;
	}

	public boolean canConvert(@SuppressWarnings("rawtypes") Class type) {
		return HashMap.class.equals(type);
	}

}
