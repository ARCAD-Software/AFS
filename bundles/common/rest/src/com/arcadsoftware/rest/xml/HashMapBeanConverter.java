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
package com.arcadsoftware.rest.xml;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;

/**
 * String representation of a Map<?,?> Object. Work for any Map with key that
 * can be converted as string.
 * 
 * <p>
 * Values are converted according to the classloader visibility.
 */
public class HashMapBeanConverter implements Converter {

	private final ClassLoader classloader;
	
	public HashMapBeanConverter() {
		super();
		classloader = HashMapBeanConverter.class.getClassLoader();
	}
	
	public HashMapBeanConverter(ClassLoader classloader) {
		super();
		this.classloader = classloader;
	}
	
	public boolean canConvert(@SuppressWarnings("rawtypes") Class type) {
		return Map.class.isAssignableFrom(type);
	}

	public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
		if (source instanceof Map) {
			Map<?,?> m = (Map<?,?>) source;
			for (Entry<?,?> entry : m.entrySet()) {
				Object value = entry.getValue();
				if (value != null) {
					String key = entry.getKey().toString();
					writer.startNode(key);
					if ((value instanceof String) || (value instanceof Boolean) || (value instanceof Integer) || (value instanceof Date)) {
						writer.setValue(value.toString());
					} else {
						context.convertAnother(value);
					}
					writer.endNode();
				}
			}
		}
	}

	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		HashMap<String, Object> map = new HashMap<String, Object>();
		while (reader.hasMoreChildren()) {
			reader.moveDown();
			if ((reader.getAttributeCount() > 0) || reader.hasMoreChildren()) {
				@SuppressWarnings("rawtypes")
				Class type = Object.class;
				String cl = reader.getAttribute("class"); //$NON-NLS-1$
				if (cl != null) {
					try {
						type = classloader.loadClass(cl);
					} catch (ClassNotFoundException e) {}
				}
				map.put(reader.getNodeName(), context.convertAnother(map, type));
			} else {
				map.put(reader.getNodeName(), reader.getValue());
			}
			reader.moveUp();
		}
		return map;
	}

}
