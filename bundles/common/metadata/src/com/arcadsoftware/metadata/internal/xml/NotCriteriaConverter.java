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

import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.criteria.NotCriteria;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.ExtendedHierarchicalStreamWriterHelper;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;
import com.thoughtworks.xstream.mapper.Mapper;

public class NotCriteriaConverter implements Converter {

	private Mapper mapper;
	
	public NotCriteriaConverter(Mapper mapper) {
		super();
		this.mapper = mapper;
	}

	public void marshal(Object obj, HierarchicalStreamWriter writer, MarshallingContext context) {
		ISearchCriteria item = ((NotCriteria)obj).getCriteria();
		if (item != null) {
            String name = mapper.serializedClass(item.getClass());
            ExtendedHierarchicalStreamWriterHelper.startNode(writer, name, item.getClass());
            context.convertAnother(item);
            writer.endNode();
		}
	}

	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		NotCriteria result = new NotCriteria();
		if (reader.hasMoreChildren()) {
            reader.moveDown();
            try {
	            String classAttribute = reader.getAttribute(mapper.aliasForAttribute("class")); //$NON-NLS-1$
	            @SuppressWarnings("rawtypes")
				Class type;
	            if (classAttribute == null) {
	                type = mapper.realClass(reader.getNodeName());
	            } else {
	                type = mapper.realClass(classAttribute);
	            }
	            Object item = context.convertAnother(result, type);
	            if (item instanceof ISearchCriteria) {
	            	result.setCriteria((ISearchCriteria) item);
	            }
            } finally {
	            reader.moveUp();
            }
		}
		return result;
	}

	public boolean canConvert(@SuppressWarnings("rawtypes") Class clazz) {
		return NotCriteria.class.equals(clazz);
	}

}
