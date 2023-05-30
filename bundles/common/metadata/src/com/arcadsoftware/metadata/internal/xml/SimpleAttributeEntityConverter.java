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

import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.rest.XStreamCompact;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;
import com.thoughtworks.xstream.mapper.Mapper;

public class SimpleAttributeEntityConverter implements Converter {

	protected static final String TAG_DOMAIN = "domain"; //$NON-NLS-1$
	protected static final String TAG_METADATA = "metadata"; //$NON-NLS-1$
	protected static final String TAG_VERSION = "version"; //$NON-NLS-1$
	protected static final String TAG_CODE = "code" ; //$NON-NLS-1$
	protected static final String TAG_NAME = "name" ; //$NON-NLS-1$
	
	public SimpleAttributeEntityConverter(Mapper mapper) {
		super();
	}
	
	public SimpleAttributeEntityConverter() {
		super();
	}

	public boolean canConvert(@SuppressWarnings("rawtypes") Class type) {
		return MetaDataAttribute.class.isAssignableFrom(type);
	}

	public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
		if (!(source instanceof MetaDataAttribute)) {
			return;
		}
		Element e = (Element) source;
		if ((e.getType() != null) && (e.getType().length() > 0)) {
			writer.addAttribute(XStreamCompact.TAG_TYPE, e.getType());
		}
		if ((e.getCode() != null) && (e.getCode().length() > 0)) {
			writer.addAttribute(TAG_CODE, e.getCode());
		}
		if ((e.getName() != null) && (e.getName().length() > 0)) {
			writer.addAttribute(TAG_NAME, e.getName());
		}		
	}

	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {	
		Element result;
		String s = reader.getNodeName();
		if (XStreamCompact.TAG_ATTRIBUTE.equals(s)) {
			result = new MetaDataAttribute((MetaDataEntity)null);
		} else { // Blindage
			return null;
		}
		s = reader.getAttribute(TAG_CODE);
		if ((s != null) && (s.length() > 0)) {
			result.setCode(s);
		}
		s = reader.getAttribute(TAG_NAME);
		if ((s != null) && (s.length() > 0)) {
			result.setName(s);
		}
		s = reader.getAttribute(XStreamCompact.TAG_TYPE);
		if ((s != null) && (s.length() > 0)) {
			result.setType(s);
		}
		
		return result;
	}

}
