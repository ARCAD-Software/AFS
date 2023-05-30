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

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.UpdateMetaDataEntity;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.metadata.xml.XmlMetaDataStream;
import com.arcadsoftware.osgi.ISODateFormater;
import com.arcadsoftware.rest.XStreamCompact;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;

public class SimpleMetaDataEntityConverter implements Converter {

	protected static final String TAG_DOMAIN = "domain"; //$NON-NLS-1$
	protected static final String TAG_VERSION = "version"; //$NON-NLS-1$
	protected static final String TAG_NAME = "name" ; //$NON-NLS-1$

	
	public SimpleMetaDataEntityConverter() {
		super();
	}

	public boolean canConvert(@SuppressWarnings("rawtypes") Class type) {
		return MetaDataEntity.class.isAssignableFrom(type);
	}

	public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
		if (!(source instanceof MetaDataEntity)) {
			return;
		}
		MetaDataEntity e = (MetaDataEntity) source;
		if (e.getDomain() != null) {
			writer.addAttribute(TAG_DOMAIN, e.getDomain());
		}
		writer.addAttribute(TAG_VERSION, Integer.toString(e.getVersion()));
		if (e.getDate() != null) {
			writer.addAttribute(XStreamCompact.TAG_DATE, ISODateFormater.toString(e.getDate()));
		}
		if (e.getType() != null) {
			writer.addAttribute(XStreamCompact.TAG_TYPE, e.getType());
		}
		if ((e.getName() != null) && (e.getName().length() > 0)) {
			writer.addAttribute(TAG_NAME, e.getName());
		}
		List<MetaDataAttribute> attributes = new ArrayList<MetaDataAttribute>(e.getAttributes().values());
		Collections.sort(attributes, new Comparator<MetaDataAttribute>() {
			@Override
			public int compare(MetaDataAttribute o1, MetaDataAttribute o2) {
				return o1.getCode().compareTo(o2.getCode());
			}
		});
		for (MetaDataAttribute att: attributes) {
			writer.startNode(XStreamCompact.TAG_ATTRIBUTE);
			context.convertAnother(att, new SimpleAttributeEntityConverter());
			writer.endNode();
		}
	}

	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		String s = reader.getAttribute(TAG_VERSION);
		int version = 0;
		if (s != null) {
			try {
				version = Integer.parseInt(s);
			} catch (NumberFormatException e) {
				Activator.getInstance().debug(e);
			}
		}
		s = reader.getAttribute(XStreamCompact.TAG_TYPE);
		MetaDataEntity result;
		if (XmlMetaDataStream.TAG_UPDATEENTITY.equals(reader.getNodeName())) {
			result = new UpdateMetaDataEntity(s,version);
		} else {
			result = new MetaDataEntity(s, version);
		}
		s = reader.getAttribute(TAG_NAME);
		if (s != null) {
			result.setName(s);
		}
		s = reader.getAttribute(TAG_DOMAIN);
		if (s != null) {
			result.setDomain(s);
		}
		s = reader.getAttribute(XStreamCompact.TAG_DATE);
		if (ISODateFormater.mayIsoDate(s)) {
			try {
				result.setDate(ISODateFormater.toDate(s));
			} catch (ParseException e) {}
		}
		while (reader.hasMoreChildren()) {
			reader.moveDown();
			s = reader.getNodeName();
			if (XStreamCompact.TAG_ATTRIBUTE.equalsIgnoreCase(s)) {
				Object o = context.convertAnother(result, MetaDataAttribute.class);
				if (o instanceof MetaDataAttribute) {
					((MetaDataAttribute)o).setParent(result);
					result.getAttributes().put(((MetaDataAttribute)o).getCode(),(MetaDataAttribute)o);
				}
			}
			reader.moveUp();
		}
		return result;
	}
}
