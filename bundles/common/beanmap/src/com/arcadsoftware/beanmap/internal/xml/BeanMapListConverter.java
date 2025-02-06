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
package com.arcadsoftware.beanmap.internal.xml;

import java.text.ParseException;
import java.util.Date;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapPartialList;
import com.arcadsoftware.osgi.ISODateFormater;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.ExtendedHierarchicalStreamWriter;
import com.thoughtworks.xstream.io.ExtendedHierarchicalStreamWriterHelper;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;
import com.thoughtworks.xstream.mapper.Mapper;

@SuppressWarnings("deprecation")
public class BeanMapListConverter implements Converter {

	public static final String TAG_RANK = "rank"; //$NON-NLS-1$
	public static final String TAG_DATE = "date"; //$NON-NLS-1$
	public static final String TAG_TOTAL = "total"; //$NON-NLS-1$
	public static final String TAG_COUNT = "count"; //$NON-NLS-1$
	private static final String TAG_ITEMS = "items"; //$NON-NLS-1$

	private final Mapper mapper;
	private final boolean addItems;
	private final BeanMapConverter beanMapConverter;
	private final boolean useAttributes;

	public BeanMapListConverter(Mapper mapper) {
		super();
		this.mapper = mapper;
		addItems = false;
		useAttributes = true;
		beanMapConverter = null;
	}

	/**
	 * 
	 * @param mapper
	 * @param addItems Used for JSon serialization to put all BeanMaps into an "items" key.
	 */
	public BeanMapListConverter(BeanMapConverter beanMapConverter, Mapper mapper, boolean addItems, boolean useAttributes) {
		super();
		this.mapper = mapper;
		this.addItems = addItems;
		this.beanMapConverter = beanMapConverter;
		this.useAttributes = useAttributes;
	}

	public boolean canConvert(@SuppressWarnings("rawtypes") Class type) {
		return BeanMapList.class.equals(type) || BeanMapPartialList.class.equals(type);
	}

	public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
		if (source instanceof BeanMapPartialList) {
			if (useAttributes) {
				writer.addAttribute(TAG_RANK, String.valueOf(((BeanMapPartialList) source).getRank()));
				writer.addAttribute(TAG_TOTAL, String.valueOf(((BeanMapPartialList) source).getTotal()));
			} else {
				((ExtendedHierarchicalStreamWriter) writer).startNode(TAG_RANK, Integer.class);
				context.convertAnother(((BeanMapPartialList) source).getRank());
				writer.endNode();
				((ExtendedHierarchicalStreamWriter) writer).startNode(TAG_TOTAL, Integer.class);
				context.convertAnother(((BeanMapPartialList) source).getTotal());
				writer.endNode();
			}
		}
		if (source instanceof BeanMapList) {
			if (useAttributes) {
				writer.addAttribute(TAG_COUNT, String.valueOf(((BeanMapList) source).size()));
			} else {
				((ExtendedHierarchicalStreamWriter) writer).startNode(TAG_COUNT, Integer.class);
				context.convertAnother(((BeanMapList) source).size());
				writer.endNode();
			}
			Date date = ((BeanMapList) source).getDate();
			if (date != null) {
				if (useAttributes) {
					writer.addAttribute(TAG_DATE, ISODateFormater.toString(date));
				} else {
					((ExtendedHierarchicalStreamWriter) writer).startNode(TAG_DATE, String.class);
					context.convertAnother(ISODateFormater.toString(date));
					writer.endNode();
				}
			}
			if (addItems && !((BeanMapList) source).isEmpty()) {
				ExtendedHierarchicalStreamWriterHelper.startNode(writer, TAG_ITEMS, BeanMap[].class);
				if (beanMapConverter != null) {
					beanMapConverter.setAddNulls(true);
				}
			}
			for (BeanMap item : (BeanMapList) source) {
				if (item == null) {
					String name = mapper.serializedClass(null);
					writer.startNode(name);
					writer.endNode();
				} else {
					String name = item.getType();
					if (name == null) {
						name = mapper.serializedClass(item.getClass());
					}
					ExtendedHierarchicalStreamWriterHelper.startNode(writer, name.replace(BeanMapList.SLASH_TYPE, BeanMapList.SLASH_TAG), item.getClass());
					context.convertAnother(item);
					writer.endNode();
				}
			}
			if (addItems && !((BeanMapList) source).isEmpty()) {
				writer.endNode();
			}
		}
	}

	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		BeanMapList result;
		String count = reader.getAttribute(TAG_COUNT);
		int capacity = 12;
		if (count != null) {
			try {
				capacity = Integer.parseInt(count);
			} catch (NumberFormatException e) {}
		}
		String total = reader.getAttribute(TAG_TOTAL);
		if (total != null) {
			result = new BeanMapPartialList(capacity);
			try {
				((BeanMapPartialList) result).setTotal(Integer.parseInt(total));
			} catch (NumberFormatException e) {}
			String rank = reader.getAttribute(TAG_RANK);
			try {
				((BeanMapPartialList) result).setRank(Integer.parseInt(rank));
			} catch (NumberFormatException e) {}
		} else {
			result = new BeanMapList(capacity);
		}
		String date = reader.getAttribute(TAG_DATE);
		if (date != null) {
			try {
				result.setDate(ISODateFormater.toDate(date));
			} catch (ParseException e) {}
		}
		boolean items = false;
		while (reader.hasMoreChildren()) {
			reader.moveDown();
			if (addItems && (TAG_ITEMS.equals(reader.getNodeName()))) {
				reader.moveDown();
				if (!reader.hasMoreChildren()) {
					reader.moveUp();
					continue;
				} else {
					items = true;
				}
			}
			Object item = context.convertAnother(result, BeanMap.class);
			if (item instanceof BeanMap) {
				result.add((BeanMap) item);
			}
			reader.moveUp();
			if (items && !reader.hasMoreChildren()) {
				reader.moveUp();
			}
		}
		return result;
	}

}
