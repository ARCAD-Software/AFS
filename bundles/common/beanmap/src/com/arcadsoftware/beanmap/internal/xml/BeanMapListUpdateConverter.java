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

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapPartialList;
import com.arcadsoftware.osgi.ISODateFormater;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.mapper.Mapper;

/**
 *
 */
public class BeanMapListUpdateConverter extends BeanMapListConverter implements Converter {

	private BeanMapList listToUpdate;
	
	/**
	 * @param mapper
	 */
	public BeanMapListUpdateConverter(Mapper mapper, BeanMapList listToUpdate) {
		super(mapper);
		this.listToUpdate = listToUpdate;
	}

	@Override
	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		if (listToUpdate instanceof BeanMapPartialList) {
			String total = reader.getAttribute(TAG_TOTAL);
			if (total != null) {
				try {
					((BeanMapPartialList)listToUpdate).setTotal(Integer.parseInt(total));
				} catch (NumberFormatException e) {}
				String rank = reader.getAttribute(TAG_RANK);
				try {
					((BeanMapPartialList)listToUpdate).setRank(Integer.parseInt(rank));
				} catch (NumberFormatException e) {}
			}
		}
		String count = reader.getAttribute(TAG_COUNT);
		int capacity = 12;
		if (count != null) {
			try {
				capacity = Integer.parseInt(count);
			} catch (NumberFormatException e) {}
		}
		String date = reader.getAttribute(BeanMap.KEY_DATE);
		if (date != null) {
			try {
				listToUpdate.setDate(ISODateFormater.toDate(date));
			} catch (ParseException e) {}
		}
		BeanMapList newones = new BeanMapList(capacity);
        while (reader.hasMoreChildren()) {
            reader.moveDown();
            Object item = context.convertAnother(listToUpdate, BeanMap.class);
            if (item instanceof BeanMap) {
            	String type = ((BeanMap)item).getType();
            	int id = ((BeanMap)item).getId();
            	if (((type == null) && (listToUpdate.find(id) == null)) || 
            			(type != null) && (listToUpdate.find(id, type) == null)) {
            		newones.add((BeanMap)item);
            	}
            }
            reader.moveUp();
        }
        listToUpdate.addAll(newones);
		return listToUpdate;
		//return super.unmarshal(reader, context);
	}

}
