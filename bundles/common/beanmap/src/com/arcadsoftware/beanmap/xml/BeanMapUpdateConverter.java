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
package com.arcadsoftware.beanmap.xml;

import java.text.ParseException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.internal.xml.BeanMapConverter;
import com.arcadsoftware.osgi.ISODateFormater;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.mapper.Mapper;

public class BeanMapUpdateConverter extends BeanMapConverter {

	private BeanMap beanToUpdate;

	/**
	 * @param mapper
	 */
	public BeanMapUpdateConverter(Mapper mapper, BeanMap beanToUpdate) {
		super(mapper);
		this.beanToUpdate = beanToUpdate;
		// Used for sub BeanMap...
		setThisConverter(new BeanMapConverter(mapper));
	}

	public BeanMapUpdateConverter(Mapper mapper, boolean writeType, boolean useAttributes, BeanMap beanToUpdate) {
		super(mapper, writeType, useAttributes);
		this.beanToUpdate = beanToUpdate;
	}

	@Override
	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		String sid = reader.getAttribute(BeanMap.KEY_ID);
		int id = 0;
		if (sid != null) {
			try {
				id = Integer.parseInt(sid);
			} catch (NumberFormatException e) {
			}
		}
		if (id != 0) {
			beanToUpdate.setId(id);
		}
		String date = reader.getAttribute(BeanMap.KEY_DATE);
		if (date != null) {
			try {
				beanToUpdate.setDate(ISODateFormater.toDate(date));
			} catch (ParseException e) {
			}
		}
		return updateBean(reader, context);
	}

	protected Object updateBean(HierarchicalStreamReader reader, UnmarshallingContext context) {
		for (int i = 0; i < reader.getAttributeCount(); i++) {
			String key = reader.getAttributeName(i);
			String val = reader.getAttribute(i);
			if ((!BeanMap.KEY_ID.equals(key)) && (!BeanMap.KEY_DATE.equals(key)) && (!BeanMap.KEY_TYPE.equals(key))) {
				beanToUpdate.put(key.replace('_', '.'), xmlToValue(key,val));
			}
		}
		// Recuperation autres valeurs
		while (reader.hasMoreChildren()) {
			reader.moveDown();
			String key = reader.getNodeName().replace('_', '.');
			beanToUpdate.put(key, readAttribute(reader, context, key, beanToUpdate));
			reader.moveUp();
		}
		return beanToUpdate;
	}

}
