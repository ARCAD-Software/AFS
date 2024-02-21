/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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
package com.arcadsoftware.rest.internal;

import java.text.ParseException;
import java.util.Date;

import com.arcadsoftware.rest.ErrorMessageBean;
import com.arcadsoftware.osgi.ISODateFormater;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;

public class ErrorMessageConverter implements Converter {
	
	public ErrorMessageConverter() {
		super();
	}
	 
	public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
		ErrorMessageBean msg = (ErrorMessageBean)source;
		writer.addAttribute("date", ISODateFormater.toString(msg.getDate())); //$NON-NLS-1$
		writer.addAttribute("name", msg.getName()); //$NON-NLS-1$
		if (msg.getHref() != null) {
			writer.addAttribute("href", msg.getHref()); //$NON-NLS-1$
		}
		if (msg.getDescription() != null) {
			writer.setValue(msg.getDescription());
		}
	}

	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		Date date = new Date();
		try {
			date = ISODateFormater.toDate(reader.getAttribute("date")); //$NON-NLS-1$
		} catch(ParseException e) {}
		String name = reader.getAttribute("name"); //$NON-NLS-1$
		String href = reader.getAttribute("href"); //$NON-NLS-1$
		String desc = reader.getValue(); // Xstream: can not access to xml attributes after the node value.
		return new ErrorMessageBean(date,name,desc,href);
	}

	public boolean canConvert(@SuppressWarnings("rawtypes") Class clazz) {
		return ErrorMessageBean.class.equals(clazz);
	}
}