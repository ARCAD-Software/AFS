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

import com.arcadsoftware.metadata.MetaDataTest;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;

public class TestConverter implements Converter {

	public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
		MetaDataTest test = (MetaDataTest)source;
		if ((test.getEvent() != null) && (test.getEvent().trim().length() > 0)) {
			writer.addAttribute("event", test.getEvent()); //$NON-NLS-1$
		}
		if (test.getCode() != null) {
			writer.addAttribute("code", test.getCode()); //$NON-NLS-1$
		}
		if ((test.getTest() != null) && (test.getTest().trim().length() > 0)) {
			writer.setValue(test.getTest());
		}
	}

	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		String event = reader.getAttribute("event"); //$NON-NLS-1$
		String code = reader.getAttribute("code"); //$NON-NLS-1$
		return new MetaDataTest(null, code, event, reader.getValue());
	}

	public boolean canConvert(@SuppressWarnings("rawtypes") Class type) {
		return MetaDataTest.class.equals(type);
	}

}
