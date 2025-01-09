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
package com.arcadsoftware.metadata.internal.xml;

import com.arcadsoftware.metadata.criteria.ConstantCriteria;
import com.arcadsoftware.metadata.criteria.PreGeneratedCriteria;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;

public class PregeneratedCriteriaConverter implements Converter {

	public boolean canConvert(@SuppressWarnings("rawtypes") Class type) {
		return PreGeneratedCriteria.class.equals(type);
	}
	
	public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
	}
	
	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		return ConstantCriteria.FALSE;
	}

}
