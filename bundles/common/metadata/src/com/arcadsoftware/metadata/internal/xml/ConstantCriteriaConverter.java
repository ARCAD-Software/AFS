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
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;

/**
 *
 */
public class ConstantCriteriaConverter implements Converter {

	/* (non-Javadoc)
	 * @see com.thoughtworks.xstream.converters.Converter#marshal(java.lang.Object, com.thoughtworks.xstream.io.HierarchicalStreamWriter, com.thoughtworks.xstream.converters.MarshallingContext)
	 */
	public void marshal(Object obj, HierarchicalStreamWriter writer, MarshallingContext context) {
		if (obj instanceof ConstantCriteria) {
			if (((ConstantCriteria)obj).isValue()) {
				writer.setValue("true"); //$NON-NLS-1$
			} else {
				writer.setValue("false"); //$NON-NLS-1$
			}
		}
	}

	/* (non-Javadoc)
	 * @see com.thoughtworks.xstream.converters.Converter#unmarshal(com.thoughtworks.xstream.io.HierarchicalStreamReader, com.thoughtworks.xstream.converters.UnmarshallingContext)
	 */
	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
		String s = reader.getNodeName();
		if ("true".equalsIgnoreCase(s)) { //$NON-NLS-1$
			return ConstantCriteria.TRUE;
		}
		if ("false".equalsIgnoreCase(s)) { //$NON-NLS-1$
			return ConstantCriteria.FALSE;
		}
		s = reader.getValue();
		return new ConstantCriteria("yes".equalsIgnoreCase(s) ||  //$NON-NLS-1$
				"true".equalsIgnoreCase(s) ||  //$NON-NLS-1$
				"1".equals(s) || //$NON-NLS-1$
				"t".equalsIgnoreCase(s)); //$NON-NLS-1$
	}

	/* (non-Javadoc)
	 * @see com.thoughtworks.xstream.converters.ConverterMatcher#canConvert(java.lang.Class)
	 */
	public boolean canConvert(@SuppressWarnings("rawtypes") Class clazz) {
		return ConstantCriteria.class.equals(clazz);
	}

}
