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
package com.arcadsoftware.metadata.xml;

import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.internal.xml.SimpleAttributeEntityConverter;
import com.arcadsoftware.metadata.internal.xml.SimpleMetaDataEntityConverter;
import com.arcadsoftware.metadata.internal.xml.TestConverter;
import com.arcadsoftware.rest.XStreamCompact;
import com.thoughtworks.xstream.io.HierarchicalStreamDriver;

/**
 * XML Streamer specialized to the serialization of MetaData objects.
 * 
 * <p>
 * This class can be used to serialize BeanMaps.
 * 
 * @author ARCAD Software
 */
public class XmlAttributesSimpleStream extends XStreamCompact /*XmlBeanMapStream */{

	public XmlAttributesSimpleStream() {
		super(XmlAttributesSimpleStream.class.getClassLoader());
	}

	public XmlAttributesSimpleStream(ClassLoader classLoader) {
		super(classLoader);
	}

	public XmlAttributesSimpleStream(ClassLoader classLoader, HierarchicalStreamDriver driver) {
		super(classLoader,driver);
	}

	@Override
	protected void InitializeBase() {
		super.InitializeBase();
		// Entities structures aliases...
		useAttributeFor(Element.class, TAG_TYPE);
		useAttributeFor(Element.class, "code"); //$NON-NLS-1$
		useAttributeFor(Element.class, "type"); //$NON-NLS-1$
		useAttributeFor(Element.class, "name"); //$NON-NLS-1$
		registerConverter(new TestConverter());
		alias(TAG_ENTITY, MetaDataEntity.class);
		registerConverter(new SimpleMetaDataEntityConverter());
		registerConverter(new SimpleAttributeEntityConverter());
		alias(TAG_ATTRIBUTE, MetaDataAttribute.class);
		setMode(NO_REFERENCES);
	}

}
