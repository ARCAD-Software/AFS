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
package com.arcadsoftware.metadata.xml;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.xml.XmlBeanMapStream;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.MetaDataTest;
import com.arcadsoftware.metadata.UpdateMetaDataEntity;
import com.arcadsoftware.metadata.internal.xml.MetaDataElementConverter;
import com.arcadsoftware.metadata.internal.xml.MetaDataEntityConverter;
import com.arcadsoftware.metadata.internal.xml.TestConverter;
import com.thoughtworks.xstream.io.HierarchicalStreamDriver;

/**
 * XML Streamer specialized to the serialization of MetaData objects.
 * 
 * <p>
 * This class can be used to serialize BeanMaps.
 * 
 * @author ARCAD Software
 */
public class XmlMetaDataStream extends XmlBeanMapStream {

	public static final String TAG_UPDATEENTITY = "update-entity"; //$NON-NLS-1$

	public XmlMetaDataStream() {
		super(XmlMetaDataStream.class.getClassLoader());
	}

	public XmlMetaDataStream(ClassLoader classLoader) {
		super(classLoader);
	}

	public XmlMetaDataStream(ClassLoader classLoader, HierarchicalStreamDriver driver) {
		super(classLoader,driver);
	}

	public XmlMetaDataStream(BeanMap beanToUpdate) {
		super(beanToUpdate);
	}

	public XmlMetaDataStream(BeanMapList listToUpdate) {
		super(listToUpdate);
	}

	@Override
	protected void InitializeBase() {
		super.InitializeBase();
		// Entities structures aliases...
		useAttributeFor(Element.class, TAG_TYPE);
		useAttributeFor(Element.class, "code"); //$NON-NLS-1$
		useAttributeFor(Element.class, "readonly"); //$NON-NLS-1$
		useAttributeFor(Element.class, "type"); //$NON-NLS-1$
		useAttributeFor(Element.class, "name"); //$NON-NLS-1$
		//useAttributeFor(Element.class, TAG_ID);
		useAttributeFor(MetaDataAttribute.class, "length"); //$NON-NLS-1$
		useAttributeFor(MetaDataAttribute.class, "precision"); //$NON-NLS-1$
		useAttributeFor(MetaDataAttribute.class, "listable"); //$NON-NLS-1$
		useAttributeFor(MetaDataAttribute.class, "mandatory"); //$NON-NLS-1$
		alias(TAG_TEST, MetaDataTest.class);
		registerConverter(new TestConverter());
		alias(TAG_UPDATEENTITY, UpdateMetaDataEntity.class);
		alias(TAG_ENTITY, MetaDataEntity.class);
		registerConverter(new MetaDataEntityConverter(getMapper(), false));
		registerConverter(new MetaDataElementConverter(getMapper()));
		alias(TAG_LINK, MetaDataLink.class);
		alias(TAG_ATTRIBUTE, MetaDataAttribute.class);
		XmlCriteriaStream.initialize(this);
		setMode(NO_REFERENCES);
	}

}
