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

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapPartialList;
import com.arcadsoftware.beanmap.IBeanMap;
import com.arcadsoftware.beanmap.internal.xml.BeanMapConverter;
import com.arcadsoftware.beanmap.internal.xml.BeanMapListConverter;
import com.arcadsoftware.beanmap.internal.xml.BeanMapListUpdateConverter;
import com.arcadsoftware.rest.JsonStreamCompact;
import com.thoughtworks.xstream.converters.basic.DateConverter;

/**
 * JSON Serialization of BeanMap Objects.
 * 
 * <p>
 * This serialization use the same paradigms used for XML conversion but with some modification:
 * 
 * <ul>
 * <li>An "items" element is added into the BeanMapList to contain the BeanMaps objects.
 * <li>As there is no difference between "attributes" and "tag", if a BeanMap contain a key 
 * named "id", "type", "date" of any other reserved attribute name, the both values are stored
 * into the JSON object, in that case the first occurrence is the reserved attribute value.
 * </ul>
 * 
 * <p>
 * <b>This serializer can not transform JSON into BeanMap.</b>
 * 
 * @author ARCAD Software
 */
public class JSonBeanMapStream extends JsonStreamCompact {

	/**
	 * Default Constructor. 
	 * 
	 */
	public JSonBeanMapStream() {
		this(JSonBeanMapStream.class.getClassLoader());
	}

	/**
	 * Construct a XStream parser using the specified classloader.
	 * 
	 * Use this constructor only when you need to parse some bean that are not accessible to the
	 * bundle form where this class is defined.
	 * 
	 * @param classLoader 
	 */
	public JSonBeanMapStream(ClassLoader classLoader) {
		super(classLoader);
		standartInitialization();
	}

	/**
	 * Construct a XStream parser using the specified classloader.
	 * 
	 * Use this constructor only when you need to parse some bean that are not accessible to the
	 * bundle form where this class is defined.
	 * 
	 * @param classLoader 
	 * @param listArray If true lists are managed as pure Array objects.
	 * @param noRoot If true the Root element is removed.
	 * @param addClass If true the names of "com.*" classes is added a a value named "class" of each object.
	 */
	public JSonBeanMapStream(ClassLoader classLoader, boolean listArray, boolean noRoot, boolean addClass) {
		super(classLoader, listArray, noRoot, addClass);
		standartInitialization();
	}

	/**
	 * Construct a XStream parser used to load a specific BeanMap already created.
	 * the result of <code>fromXML()</code> method should return the same instance of the 
	 * BeanMap.
	 * 
	 * @param beanToUpdate
	 */
	public JSonBeanMapStream(BeanMap beanToUpdate) {
		super();
		BeanMapConverter beanConverter = new BeanMapUpdateConverter(getMapper(), false, false, beanToUpdate);
		BeanMapListConverter listConverter = new BeanMapListConverter(beanConverter, getMapper(), true, false);
		beanConverter.setListConverter(listConverter);
		registerConverter(beanConverter);
		registerConverter(listConverter);
	}

	/**
	 * Construct a XStream parser used to load a specific BeanMapList already created.
	 * the result of <code>fromXML()</code> method should return the same instance of the 
	 * BeanMapList.
	 * 
	 * The BeanMap already contained in the list are not updated only new ones are added.
	 * 
	 * @param listToUpdate
	 */
	public JSonBeanMapStream(BeanMapList listToUpdate) {
		super();
		alias(TAG_LIST, listToUpdate.getClass());
		alias(TAG_BEAN, BeanMap.class);
		BeanMapConverter beanConverter = new BeanMapConverter(getMapper(), true, false);
		beanConverter.setListConverter(new BeanMapListConverter(beanConverter, getMapper(), true, false));
		registerConverter(beanConverter);
		registerConverter(new BeanMapListUpdateConverter(getMapper(), listToUpdate));
	}

	protected void standartInitialization() {
		// Convert date to ISO format by default... (but accept other format as well).
		registerConverter(new DateConverter("yyyy-MM-dd'T'HH:mm:ss.S", new String[] { //$NON-NLS-1$ 
				"yyyy-MM-dd HH:mm:ss.S z", //$NON-NLS-1$
				"yyyy-MM-dd HH:mm:ss.S a", //$NON-NLS-1$
				"yyyy-MM-dd HH:mm:ssz", //$NON-NLS-1$ 
				"yyyy-MM-dd HH:mm:ss z", //$NON-NLS-1$
				"yyyy-MM-dd HH:mm:ssa", //$NON-NLS-1$
				"EEE MMM dd HH:mm:ss zzz yyyy" })); //$NON-NLS-1$;
		BeanMapConverter beanMapConverter = new BeanMapConverter(getMapper(), true, false);
		registerConverter(new BeanMapListConverter(beanMapConverter, getMapper(), true, false));
		registerConverter(beanMapConverter);
		alias(TAG_BEAN, BeanMap.class);
		alias(TAG_LIST, BeanMapList.class);
		alias(TAG_LIST, BeanMapPartialList.class);
	}

	/**
	 * Serialize a BeanMap to a compacted JSON string.
	 * 
	 * @param object
	 * @return
	 */
	public String toXML(BeanMap object) {
		// theses aliases are not thread-safe if the object class can have different representations in XML.
		String type = object.getType();
		if (type != null) {
			type = type.replace(BeanMapList.SLASH_TYPE, BeanMapList.SLASH_TAG);
			alias(type, object.getClass());
			alias(BeanMapList.getListTag(type), BeanMapList.class);
		}
		return toXML((Object) object);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.thoughtworks.xstream.XStream#toXML(java.lang.Object)
	 */
	@Override
	public String toXML(Object object) {
		if (object instanceof IBeanMap) {
			String type = ((IBeanMap) object).getType();
			if (type != null) {
				type = type.replace(BeanMapList.SLASH_TYPE, BeanMapList.SLASH_TAG);
				alias(type, object.getClass());
				alias(BeanMapList.getListTag(type), BeanMapList.class);
			}
		}
		return super.toXML(object);
	}

	/**
	 * Serialize a BeanMap to a compacted JSON string.
	 * 
	 * @param object
	 * @return
	 */
	public String toXML(String type, BeanMapList object) {
		// theses aliases are not thread-safe if this type can have different representations in XML.
		type = type.replace(BeanMapList.SLASH_TYPE, BeanMapList.SLASH_TAG);
		alias(type, BeanMap.class);
		alias(BeanMapList.getListTag(type), object.getClass());
		return toXML(object);
	}

	/**
	 * Deserialize a unique BeanMap.
	 * 
	 * @param type
	 *            a BeanMap type
	 * @param xml
	 * @return
	 */
	public Object fromXML(String type, String xml) {
		// theses aliases are not thread-safe if this type can have different representations in XML.
		type = type.replace(BeanMapList.SLASH_TYPE, BeanMapList.SLASH_TAG);
		alias(type, BeanMap.class);
		alias(BeanMapList.getListTag(type), BeanMapList.class);
		return super.fromXML(xml);
	}

}
