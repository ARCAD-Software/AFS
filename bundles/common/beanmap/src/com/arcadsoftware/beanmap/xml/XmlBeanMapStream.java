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

import java.io.StringReader;
import java.io.StringWriter;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapPartialList;
import com.arcadsoftware.beanmap.IBeanMap;
import com.arcadsoftware.beanmap.internal.xml.BeanMapConverter;
import com.arcadsoftware.beanmap.internal.xml.BeanMapListConverter;
import com.arcadsoftware.beanmap.internal.xml.BeanMapListUpdateConverter;
import com.arcadsoftware.beanmap.internal.xml.BeanMapRootReader;
import com.arcadsoftware.beanmap.internal.xml.BeanMapRootWriter;
import com.arcadsoftware.rest.XStreamCompact;
import com.thoughtworks.xstream.io.HierarchicalStreamDriver;
import com.thoughtworks.xstream.io.xml.CompactWriter;

/**
 * A specialized XStream serializer/deserializer for BeanMap and BeanMapList.
 * 
 */
public class XmlBeanMapStream extends XStreamCompact {

	private static class InstanceHolder {
        private static final XmlBeanMapStream INSTANCE = new XmlBeanMapStream();
    }
	
	/**
	 * Singleton instance of the BeanMap XMl serializer.
	 * 
	 * @return
	 */
    public static XmlBeanMapStream getInstance() {
        return InstanceHolder.INSTANCE;
    }
	
	/**
	 * Try to get the BeanMap type from an XML String, without XML parsing.
	 * 
	 * @param xml
	 * @return null is the type can not be found !
	 */
	public static String guessTypeFromXML(String xml) {
		if (xml == null) {
			return null;
		}
		// TODO Optimisation tronquer le xml aux X premiers octets (genre 512...)
		xml = xml.trim().replace('\t', ' ').replace('\n', ' ').replace('\r', ' ');
		if (xml.startsWith("<?")) { //$NON-NLS-1$
			int eh = xml.indexOf("?>"); //$NON-NLS-1$
			if ((eh <= 0) || ((eh + 2) >= xml.length())) {
				return null;
			}
			xml = xml.substring(eh+2).trim();
		}
		if ((!xml.startsWith("<")) || (!xml.endsWith(">"))) { //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
		int b1 = xml.indexOf('>');
		if (b1 <= 1) {
			return null;
		}
		// Teste l'existance de l'attribut "type" dans le premier tag.
		int t1 = xml.indexOf("type=\""); //$NON-NLS-1$
		if ((t1 > 0) && (t1 < b1)) {
			t1 += 6;
			int q1 = xml.indexOf('"', t1);
			if ((q1 > t1) && (q1 < b1)) {
				return xml.substring(t1, q1);
			}
			return null;
		}
		// Renvois le nom du tag entre <... ...>, délimité par espace, / ou > 
		int s1 = xml.indexOf(' ');
		if ((s1 > 0) && (s1 < b1)) {
			return xml.substring(1, s1);
		}
		if (xml.charAt(b1 -1) == '/') {
			return xml.substring(1, b1 - 1).trim();
		}
		return xml.substring(1, b1).trim();
	}
	
	/**
	 * Default Constructor. 
	 */
	protected XmlBeanMapStream() {
		this(XmlBeanMapStream.class.getClassLoader());
	}

	/**
	 * Construct a XStream parser using the specified classloader.
	 * 
	 * Use this constructor only when you need to parse some bean that are not accessible to the
	 * bundle form where this class is defined.
	 * 
	 * @param classLoader 
	 */
	public XmlBeanMapStream(ClassLoader classLoader) {
		super(classLoader);
		standartInitialization();
	}

	/**
	 * Construct a XStream parser using the specified classloader and driver.
	 * 
	 * Use this constructor only when you need to parse some bean that are not accessible to the
	 * bundle form where this class is defined and that you want to produce specially formated files.
	 * 
	 * @param classLoader 
	 */
	public XmlBeanMapStream(ClassLoader classLoader, HierarchicalStreamDriver driver) {
		super(classLoader, driver);
		standartInitialization();
	}

	/**
	 * Construct a XStream parser used to load a specific BeanMap already created.
	 * the result of <code>fromXML()</code> method should return the same instance of the 
	 * BeanMap.
	 * 
	 * @param beanToUpdate
	 */
	public XmlBeanMapStream(BeanMap beanToUpdate) {
		super();
		BeanMapListConverter listConverter = new BeanMapListConverter(getMapper());
		BeanMapConverter beanConverter = new BeanMapUpdateConverter(getMapper(),beanToUpdate);
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
	public XmlBeanMapStream(BeanMapList listToUpdate) {
		super();
		alias(TAG_LIST, listToUpdate.getClass());
		alias(TAG_BEAN, BeanMap.class);
		BeanMapConverter beanConverter = new BeanMapConverter(getMapper());
		beanConverter.setListConverter(new BeanMapListConverter(getMapper()));
		registerConverter(beanConverter);
		registerConverter(new BeanMapListUpdateConverter(getMapper(),listToUpdate));
	}
	
	/**
	 * Initalization method called on construction of this object.
	 */
	protected void standartInitialization() {
		registerConverter(new BeanMapListConverter(getMapper()));
		registerConverter(new BeanMapConverter(getMapper()));
		alias(TAG_BEAN, BeanMap.class);
		alias(TAG_LIST, BeanMapList.class);
		alias(TAG_LIST, BeanMapPartialList.class);
	}

	/**
	 * Serialize a BeanMap to a compacted XML string.
	 * 
	 * @param object
	 * @return
	 */
	public String toXML(BeanMap object) {
		if (object == null) {
			return null;
		}
		final String type = object.getType();
		if (type != null) {
			StringWriter sw = new StringWriter();
			marshal(object, new BeanMapRootWriter(new CompactWriter(sw), type.replace(BeanMapList.SLASH_TYPE, BeanMapList.SLASH_TAG), null));
			return sw.toString();
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
				StringWriter sw = new StringWriter();
				marshal(object, new BeanMapRootWriter(new CompactWriter(sw), type.replace(BeanMapList.SLASH_TYPE, BeanMapList.SLASH_TAG), null));
				return sw.toString();
			}
		}
		return super.toXML(object);
	}

	/**
	 * Serialize a BeanMap to a compacted XML string.
	 * 
	 * @param object
	 * @return
	 */
	public String toXML(String type, BeanMapList object) {
		if (type == null) {
			return toXML(object);
		}
		type = type.replace(BeanMapList.SLASH_TYPE, BeanMapList.SLASH_TAG);
		StringWriter sw = new StringWriter();
		marshal(object, new BeanMapRootWriter(new CompactWriter(sw), BeanMapList.getListTag(type), type));
		return sw.toString();
	}

	/**
	 * Deserialize a unique BeanMap or a BeanMapList.
	 * 
	 * @param type
	 *            a BeanMap type
	 * @param xml
	 * @return
	 */
	public Object fromXML(String type, String xml) {
		if (type == null) {
			return fromXML(xml);
		}
		final String xmlType = type.replace(BeanMapList.SLASH_TYPE, BeanMapList.SLASH_TAG);
		Object result = unmarshal(new BeanMapRootReader(getDriver().createReader(new StringReader(xml)), xmlType, BeanMapList.getListTag(xmlType)));
		if (result instanceof BeanMap bean) {
			if (TAG_BEAN.equals(bean.getType())) {
				bean.setType(type);
			}
		}
		return result;
	}
	
	/**
	 * Test if the the given string may ba an XML string that contain a BeanMap.
	 * 
	 * @param xml
	 * @return
	 */
	public static boolean isBeanMapXML(String xml) {
		return guessTypeFromXML(xml) != null;
	}
}