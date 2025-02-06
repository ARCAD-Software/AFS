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
package com.arcadsoftware.rest;

import java.io.File;
import java.io.InputStream;

import com.arcadsoftware.rest.internal.ErrorMessageConverter;
import com.arcadsoftware.rest.internal.OSGiXStreamClassLoader;
import com.arcadsoftware.rest.internal.MxCompactDriver;
import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.ConverterLookup;
import com.thoughtworks.xstream.converters.ConverterRegistry;
import com.thoughtworks.xstream.converters.basic.DateConverter;
import com.thoughtworks.xstream.converters.reflection.ReflectionProvider;
import com.thoughtworks.xstream.core.ClassLoaderReference;
import com.thoughtworks.xstream.io.HierarchicalStreamDriver;
import com.thoughtworks.xstream.mapper.Mapper;

/**
 * This class extend <code>XStream</code> class. by adding specific mappings.
 * 
 * <p>
 * This implementation is <b>not</b> threadsafe (an instance of XStreamCompact can not be shared between different
 * threads
 * 
 * <p>
 * You can provide specific mapping by override the <code>InitializeBase</code> method.
 * 
 */
public class XStreamCompact extends XStream {

	// Default tag names if no type are given for aliases.
	public static final String TAG_LIST = "list"; //$NON-NLS-1$
	public static final String TAG_BEAN = "item"; //$NON-NLS-1$
	public static final String TAG_ENTITY = "entity"; //$NON-NLS-1$
	public static final String TAG_ATTRIBUTE = "attribute"; //$NON-NLS-1$
	public static final String TAG_LINK = "link"; //$NON-NLS-1$
	public static final String TAG_TEST = "test"; //$NON-NLS-1$
	public static final String TAG_TYPE = "type"; //$NON-NLS-1$
	public static final String TAG_ID = "id"; //$NON-NLS-1$
	public static final String TAG_DATE = "date"; //$NON-NLS-1$

	/**
	 * List all the reserved keyword that can not be used as attributes names.
	 * 
	 * @return a non null array.
	 */
	public static final String[] getReservedXMLNames() {
		return new String[] { "xml", //$NON-NLS-1$
			TAG_BEAN, TAG_LIST, TAG_ID, TAG_TYPE, "uri", //$NON-NLS-1$
			"href"}; //$NON-NLS-1$
	}
	
	/**
	 * Default Constructor. 
	 * 
	 */
	public XStreamCompact() {
		this((ClassLoader) null);
	}

	/**
	 * Construct a XStream parser using the specified classloader.
	 * 
	 * Use this constructor only when you need to parse some bean that are not accessible to the
	 * bundle form where this class is defined.
	 * 
	 * @param classLoader 
	 */
	public XStreamCompact(ClassLoader classLoader) {
		this(classLoader, new MxCompactDriver());
	}

	/**
	 * Construct an XML streamer for the specified driver.
	 * 
	 * @param compactDriver
	 * @param reflectionProvider
	 */
	public XStreamCompact(ClassLoader classLoader, HierarchicalStreamDriver compactDriver) {
		this(compactDriver, classLoader);
		InitializeBase();
	}

	/**
	 * Construct an XML streamer for the specified <code>ConverterLookup</code>.
	 * 
	 * <p>
	 * Constructor without any initialization, do not provide any specific Converter.
	 * 
	 * @param compactDriver
	 * @param classLoader
	 */
	protected XStreamCompact(HierarchicalStreamDriver compactDriver, ClassLoader classLoader) {
		super((ReflectionProvider) null, 
				compactDriver, 
				new ClassLoaderReference(new OSGiXStreamClassLoader(classLoader)), 
				(Mapper) null);
		initializeSecurity();
	}

	/**
	 * Construct an XML streamer for the specified <code>ConverterLookup</code>.
	 * 
	 * <p>
	 * Constructor without any initialization, do not provide any specific Converter.
	 * 
	 * @param classLoader
	 * @param converterLookup
	 */
	protected XStreamCompact(ClassLoader classLoader, ConverterLookup converterLookup) {
		this(new MxCompactDriver(), classLoader, converterLookup);
	}

	/**
	 * Construct an XML streamer for the specified <code>ConverterLookup</code>.
	 * 
	 * <p>
	 * Constructor without any initialization, do not provide any specific Converter.
	 * 
	 * @param driver
	 * @param classLoader
	 * @param converterLookup
	 */
	protected XStreamCompact(HierarchicalStreamDriver driver, ClassLoader classLoader, ConverterLookup converterLookup) {
		super(null, driver, new ClassLoaderReference(new OSGiXStreamClassLoader(classLoader)), null, converterLookup, new ConverterRegistry() {
			@Override
			public void registerConverter(Converter converter, int priority) {
				// FIXME Modification de XStrem 1.4.9 à tester !!!
			}
		});
		initializeSecurity();
	}

	/**
	 * This method is used by all create methods (except one {@link com.arcadsoftware.rest.XStreamCompact#XStreamCompact(com.thoughtworks.xstream.io.HierarchicalStreamDriver,ClassLoader,com.thoughtworks.xstream.converters.ConverterLookup)}).
	 */
	protected void InitializeBase() {
		// Convert date to ISO format by default... (but accept other format as well).
		registerConverter(new DateConverter("yyyy-MM-dd'T'HH:mm:ss.S", new String[] { //$NON-NLS-1$ 
				"yyyy-MM-dd HH:mm:ss.S z", //$NON-NLS-1$
				"yyyy-MM-dd HH:mm:ss.S a", //$NON-NLS-1$
				"yyyy-MM-dd HH:mm:ssz", //$NON-NLS-1$ 
				"yyyy-MM-dd HH:mm:ss z", //$NON-NLS-1$
				"yyyy-MM-dd HH:mm:ssa", //$NON-NLS-1$
				"EEE MMM dd HH:mm:ss zzz yyyy" })); //$NON-NLS-1$;
		// Error messages aliases...
		registerConverter(new ErrorMessageConverter());
		alias("message", ErrorMessageBean.class); //$NON-NLS-1$
		useAttributeFor(ErrorMessageBean.class, "name"); //$NON-NLS-1$
		useAttributeFor(ErrorMessageBean.class, "date"); //$NON-NLS-1$
	}

	/**
	 * This method is used by all create methods to set the required level of security during deserializing.
	 */
	protected void initializeSecurity() {
		allowTypesByWildcard(new String[] {"com.arcadsoftware.**"});
	}

	@Override
	public void alias(String name, @SuppressWarnings("rawtypes") Class type) {
		super.alias(name, type);
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void alias(String name, Class type, Class defaultImplementation) {
		super.alias(name, type, defaultImplementation);
	}

	@Override
	public void useAttributeFor(String fieldName, @SuppressWarnings("rawtypes") Class type) {
		super.useAttributeFor(fieldName, type);
	}

	@Override
	public void useAttributeFor(@SuppressWarnings("rawtypes") Class definedIn, String fieldName) {
		super.useAttributeFor(definedIn, fieldName);
	}

	@Override
	public void useAttributeFor(@SuppressWarnings("rawtypes") Class type) {
		super.useAttributeFor(type);
	}

	@Override
	public String toXML(Object object) {
		return super.toXML(object);
	}

	@Override
	public Object fromXML(String xml) {
		return super.fromXML(xml);
	}

	@Override
	public Object fromXML(InputStream input) {
		return super.fromXML(input);
	}

	@Override
	public Object fromXML(File file) {
		return super.fromXML(file);
	}	
	
}