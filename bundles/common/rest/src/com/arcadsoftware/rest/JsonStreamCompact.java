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
package com.arcadsoftware.rest;

import com.arcadsoftware.rest.internal.JSonCompactDriver;

/**
 * This class implement a JSon serializer with compact output.
 * 
 * <p>This serializer can be used to convert BeanMaps to JSon but can not <b>deserialize</b>.
 * 
 * @see JettisonStreamCompact
 */
public class JsonStreamCompact extends XStreamCompact {

	/**
	 * If you want to serialize a BeanMap simply use this constructor.
	 */
	public JsonStreamCompact() {
		this((ClassLoader) null);
	}

	/**
	 * This constructor can be used if you want to serialize some special classes without 
	 * declare any aliases.
	 *  
	 * @param classLoader
	 */
	public JsonStreamCompact(ClassLoader classLoader) {
		super(classLoader, new JSonCompactDriver());
		setMode(NO_REFERENCES);
	}

	/**
	 * This constructor can be used if you want to serialize some special classes without 
	 * declare any aliases.
	 *  
	 * @param classLoader
	 * @param listArray If true lists are managed as pure Array objects.
	 */
	public JsonStreamCompact(ClassLoader classLoader, boolean listArray) {
		super(classLoader, new JSonCompactDriver(listArray));
		setMode(NO_REFERENCES);
	}

	/**
	 * This constructor can be used if you want to serialize some special classes without 
	 * declare any aliases.
	 *  
	 * @param classLoader
	 * @param listArray If true lists are managed as pure Array objects.
	 * @param noRoot If true the Root element is removed.
	 * @param addClass If true the names of "com.*" classes is added a a value named "class" of each object.
	 */
	public JsonStreamCompact(ClassLoader classLoader, boolean listArray, boolean noRoot, boolean addClass) {
		super(classLoader, new JSonCompactDriver(listArray, noRoot, addClass));
		setMode(NO_REFERENCES);
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void addImplicitArray(Class ownerType, String fieldName, Class itemType) {
		// Implicit items are not supported in JSON format.
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void addImplicitArray(Class ownerType, String fieldName, String itemName) {
		// Implicit items are not supported in JSON format.
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void addImplicitArray(Class ownerType, String fieldName) {
		// Implicit items are not supported in JSON format.
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void addImplicitCollection(Class ownerType, String fieldName, Class itemType) {
		// Implicit items are not supported in JSON format.
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void addImplicitCollection(Class ownerType, String fieldName, String itemFieldName, Class itemType) {
		// Implicit items are not supported in JSON format.
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void addImplicitCollection(Class ownerType, String fieldName) {
		// Implicit items are not supported in JSON format.
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void addImplicitMap(Class ownerType, String fieldName, Class itemType, String keyFieldName) {
		// Implicit items are not supported in JSON format.
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void addImplicitMap(Class ownerType, String fieldName, String itemName, Class itemType,
			String keyFieldName) {
		// Implicit items are not supported in JSON format.
	}
	
}
