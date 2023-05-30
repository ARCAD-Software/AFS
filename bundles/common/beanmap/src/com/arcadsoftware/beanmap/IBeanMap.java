/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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
package com.arcadsoftware.beanmap;

import java.util.Set;
import java.util.Map.Entry;

/**
 * This interface define the standard BeanMap service.
 */
public interface IBeanMap extends ITypedBean, IIdentifiedBean {

	/**
	 * @param key the attribute key.
	 * @param value the attribute value.
	 * @return the previous attribute value.
	 */
	public Object put(String key, Object value);

	/**
	 * @param key the attribute code.
	 * @return the attribute value.
	 */
	public Object get(String key);

	/**
	 * Return a object converted to the desired type.
	 * 
	 * <p>Conversions are as follow (not exhaustive list) :</p> 
	 * 
	 * <ul>
	 * <li> BeanMap to Integer : If the object is a BeanMap and the desired class Integer, then
	 * the conversion will return the BeanMap id.
	 * <li> Date : many date formats are supported (most current local String format should be correctly converted).
	 * </ul>
	 * 
	 * @param <T> any type (converted types are String, Integer, Date, Boolean, Float).
	 * @param key the attribute code.
	 * @param clazz the desired class
	 * @return the Object of the desired class or null if it can not be converted.
	 */
	public <T extends Object> T get(String key, Class<T> clazz);

	/**
	 * @return the number of actual attributes.
	 */
	public int size();
	
	/**
	 * @return true is this BeanMap contain no specifics informations (aka. attributes).
	 */
	public boolean isEmpty();
	
	/**
	 * @return the current list of used keys.
	 */
	public Set<String> keys();
	
	/**
	 * Return the key value as an integer.
	 * 
	 * @param key the attribute key.
	 * @return zero if the key does not exist, -1 if the key is not an integer.
	 */
	public int getInt(String key);

	/**
	 * Return the key value as a float.
	 * 
	 * @param key
	 *            the attribute key.
	 * @return zero if the key does not exist, -1 if the key is not a float.
	 */
	public float getFloat(String key);

	/**
	 * Return the key value as a String.
	 * 
	 * @param key the attribute key.
	 * @return null if the key does not exist or the string representation of the value.
	 */
	public String getString(String key);

	/**
	 * @return the list of the attributes of this BeanMap except "id" and "type"
	 */
	public Set<Entry<String, Object>> entrySet();
	
	/**
	 * Insert into this bean all the values contained into another one except its Id and type.
	 * 
	 * @param bean
	 */
	public IBeanMap addAll(IBeanMap bean);
}
