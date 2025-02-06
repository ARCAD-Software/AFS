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
package com.arcadsoftware.osgi;

import java.util.Locale;
import java.util.ResourceBundle;

/**
 * Centralized manager of properties files.
 */
public interface IProperties {

	/**
	 * OSGi service reference name.
	 */
	static public String clazz = IProperties.class.getName();
	
	/**
	 * Return the key value for current Locale.
	 * 
	 * @param properties
	 * @param key
	 * @return
	 */
	public String get(String properties, String key);

	/**
	 * Return the resource bundle for current Locale.
	 * 
	 * @param properties
	 * @return
	 */
	public ResourceBundle get(String properties);

	/**
	 * Return the Key value.
	 * 
	 * @param properties
	 * @param key
	 * @param locale
	 * @return
	 */
	public String get(String properties, String key, Locale locale);

	/**
	 * Return the ResourceBundle
	 * 
	 * @param properties
	 * @param locale
	 * @return
	 */
	public ResourceBundle get(String properties, Locale locale);
	
	/**
	 * Return true if the given resource bundle contains the key.
	 * 
	 * @param properties
	 * @param key
	 * @return
	 */
	public boolean containsKey(String properties, String key);
	
}
