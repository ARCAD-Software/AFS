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
package com.arcadsoftware.beanmap;

/**
 * An IdentifiedBean is a JavaBean with an unique ID.
 */
public interface IIdentifiedBean {

	/**
	 * ID property code (reserved attribute code, used as internal Identifier).
	 */
	public static final String KEY_ID = "id"; //$NON-NLS-1$
	
	/**
	 * CODE property code (generally used to give a short functional Identifier to a Bean).
	 */
	public static final String KEY_CODE = "code"; //$NON-NLS-1$
	
	/**
	 * TEXT property code (generally used to document an Bean).
	 */
	public static final String KEY_TEXT = "text"; //$NON-NLS-1$
	
	/**
	 * NAME property code (generally used as a functional Identifier, longer than CODE).
	 */
	public static final String KEY_NAME = "name"; //$NON-NLS-1$
	
	/**
	 * FULLNAME property code (Functional identifier for USERS).
	 */
	public static final String KEY_FULLNAME = "fullname"; //$NON-NLS-1$
	
	/**
	 * Get the internal bean id key value.
	 * 
	 * @return the Bean Id.
	 */
	public int getId();
}
