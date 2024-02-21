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
 * Basically typed Bean are JavaBean that store a global identifier as a type information.
 * 
 *  An sample implementation could be to return the <code>getClass().getName()<code> value.
 */
public interface ITypedBean {

	/**
	 * The type attribute is an internal attribute that define the Bean type.
	 * Its value is a String.
	 */
	public static final String KEY_TYPE = "type"; //$NON-NLS-1$

	/**
	 * Types are string keys used to identified the contains of the bean and the
	 * web resources linked to this object.
	 * 
	 * @return the type of the Bean.
	 */
	public String getType();
	

	/**
	 * Compare the types of two TypedBean.
	 * 
	 * @param bm a TypedBean
	 * @return true if bm has the same type. 
	 */
	public boolean equalsType(ITypedBean bm);

}
