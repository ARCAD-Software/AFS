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
package com.arcadsoftware.editor.swt;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.editor.swt.renderer.ILoadedListListener;

/**
 * Define a BeanMapList container that can be bind to a MetaDataLink.
 * 
 * @see IBeanMapContainer
 */
public interface IBeanMapContainerList extends IBeanMapContainer, ILoadedListListener {

	/**
	 * @return the BeanMap list.
	 */
	public BeanMapList getBeanMapList();

	/**
	 * set the BeanMap list.
	 * 
	 * @param list
	 */
	public void setBeanMapList(BeanMapList list);

	/**
	 * Add a element to the list.
	 * 
	 * @param index
	 * @param beanMap
	 */
	public void addBeanMapToList(int index, BeanMap beanMap);

	/**
	 * Get the list of used attributes (this is used when the list is loaded).
	 * 
	 * @return null if none attribute is required.
	 */
	public String getAttributeList();
	
	/**
	 * Get the list of attributes to be ordered (this is used when the list is loaded).
	 * 
	 * @return null if no specific order is required.
	 */
	public String getOrderList();
}
