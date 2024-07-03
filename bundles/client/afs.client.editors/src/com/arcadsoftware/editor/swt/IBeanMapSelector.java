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
package com.arcadsoftware.editor.swt;

import com.arcadsoftware.beanmap.BeanMap;

/**
 * Permits to return selected beanMap.
 */
public interface IBeanMapSelector {

	/**
	 * @return the selected beanMap.
	 */
	public BeanMap getSelectedBeanMap();

	/**
	 * Refresh the selector with the updated BeanMap
	 *
	 * @param beanMap
	 *            the updated beanMap
	 */
	public void refreshSelector(BeanMap beanMap);

	/**
	 * @param bm
	 *            the beanMap to set
	 */
	public void selectBeanMap(BeanMap bm);

}
