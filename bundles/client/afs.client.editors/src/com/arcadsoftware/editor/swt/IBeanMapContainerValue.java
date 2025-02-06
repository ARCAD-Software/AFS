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
package com.arcadsoftware.editor.swt;

import org.eclipse.swt.events.SelectionAdapter;

import com.arcadsoftware.beanmap.BeanMap;

/**
 * This interface must be implemented by widgets that contains BeanMaps.
 */
public interface IBeanMapContainerValue extends IBeanMapContainer {

	/**
	 * @return the BeanMap value.
	 */
	public BeanMap getBeanMapValue();

	/**
	 * Set the BeanMap Value;
	 *
	 * @param beanMap
	 */
	public void setBeanMapValue(BeanMap beanMap);

	/**
	 * Listener to the value change operation.
	 *
	 * @param selectionAdapter
	 */
	public void addSelectionListener(SelectionAdapter selectionAdapter);
}
