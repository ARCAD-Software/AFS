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

import com.arcadsoftware.beanmap.BeanMap;

/**
 * This listener is linked to a BeanMap and fire when the BeanMap is replaced by another one.
 * <p>
 * When we load or reset the BeanMap.
 */
public interface IBeanMapSavedListener {

	/**
	 * Fired when a new BeanMap replace another one. the BeanMapEvent reference the new BeanMap.
	 * 
	 * @param beanmap
	 *            The beanMap saved.
	 */
	public void beanMapSaved(BeanMap beanmap);

}
