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

/**
 *
 */
public interface IEditorChangeListener {

	/**
	 * Fired when a new BeanMap replace another one. the BeanMapEvent reference the new BeanMap.
	 *
	 * @param renderer
	 *            the renderer source of this event.
	 */
	public void changed(ISWTRenderer renderer);

}
