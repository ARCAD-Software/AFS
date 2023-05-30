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

/**
 * This interface can be implemented by "scriptAction" extension point to obtain an reference to the 
 * Renderer.
 */
public interface ISWTRendererAction {

	/**
	 * Set the current renderer.
	 * 
	 * @param renderer
	 */
	public void setRenderer(ISWTRenderer renderer);
}
