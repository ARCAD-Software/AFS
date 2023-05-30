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
package com.arcadsoftware.editor.swt.renderer;

import com.arcadsoftware.editor.swt.IUpdateDateChanged;

/**
 * This interface permits renderer using update date listeners.
 */
public interface IUpdateDateListeners {

	/**
	 * Registers the update date changed listener in renderer.
	 * 
	 * @param listener
	 *            The listener.
	 */
	public void addUpdateDateChanged(IUpdateDateChanged listener);

	/**
	 * Unregisters the update date changed listener in renderer.
	 * 
	 * @param listener
	 *            The listener.
	 */
	public void removeUpdateDateChanged(IUpdateDateChanged listener);

}
