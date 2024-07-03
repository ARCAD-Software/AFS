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
package com.arcadsoftware.editor.swt.renderer;

import com.arcadsoftware.editor.implementation.swt.renderer.ILoadingListener;

/**
 * This interface permits renderer using loading listeners.
 */
public interface ILoadingListeners {

	/**
	 * Add the given loading listener.
	 *
	 * @param listener
	 *            The loading listener to be added.
	 */
	public void addLoadingListener(ILoadingListener listener);

	/**
	 * Remove the given loading listener.
	 *
	 * @param listener
	 *            The loading listener to be removed.
	 */
	public void removeLoadingListener(ILoadingListener listener);

}
