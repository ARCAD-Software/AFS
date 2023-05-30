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
package com.arcadsoftware.editor.implementation.swt.renderer;

import java.util.ArrayList;
import java.util.List;

import com.arcadsoftware.editor.swt.renderer.ILoadingListeners;

/**
 * This class permits renderer using loading listeners.
 */
public class LoadingListeners implements ILoadingListeners {

	private List<ILoadingListener> loadingListeners;

	public void addLoadingListener(ILoadingListener listener) {
		if (loadingListeners == null)
			loadingListeners = new ArrayList<ILoadingListener>();
		loadingListeners.add(listener);
	}

	public void removeLoadingListener(ILoadingListener listener) {
		if (loadingListeners != null)
			loadingListeners.remove(listener);
	}

	protected void fireLoadingError() {
		if (loadingListeners != null) {
			for (ILoadingListener listener : loadingListeners) {
				try {
					listener.loadingError();
				} catch (RuntimeException e) {
					removeLoadingListener(listener);
				}
			}
		}
	}
}
