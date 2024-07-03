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
package com.arcadsoftware.editor.implementation.swt.renderer;

import java.util.ArrayList;
import java.util.List;

import com.arcadsoftware.editor.swt.renderer.ILoadingListeners;

/**
 * This class permits renderer using loading listeners.
 */
public class LoadingListeners implements ILoadingListeners {

	private List<ILoadingListener> loadingListeners;

	@Override
	public void addLoadingListener(ILoadingListener listener) {
		if (loadingListeners == null) {
			loadingListeners = new ArrayList<>();
		}
		loadingListeners.add(listener);
	}

	@Override
	public void removeLoadingListener(ILoadingListener listener) {
		if (loadingListeners != null) {
			loadingListeners.remove(listener);
		}
	}

	protected void fireLoadingError() {
		if (loadingListeners != null) {
			for (final ILoadingListener listener : loadingListeners) {
				try {
					listener.loadingError();
				} catch (final RuntimeException e) {
					removeLoadingListener(listener);
				}
			}
		}
	}
}
