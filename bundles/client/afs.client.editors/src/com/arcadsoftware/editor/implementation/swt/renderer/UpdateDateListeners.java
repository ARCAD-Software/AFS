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
package com.arcadsoftware.editor.implementation.swt.renderer;

import java.util.ArrayList;
import java.util.List;

import com.arcadsoftware.editor.swt.IUpdateDateChanged;
import com.arcadsoftware.editor.swt.renderer.IUpdateDateListeners;

/**
 * This class permits renderer using update date listeners.
 */
public class UpdateDateListeners implements IUpdateDateListeners {

	private List<IUpdateDateChanged> updateDateListeners;

	@Override
	public void addUpdateDateChanged(IUpdateDateChanged listener) {
		if (updateDateListeners == null) {
			updateDateListeners = new ArrayList<>();
		}
		updateDateListeners.add(listener);
	}

	@Override
	public void removeUpdateDateChanged(IUpdateDateChanged listener) {
		if (updateDateListeners != null) {
			updateDateListeners.remove(listener);
		}
	}

	protected void fireUpdateDateChanged() {
		if (updateDateListeners != null) {
			for (final IUpdateDateChanged listener : updateDateListeners) {
				try {
					listener.updateDateChanged();
				} catch (final RuntimeException e) {
					removeUpdateDateChanged(listener);
				}
			}
		}
	}
}
