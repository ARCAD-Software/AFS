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
package com.arcadsoftware.afs.client.ssh.internal.actions;

import com.arcadsoftware.afs.client.core.ui.widgets.AbstractConnectedEditorAction;
import com.arcadsoftware.editor.implementation.swt.renderer.SWTRenderer;
import com.arcadsoftware.editor.swt.IBeanMapSelector;

public class EditorSaveAction extends AbstractConnectedEditorAction {

	@Override
	public void run() {
		final IBeanMapSelector sel = getSelector();
		if (sel instanceof SWTRenderer) {
			final SWTRenderer renderer = (SWTRenderer) sel;
			renderer.requestSave();
		}
	}

}
