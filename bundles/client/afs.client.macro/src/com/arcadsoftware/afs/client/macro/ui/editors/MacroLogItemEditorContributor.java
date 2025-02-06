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
package com.arcadsoftware.afs.client.macro.ui.editors;

import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.part.EditorActionBarContributor;

public class MacroLogItemEditorContributor extends EditorActionBarContributor {

	private MacroLogItemEditorPart editor;

	public MacroLogItemEditorContributor() {
	}

	@Override
	public void setActiveEditor(IEditorPart targetEditor) {
		if (targetEditor instanceof MacroLogItemEditorPart) {
			editor = (MacroLogItemEditorPart) targetEditor;
		} else {
			editor = null;
		}
	}

	@Override
	public void contributeToToolBar(IToolBarManager toolBarManager) {
		super.contributeToToolBar(toolBarManager);
		if (editor != null) {
			editor.getViewer().contributeToToolBar(toolBarManager);
		}
	}

}
