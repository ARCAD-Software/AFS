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
package com.arcadsoftware.afs.client.core.file;

import java.io.File;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IPropertyListener;
import org.eclipse.ui.ISaveablePart;
import org.eclipse.ui.IWorkbenchPart;

public class FileEditorListener implements IPartListener,IPropertyListener {

	protected File file;
	protected IEditorPart editor;
	
	
	private boolean changeInProgress = false;

	public FileEditorListener(final File file) {
		super();
		this.file = file;
	}

	public void setEditor(IEditorPart editor) {
		this.editor = editor;
	}

	public void partActivated(IWorkbenchPart part) {
		// Do nothing
	}

	public void partBroughtToTop(IWorkbenchPart part) {
		// Do nothing
	}

	public void partClosed(IWorkbenchPart part) {
		//To be Overridden
	}

	public void partDeactivated(IWorkbenchPart part) {
		// Do nothing
	}

	public void partOpened(IWorkbenchPart part) {
		// Do nothing
	}

	public File getFile() {
		return file;
	}

	public void setFile(File file) {
		this.file = file;
	}

	public IEditorPart getEditor() {
		return editor;
	}

	public void propertyChanged(Object source, int propId) {
		if ((source==editor) && (propId==ISaveablePart.PROP_DIRTY)){
			if (editor.isDirty()) {
				changeInProgress = true;
			} else {
				if (changeInProgress) {
					saved();
					changeInProgress = false;
				}
			}
		}
	}

	public void saved(){
		//To be Overridden
	}
	
}
