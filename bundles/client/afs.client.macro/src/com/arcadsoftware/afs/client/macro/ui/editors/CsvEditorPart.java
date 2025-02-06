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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.EditorPart;

import com.arcadsoftware.afs.client.macro.internal.Activator;
import com.arcadsoftware.afs.client.macro.internal.ui.viewers.CsvTableViewer;
import com.arcadsoftware.afs.client.macro.model.CSVLines;

public class CsvEditorPart extends EditorPart {

	public final static String CSVFILE_EDITOR_ID = "com.arcadsoftware.afs.client.macro.ui.editors.CsvEditor"; //$NON-NLS-1$

	CSVLines list;
	CsvTableViewer viewer;
	CsvEditorInput listInput;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.IEditorPart#init(org.eclipse.ui.IEditorSite, org.eclipse.ui.IEditorInput)
	 */
	@Override
	public void init(IEditorSite site, IEditorInput input)
			throws PartInitException {
		if ((input == null) || !(input instanceof CsvEditorInput)) {
			throw new PartInitException(Activator.resString("csv.editor.noEditorInput")); //$NON-NLS-1$
		} else {
			setInput(input);
			firePropertyChange(PROP_TITLE);
			listInput = (CsvEditorInput) input;
			list = listInput.getInput();
			setPartName(((CsvEditorInput) input).getName());
		}
		setSite(site);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.ISaveablePart#isDirty()
	 */
	@Override
	public boolean isDirty() {
		return false;
	}

	@Override
	public void createPartControl(Composite parent) {
		viewer = new CsvTableViewer(parent, SWT.NONE, list);
		viewer.setInput(list);
	}

	@Override
	public void doSave(IProgressMonitor arg0) {

	}

	@Override
	public void doSaveAs() {

	}

	@Override
	public boolean isSaveAsAllowed() {
		return false;
	}

	@Override
	public void setFocus() {

	}

	public CsvTableViewer getViewer() {
		return viewer;
	}

}
