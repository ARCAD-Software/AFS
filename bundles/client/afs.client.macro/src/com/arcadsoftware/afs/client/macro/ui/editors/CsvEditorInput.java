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

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPersistableElement;
import org.eclipse.ui.PartInitException;

import com.arcadsoftware.aev.core.messages.MessageManager;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.macro.internal.Activator;
import com.arcadsoftware.afs.client.macro.model.CSVLines;

public class CsvEditorInput implements IEditorInput {

	private final CSVLines input;
	private final String editorName;

	public CsvEditorInput(CSVLines input, String editorName) {
		super();
		this.input = input;
		this.editorName = editorName;
	}

	public CSVLines getInput() {
		return input;
	}

	@Override
	public boolean exists() {
		return false;
	}

	@Override
	public ImageDescriptor getImageDescriptor() {
		return AFSIcon.LIST.imageDescriptor();
	}

	@Override
	public String getName() {
		return editorName;
	}

	@Override
	public IPersistableElement getPersistable() {
		return null;
	}

	@Override
	public String getToolTipText() {
		return Activator.resString("csv.editor.tooltip"); //$NON-NLS-1$
	}

	@Override
	@SuppressWarnings("unchecked")
	public Object getAdapter(@SuppressWarnings("rawtypes") Class arg0) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object arg0) {
		return false;

	}

	/**
	 * Ouverture de l'�diteur associ� � un input.
	 *
	 * @param input
	 * @return
	 */
	public static CsvEditorPart openEditor(CsvEditorInput input) {
		try {
			final IEditorPart editor = Activator.getDefault()
					.getWorkbench()
					.getActiveWorkbenchWindow()
					.getActivePage()
					.openEditor(input,
							CsvEditorPart.CSVFILE_EDITOR_ID);

			if ((editor != null) && (editor instanceof CsvEditorPart)) {
				return (CsvEditorPart) editor;
			}
			return null;
		} catch (final PartInitException e) {
			MessageManager.addExceptionBeta(e);
			return null;
		}
	}

}
