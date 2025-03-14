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

package com.arcadsoftware.afs.client.list.ui.editors;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.EditorPart;

import com.arcadsoftware.afs.client.list.internal.Activator;
import com.arcadsoftware.afs.client.list.internal.ui.viewers.AbstractXmlListViewer;
import com.arcadsoftware.mmk.lists.AbstractXmlList;

public class XmlListEditorPart extends EditorPart {

	public final static String XMLLIST_EDITOR_ID = "com.arcadsoftware.afs.client.list.ui.editors.XmlListEditor"; //$NON-NLS-1$

	private AbstractXmlList list;
	private AbstractXmlListViewer viewer;
	private AbstractXmlListEditorInput listInput;

	@Override
	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		if ((input == null) || !(input instanceof AbstractXmlListEditorInput)) {
			throw new PartInitException(Activator.resString("xmllist.editor.noEditorInput")); //$NON-NLS-1$
		}
		setInput(input);
		firePropertyChange(PROP_TITLE);
		listInput = (AbstractXmlListEditorInput) input;
		list = listInput.getList();
		setPartName(getPartName() + ": " + ((AbstractXmlListEditorInput) input).getName()); //$NON-NLS-1$
		setSite(site);
	}

	@Override
	public boolean isDirty() {
		return false;
	}

	@Override
	public void createPartControl(Composite parent) {
		// Chargement uniquement des metadatas
		list.load(false, true);
		viewer = new AbstractXmlListViewer(parent, SWT.MULTI | SWT.FULL_SELECTION, list.getMetadatas()) {
			@Override
			protected String getColumnHeader(String propertyName) {
				return listInput.getColumnHeader(propertyName);
			}

			@Override
			protected int getColumnSize(String propertyName) {
				return listInput.getColumnSize(propertyName);
			}
		};
		viewer.setListInput(list);
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
}
