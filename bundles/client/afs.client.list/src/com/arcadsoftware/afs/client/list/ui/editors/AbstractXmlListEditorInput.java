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

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPersistableElement;
import org.eclipse.ui.PartInitException;

import com.arcadsoftware.aev.core.messages.MessageManager;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.list.internal.Activator;
import com.arcadsoftware.mmk.lists.AbstractXmlList;

public abstract class AbstractXmlListEditorInput implements IEditorInput {

	private final AbstractXmlList list;

	public AbstractXmlListEditorInput(AbstractXmlList list) {
		super();
		this.list = list;
	}

	public AbstractXmlList getList() {
		return list;
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
		return list.getXmlFileName();
	}

	@Override
	public IPersistableElement getPersistable() {
		return null;
	}

	@Override
	public String getToolTipText() {
		return Activator.resString("xmllist.editor.tooltip"); //$NON-NLS-1$
	}

	@Override
	public <T> T getAdapter(Class<T> adapter) {
		return null;
	}

	@Override
	public boolean equals(Object arg0) {
		if (arg0 instanceof AbstractXmlListEditorInput) {
			return ((AbstractXmlListEditorInput) arg0).getList().getXmlFileName().equals(getList().getXmlFileName());
		}
		return false;
	}

	@Override
	public int hashCode() {
		return getList().getXmlFileName().hashCode();
	}

	/**
	 * Ouverture de l'�diteur associ� � un input.
	 *
	 * @param input
	 * @return
	 */
	public static XmlListEditorPart openEditor(AbstractXmlListEditorInput input) {
		try {
			final IEditorPart editor = Activator.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage()
					.openEditor(input, XmlListEditorPart.XMLLIST_EDITOR_ID);
			if (editor instanceof XmlListEditorPart) {
				return (XmlListEditorPart) editor;
			}
		} catch (final PartInitException e) {
			MessageManager.addExceptionBeta(e);
		}
		return null;
	}

	protected abstract String getColumnHeader(String propertyName);

	protected abstract int getColumnSize(String propertyName);

}
