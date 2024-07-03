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
package com.arcadsoftware.client.editors.swtwidgets.tools;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.client.editors.swtwidgets.internal.Activator;
import com.arcadsoftware.editor.swt.BeanMapEditorInput;

/**
 * This class permits to open the bean map dynamic editor.
 */
public final class OpenBeanMapEditor {

	private static final String EDITOR_ID = "com.arcadsoftware.editor.swt.DynamicEditorPart"; //$NON-NLS-1$

	/**
	 * Open the default dynamic editor associated to this BeanMap.
	 *
	 * @param beanMap
	 *            The BeanMap to be edited.
	 * @return The created editorPart.
	 */
	public static IEditorPart openEditor(BeanMap beanMap) {
		final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		IEditorPart editor = null;
		try {
			// TODO [JC] make an editor manager
			editor = page.openEditor(new BeanMapEditorInput(beanMap), EDITOR_ID);
		} catch (final PartInitException e) {
			Activator.getInstance().log(e);
			page.closeEditor(editor, false);
		}
		return editor;
	}

	/**
	 * Open the default dynamic editor associated to this BeanMap.
	 *
	 * @param beanMap
	 *            The BeanMap to be edited.
	 * @param layoutName
	 *            The name of the layout used to edit the beanMap.
	 * @return The created editorPart.
	 */
	public static IEditorPart openEditor(BeanMap beanMap, String layoutName) {
		final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		IEditorPart editor = null;
		try {
			final BeanMapEditorInput input = new BeanMapEditorInput(beanMap, layoutName);
			// TODO [JC] make an editor manager
			editor = page.openEditor(input, EDITOR_ID);
		} catch (final PartInitException e) {
			Activator.getInstance().log(e);
			page.closeEditor(editor, false);
		}
		return editor;
	}
}
