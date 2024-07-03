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
package com.arcadsoftware.afs.client.core.ui.editors;

import java.util.Hashtable;
import java.util.Map;
import java.util.Set;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.listeners.IBeanMapModifier;
import com.arcadsoftware.afs.client.core.ui.loaders.CoreContentLoader;
import com.arcadsoftware.afs.client.core.ui.loaders.CoreEditorLoader;
import com.arcadsoftware.afs.framework.ui.help.DynamicHelp;
import com.arcadsoftware.afs.framework.ui.plugins.LogUITools;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.implementation.swt.renderer.SWTRenderer;
import com.arcadsoftware.editor.swt.DynamicEditorPart;

public class ConnectedDynamicEditor extends DynamicEditorPart {

	private static final String EDITOR_ID = "com.arcadsoftware.afs.client.core.ui.editors.ConnectedDynamicEditor"; //$NON-NLS-1$
	boolean readOnly = false;

	@Override
	protected void initializeDataLoader(SWTRenderer renderer) {
		final Object inputObject = getEditorInput();
		if (inputObject instanceof ConnectedBeanMapInput) {
			final ConnectedBeanMapInput ci = (ConnectedBeanMapInput) inputObject;
			final Object dataLoader = renderer.getDataLoader();
			if (dataLoader instanceof CoreContentLoader) {
				final CoreContentLoader l = (CoreContentLoader) dataLoader;
				l.setServerConnection(ci.getConnection());
			}
		}
	}

	@Override
	protected void initializeEditorLoader(SWTRenderer renderer) {
		final Object inputObject = getEditorInput();
		if (inputObject instanceof ConnectedBeanMapInput) {
			final ConnectedBeanMapInput ci = (ConnectedBeanMapInput) inputObject;
			final Object loader = renderer.getEditorLoader();
			if (loader instanceof CoreEditorLoader) {
				final CoreEditorLoader el = (CoreEditorLoader) loader;
				el.setServerConnection(ci.getConnection());
			}
		}
	}

	@Override
	protected Map<String, Object> getVirtualValues() {
		final Object inputObject = getEditorInput();
		if (inputObject instanceof ConnectedBeanMapInput) {
			final ConnectedBeanMapInput ci = (ConnectedBeanMapInput) inputObject;
			return ci.getVirtualValues();
		}
		return null;
	}

	@Override
	protected void setFixedValues(SWTRenderer renderer) {
		final Object inputObject = getEditorInput();
		if (inputObject instanceof ConnectedBeanMapInput) {
			final ConnectedBeanMapInput ci = (ConnectedBeanMapInput) inputObject;
			if (ci.getModifier() != null) {
				final BeanMap b = new BeanMap(renderer.getCurrentBean().getType());
				ci.getModifier().modify(b);
				final Set<String> keySet = b.keySet();
				for (final String key : keySet) {
					final Object value = b.get(key);
					renderer.put(key, value);
				}

			}
		}
	}

	/**
	 * Open the default dynamic editor associated to this BeanMap.
	 *
	 * @param beanMap
	 *            The BeanMap to be edited.
	 * @return The created editorPart.
	 */
	public static IEditorPart openConnectedEditor(ServerConnection connection, BeanMap beanMap) {
		return openConnectedEditor(connection, beanMap, null);
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
	public static IEditorPart openConnectedEditor(ServerConnection connection, BeanMap beanMap, String layoutName) {
		return openConnectedEditor(connection, beanMap, layoutName, null);
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
	public static IEditorPart openConnectedEditor(ServerConnection connection, BeanMap beanMap, String layoutName,
			Hashtable<String, Object> virtualValues) {
		return openConnectedEditor(connection, beanMap, layoutName, virtualValues, null);
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
	public static IEditorPart openConnectedEditor(ServerConnection connection, BeanMap beanMap, String layoutName,
			Map<String, Object> virtualValues, IBeanMapModifier modifier) {
		return openConnectedEditor(connection, beanMap, layoutName, virtualValues, modifier, false);
	}

	public static IEditorPart openConnectedEditor(ServerConnection connection, BeanMap beanMap, String layoutName,
			Map<String, Object> virtualValues, IBeanMapModifier modifier, boolean readOnly) {
		final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		IEditorPart editor = null;
		try {
			ConnectedBeanMapInput input;
			if (layoutName == null) {
				input = new ConnectedBeanMapInput(connection, beanMap);
			} else {
				input = new ConnectedBeanMapInput(connection, beanMap, layoutName);
			}
			input.setVirtualValues(virtualValues);
			input.setModifier(modifier);
			input.setReadOnly(readOnly);
			editor = page.openEditor(input, EDITOR_ID);
			// Initialize Help Context if it is part of Parameters
			if (editor instanceof ConnectedDynamicEditor) {
				((ConnectedDynamicEditor) editor).initDynamicContextHelpId();
			}
		} catch (final PartInitException e) {
			LogUITools.logError(Activator.getDefault().getBundle(), e);
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
	public static IEditorPart openConnectedEditor(ConnectedBeanMapInput input) {
		final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		IEditorPart editor = null;
		try {
			editor = page.openEditor(input, EDITOR_ID);
		} catch (final PartInitException e) {
			LogUITools.logError(Activator.getDefault().getBundle(), e);
			page.closeEditor(editor, false);
		}
		return editor;
	}

	/**
	 * Refresh Editor content when already launched
	 *
	 * @param connection
	 *            The Server Connection
	 * @param beanMap
	 *            The BeanMap to be edited.
	 * @return true is editor exists, false otherwise.
	 */
	public static boolean refreshConnectedEditor(ServerConnection connection, BeanMap beanMap) {
		final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();

		final IEditorPart editor = page.findEditor(new ConnectedBeanMapInput(connection, beanMap));

		if ((editor != null) && (editor instanceof ConnectedDynamicEditor)) {
			final ConnectedBeanMapInput input = (ConnectedBeanMapInput) editor.getEditorInput();
			input.setBeanMap(beanMap);

			final ConnectedDynamicEditor edr = (ConnectedDynamicEditor) editor;
			edr.reload();
			return true;
		}
		return false;
	}

	/**
	 * Refresh Editor content when already launched
	 *
	 * @param connection
	 *            The Server Connection
	 * @param beanMap
	 *            The BeanMap to be edited.
	 * @return true is editor exists, false otherwise.
	 */
	public static IEditorPart reloadConnectedEditor(ServerConnection connection, BeanMap beanMap, String layoutName) {
		final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();

		final IEditorPart editor = page.findEditor(new ConnectedBeanMapInput(connection, beanMap));

		if ((editor != null) && (editor instanceof ConnectedDynamicEditor)) {
			final ConnectedBeanMapInput input = (ConnectedBeanMapInput) editor.getEditorInput();
			input.setBeanMap(beanMap);

			final ConnectedDynamicEditor edr = (ConnectedDynamicEditor) editor;
			edr.reloadPartControl(layoutName);
			return editor;
		}
		return null;
	}

	/**
	 * Close Editor content when launched
	 *
	 * @param connection
	 *            The Server Connection
	 * @param beanMap
	 *            The BeanMap in edition
	 * @return true is editor exists, false otherwise.
	 */
	public static boolean closeConnectedEditor(ServerConnection connection, BeanMap beanMap) {
		final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		final IEditorPart editor = page.findEditor(new ConnectedBeanMapInput(connection, beanMap));

		if ((editor != null) && (editor instanceof ConnectedDynamicEditor)) {
			page.closeEditor(editor, false);
			return true;
		}
		return false;
	}

	/**
	 * Set Dynamic Context Help Id from renderer Parameters and activate it
	 *
	 * @param helpId
	 */
	public void initDynamicContextHelpId() {
		if (renderer != null) {
			setDynamicContextHelpId(renderer.getHelpContextId());
		}
	}

	/**
	 * Set Dynamic Context Help Id and activate it
	 *
	 * @param helpId
	 */
	public void setDynamicContextHelpId(String helpId) {
		if (renderer != null) {
			DynamicHelp.updateContextHelpId(helpId, renderer.getParent());
		}
	}

	@Override
	public void setFocus() {
		super.setFocus();
		DynamicHelp.showContextHelpId(renderer.getHelpContextId());
	}
}
