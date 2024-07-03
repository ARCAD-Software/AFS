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
package com.arcadsoftware.editor.swt.renderer;

import com.arcadsoftware.editor.swt.IInternalEditor;

/**
 * This interface permits renderer used internal editors.
 */
public interface IInternalEditors {

	/**
	 * Load the entity in the internal editor.
	 *
	 * @param editorId
	 *            The internal editor id.
	 * @param entityId
	 *            The entity id.
	 * @return true if the load is complete, false otherwise.
	 */
	public boolean loadInternalEditor(String editorId, int entityId);

	/**
	 * Load the entity in the internal editor.
	 *
	 * @param editorId
	 *            The internal editor id.
	 * @param entityId
	 *            The entity id.
	 * @return true if the load is complete, false otherwise.
	 */
	public boolean loadInternalEditor(String editorId, int entityId, ILoaderCallback callback);

	/**
	 * Returns true if at least one internal editor is dirty, false otherwise.
	 *
	 * @return true if at least one internal editor is dirty, false otherwise.
	 */
	public boolean internalEditorsAreDirty();

	/**
	 * Registers the internal editor in renderer.
	 *
	 * @param editor
	 *            The internal editor.
	 */
	public void addInternalEditor(IInternalEditor editor);

	/**
	 * Unregisters the internal editor in renderer.
	 *
	 * @param editor
	 *            The internal editor.
	 */
	public void removeInternalEditor(IInternalEditor editor);

	public boolean isAddForEditorId(String editorId);

	public void setAdd(String editorId);

	public IInternalEditor getInternalEditor(String editorId);

}
