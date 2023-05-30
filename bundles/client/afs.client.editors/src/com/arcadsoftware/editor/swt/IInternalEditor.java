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
package com.arcadsoftware.editor.swt;

import com.arcadsoftware.beanmap.BeanMap;

/**
 * This interface must be implemented by SWT provider to implement an internal editor. It's used by editor renderer to
 * execute standards actions on editor.
 */
public interface IInternalEditor {

	/**
	 * Load the specified entity.
	 * 
	 * @param id
	 *            The entity id to be loaded.
	 * @return true if the loading process is complete; false otherwise.
	 */
	public boolean load(int id);

	/**
	 * Reload the current entity.
	 */
	public void reload();

	/**
	 * Save the current entity.
	 * 
	 * @return true if the saving process is complete; false otherwise.
	 */
	public boolean save();

	/**
	 * Returns the internal editor id.
	 * 
	 * @return The internal editor id.
	 */
	public String getInternalEditorId();

	/**
	 * Add a change listener on internal editor.
	 * 
	 * @param listener
	 *            The listener.
	 */
	public void addChangeListener(IEditorChangeListener listener);

	/**
	 * Add a change listener on internal editor.
	 * 
	 * @param listener
	 *            The listener.
	 */
	public void removeChangeListener(IEditorChangeListener listener);

	/**
	 * Returns true if internal editor is dirty, false otherwise.
	 * 
	 * @return true if internal editor is dirty, false otherwise.
	 */
	public boolean isDirty();

	/**
	 * Returns true if internal editor can be saved, false otherwise.
	 * 
	 * @return true if internal editor can be saved, false otherwise.
	 */
	public boolean canSavedEditor();

	/**
	 * Refresh editor content with the given beanMap.
	 * 
	 * @param beanMap
	 *            the beanMap to refresh
	 * @param renderer
	 *            the renderer which have modified the given beanMap
	 */
	public void refreshEditorContent(BeanMap beanMap, ISWTRenderer renderer);

	/**
	 * Link the internalEditor with this parentRenderer
	 * 
	 * @param renderer
	 */
	public void setParentRenderer(ISWTRenderer renderer);

	public ISWTRenderer getRenderer();

}
