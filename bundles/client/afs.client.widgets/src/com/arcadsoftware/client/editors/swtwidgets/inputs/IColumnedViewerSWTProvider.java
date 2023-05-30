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
package com.arcadsoftware.client.editors.swtwidgets.inputs;

import org.eclipse.jface.resource.ImageDescriptor;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataLink;

public interface IColumnedViewerSWTProvider {

	/**
	 * @param key
	 *            : icon filename
	 * @return The ImageDescriptor of the icon placed in bundle "plugin - connection".
	 */
	public ImageDescriptor getImageDescriptor(String key);

	/**
	 * @return The input of the TableViewer.
	 */
	public BeanMapList getInput();

	/**
	 * Update the input of the TableViewer
	 * 
	 * @param beanMapList
	 *            The new beanMap list.
	 */
	public void setInput(BeanMapList beanMapList);

	/**
	 * Refresh the composite
	 */
	public void refresh();

	/**
	 * @return The selected BeanMap in the TableViewer.
	 */
	public BeanMap getSelection();

	/**
	 * @return The associated renderer.
	 */
	public ISWTRenderer getRenderer();

	public MetaDataLink getLink();
}