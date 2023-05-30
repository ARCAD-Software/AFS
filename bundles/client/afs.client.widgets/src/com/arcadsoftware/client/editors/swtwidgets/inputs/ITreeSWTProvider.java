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

import com.arcadsoftware.aev.core.collections.ArcadCollection;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.client.editors.swtwidgets.viewer.BeanMapTreeViewer;

public interface ITreeSWTProvider extends IColumnedViewerSWTProvider{

	/**
	 * @return the associated Viewer
	 */
	public BeanMapTreeViewer getViewer();

	/**
	 * this method construct the data shape as input of the tree
	 * 
	 * @return data to display in the tree
	 */
	public ArcadCollection createContentData(BeanMapList list);
	
	/**
	 * 
	 * @param element
	 * @param columnIndex
	 * @return the value of the column <code>columnIndex</code>
	 */
	public String getValue(Object element, int columnIndex);
}