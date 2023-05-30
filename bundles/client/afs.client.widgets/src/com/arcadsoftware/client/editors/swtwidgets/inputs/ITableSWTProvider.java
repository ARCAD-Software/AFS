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

import com.arcadsoftware.client.editors.swtwidgets.viewer.BeanMapTableViewer;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.ISWTRenderer;

/**
 * TableSWTProvider interface.
 */
public interface ITableSWTProvider extends IColumnedViewerSWTProvider{

	/**
	 * @return The associated renderer.
	 */
	public ISWTRenderer getRenderer();

	/**
	 * 
	 * @return the associated Viewer
	 */
	public BeanMapTableViewer getViewer();
	
	public ILayoutParameters getLayoutParameters();
}