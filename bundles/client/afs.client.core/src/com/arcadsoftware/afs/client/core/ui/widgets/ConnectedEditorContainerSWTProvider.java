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
package com.arcadsoftware.afs.client.core.ui.widgets;

import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.composites.ConnectedDynamicEditorComposite;
import com.arcadsoftware.afs.client.core.ui.loaders.CoreContentLoader;
import com.arcadsoftware.client.editors.swtwidgets.containers.EditorContainerSWTProvider;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.DynamicEditorComposite;
import com.arcadsoftware.editor.swt.ISWTDataLoader;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataEntity;

public class ConnectedEditorContainerSWTProvider extends EditorContainerSWTProvider{

	protected ServerConnection connection = null;
	
	public void create(ISWTRenderer swtRenderer, ILayoutParameters parameters, boolean isEmpty,
			MetaDataEntity structure) {		
		ISWTDataLoader loader = swtRenderer.getDataLoader();
		if (loader instanceof CoreContentLoader) {
			connection = ((CoreContentLoader)loader).getConnection();	
		}	
		super.create(swtRenderer, parameters, isEmpty, structure);
	}
	
	protected DynamicEditorComposite createEditorComposite(Composite parent, String type, String layoutName) {		
		DynamicEditorComposite dynamicEditorComposite = new ConnectedDynamicEditorComposite(connection, parent, 0, type,layoutName, getParentRenderer().isReadOnly());
		dynamicEditorComposite.loadEmptyEntity();
		GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		dynamicEditorComposite.setLayoutData(gridData);
		return dynamicEditorComposite;
	}
	
}
