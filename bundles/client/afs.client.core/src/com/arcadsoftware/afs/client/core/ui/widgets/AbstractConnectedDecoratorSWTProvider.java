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

import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.loaders.CoreContentLoader;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IDecoratorSWTProvider;
import com.arcadsoftware.editor.swt.ISWTDataLoader;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataEntity;

public abstract class AbstractConnectedDecoratorSWTProvider implements IDecoratorSWTProvider {

	protected DataAccessHelper helper;
	protected ServerConnection connection;
	protected ISWTRenderer renderer;

	
	public Widget create(ISWTRenderer renderer, ILayoutParameters parameters, MetaDataEntity structure) {
		this.renderer = renderer;
		ISWTDataLoader loader = renderer.getDataLoader();
		if (loader instanceof CoreContentLoader) {
			connection = ((CoreContentLoader)loader).getConnection();	
			if (connection!=null) {
				helper = new DataAccessHelper(connection);
			}
		}	
		return createContent(renderer,parameters,structure);
	}
	
	public abstract Widget createContent(ISWTRenderer renderer, ILayoutParameters parameters, MetaDataEntity structure);

}
