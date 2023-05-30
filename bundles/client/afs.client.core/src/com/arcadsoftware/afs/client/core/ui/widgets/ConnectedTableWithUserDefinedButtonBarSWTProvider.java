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

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.editors.ConnectedDynamicEditor;
import com.arcadsoftware.afs.client.core.ui.loaders.CoreContentLoader;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.client.editors.swtwidgets.inputs.TableWithButtonBarSWTProvider;
import com.arcadsoftware.editor.swt.ISWTDataLoader;

public class ConnectedTableWithUserDefinedButtonBarSWTProvider extends TableWithButtonBarSWTProvider{
	private DataAccessHelper helper = null;
	
	public DataAccessHelper getHelper(){
		if (helper==null) {
			if (getConnection()!=null) {
				helper = new DataAccessHelper(getConnection());
			}
		}
		return helper;
	}	
	
	public ServerConnection getConnection(){
		ISWTDataLoader dataLoader = renderer.getDataLoader();
		if ( dataLoader instanceof CoreContentLoader) {
			CoreContentLoader loader = (CoreContentLoader)dataLoader;
			ServerConnection connection = loader.getConnection();
			return connection;
		}		
		return null;
	}	
	
	protected boolean editBeanMap(BeanMap beanMap){	
		ConnectedDynamicEditor.openConnectedEditor(getConnection(),beanMap);
		return true;
	}
	

}
