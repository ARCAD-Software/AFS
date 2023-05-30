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
package com.arcadsoftware.afs.client.core.ui.selectors;

import java.util.Hashtable;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.eclipse.swt.graphics.Image;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.client.core.ui.dialogs.GenericBeanMapListSelectorDialog;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.client.editors.swtwidgets.widgets.ISearchBeanMap;
import com.arcadsoftware.metadata.MetaDataEntity;

public abstract class AbstractWSSearchSelector implements ISearchBeanMap {
	
	
	private ServerConnection connection;
	
	private String wspath; 
	private Hashtable<String, String> parameters;
	
	public AbstractWSSearchSelector(ServerConnection connection, String wspath, Hashtable<String, String> parameters){
		this.connection = connection;
		this.wspath = wspath;
		this.parameters = parameters;
	}

	public BeanMap search(MetaDataEntity structure) {		
		DataAccessHelper helper = new DataAccessHelper(connection);
		StringBuilder path = new StringBuilder(wspath);
		if (parameters.size()>0) {
			path.append("?");
			StringBuilder parameterString = new StringBuilder("");
			Set<String> keySet = parameters.keySet();
			for (String key:keySet) {
				String value = parameters.get(key);
				if (value!=null) {
					if (parameterString.length()>0) {
						parameterString.append("&");
					}
					parameterString.append(key).append("=").append(value);			
				}
			}
			path.append(parameterString);
		}
		
		BeanMapList result = helper.getListFromPath(path.toString(), getType());
		if((result == null || result.isEmpty()) && helper.getLastMessage() != null && StringUtils.isNotBlank(helper.getLastMessage().toString())) {
			String message = helper.getLastMessage().toString();
			if(helper.getLastCause() != null) {
				message += "\n" + ExceptionUtils.getStackTrace(helper.getLastCause());
			}
			Activator.getDefault().openError(message);
			return null;
		}
		return select(connection, result);	
	}

	public BeanMap select(ServerConnection connection, BeanMapList content){
		return GenericBeanMapListSelectorDialog.select(connection, content,false,getType(),getAttributeList(), getTitle(),getElementIcon());
	}


	public Image getElementIcon() {
		return null;
	}
	
	public abstract String getType();
	public abstract String getAttributeList();
	public abstract String getTitle();

	
}


