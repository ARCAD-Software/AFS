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
package com.arcadsoftware.afs.client.core.tools;

import java.util.Hashtable;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.tools.MetadataUtils;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;

public class MetadataUtils {
	
	private static MetadataUtils instance = new MetadataUtils();
	private static Hashtable<String, MetaDataEntity>  entities;
	
	private MetadataUtils(){
		entities = new Hashtable<String, MetaDataEntity>();
	}

	public static MetadataUtils getInstance(){
		return instance;
	}
	
	/**
	 * Returns the final MetaDataAttribute from a complex attribute String
	 * <p>We call <b>Complex attribute String</b> a string built using severals '.' (ex: import.application.code)</p>
	 * @param helper A communication helper
	 * @param primaryEntity The root entity from which the the attribute will be resolved 
	 * @param attribute The complex attribute string to be resolved.
	 * @return the final MetaDataAttribute or null;
	 */
	public MetaDataAttribute resolveMetaDataAttribute (DataAccessHelper helper,MetaDataEntity primaryEntity, String attribute) {
		int pos = attribute.indexOf('.'); 
		if (pos>-1) {  
			String primaryCode = attribute.substring(0,pos);
			String remainingAttribute = attribute.substring(pos+1);
			MetaDataAttribute primaryAttribute =  primaryEntity.getAttribute(primaryCode);
			if (primaryAttribute!=null) {
				String primaryType = primaryAttribute.getType();
				MetaDataEntity secondaryEntity = entities.get(primaryType);
				if (secondaryEntity==null) {				
					secondaryEntity = helper.getEntity(primaryType);
					entities.put(primaryType, secondaryEntity);
				}
				if (secondaryEntity!=null) {
					return resolveMetaDataAttribute(helper,secondaryEntity,remainingAttribute);
				}
			}
		} else {
			return primaryEntity.getAttribute(attribute);
		}
		return null;
	}
}
