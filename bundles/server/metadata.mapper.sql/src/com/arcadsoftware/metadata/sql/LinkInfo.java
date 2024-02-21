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
package com.arcadsoftware.metadata.sql;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;

/**
 * Java Bean used to store all Database related information of a MetadataLink object.
 * 
 * @author ARCAD Software
 */
public class LinkInfo {
	
	public String table;
	public String sourceCol;
	public String destCol;
	public String deleteCol;

	// SQL Cache
	public String sql_add;
	public String sql_test;
	public String sql_delete;
	
	public LinkInfo(final MetaDataEntity entity, final MetaDataLink link) {
		super();
		BeanMap md = link.getMetadata();
		// Gestion des autolink, on inverse le lien destination.
		String autolinkCode = md.getString(MetaDataEntity.METADATA_AUTOLINK);
		if ((autolinkCode != null) && (autolinkCode.length() > 0)) {
			MetaDataEntity le = link.getRefEntity();
			// on ne supporte que les autolinks interne au même domaine. 
			if ((le != null) && le.sameMapper(entity)) {
				MetaDataLink l = le.getLink(autolinkCode);
				if (l != null) {
					md = l.getMetadata();
					table = md.getString(EntityInfo.METADATA_TABLE);
					// On inverse les références.
					sourceCol = md.getString(EntityInfo.METADATA_DESTCOL);
					destCol = md.getString(EntityInfo.METADATA_SOURCECOL);
					deleteCol = md.getString(EntityInfo.METADATA_DELETECOL);
				}
			}
		} else {
			table = md.getString(EntityInfo.METADATA_TABLE);
			if ((table != null) && (table.length() == 0)) {
				table = null;
			}
			sourceCol = md.getString(EntityInfo.METADATA_SOURCECOL);
			if ((sourceCol != null) && (sourceCol.length() == 0)) {
				sourceCol = null;
			}
			destCol = md.getString(EntityInfo.METADATA_DESTCOL);
			if ((destCol != null) && (destCol.length() == 0)) {
				destCol = null;
			}
			deleteCol = md.getString(EntityInfo.METADATA_DELETECOL);
			if ((deleteCol != null) && (deleteCol.length() == 0)) {
				deleteCol = null;
			}
		}
	}
	
	@Override
	public String toString() {
		return "Table: " + table + ", " + sourceCol + " - > " + destCol; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}

	public boolean isComplete() {
		return (table != null) && (sourceCol != null) && (destCol != null);
	}
	
}
