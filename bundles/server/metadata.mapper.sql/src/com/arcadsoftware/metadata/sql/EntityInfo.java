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

import java.util.HashMap;
import java.util.Map.Entry;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;

/**
 * Utility class that allows quick access to information related to the storage of the entity.
 * 
 * <p>
 * This class can also be used to cache pre-generated queries.
 *  
 * Creation Date: 2011-02-17
 */
public class EntityInfo {
	
	/*
	 * Tag de metadata ajoutés aux entitées de ce type de mappers.
	 */
	protected static final String METADATA_TABLE = "table"; //$NON-NLS-1$
	protected static final String METADATA_DELETECOL = "deleteCol"; //$NON-NLS-1$
	protected static final String METADATA_COLNAME = "col"; //$NON-NLS-1$
	protected static final String METADATA_IDCOL = "idCol"; //$NON-NLS-1$
	protected static final String METADATA_SOURCECOL = "sourceCol"; //$NON-NLS-1$
	protected static final String METADATA_DESTCOL = "destCol"; //$NON-NLS-1$
	private static final String METADATA_COLPREFIX = "colPrefix"; //$NON-NLS-1$
	private static final String METADATA_UPDATECOL = "updateCol"; //$NON-NLS-1$
	private static final String METADATA_LOCKCOL = "lockCol"; //$NON-NLS-1$
	private static final String METADATA_LOCKDATECOL = "lockDateCol"; //$NON-NLS-1$
	private static final String METADATA_GROUPTABLE = "groupTable"; //$NON-NLS-1$
	private static final String METADATA_GROUPMINCOL = "groupMinCol"; //$NON-NLS-1$
	private static final String METADATA_GROUPMAXCOL = "groupMaxCol"; //$NON-NLS-1$

	public String table;
	public String idCol;
	public String deleteCol;
	public String colPrefix;
	public String updateCol;
	public String lockCol;
	public String lockDateCol;
	public String groupTable;
	public String groupMinCol;
	public String groupMaxCol;

	public HashMap<String, String> attributesCols;
	public HashMap<String, LinkInfo> links;
	
	//SQL Cache
	public String sql_harddelete;
	public String sql_delete;
	public String sql_undelete;
	public HashMap<String, MultiLinkQuery> sql_links;
	
	public EntityInfo(MetaDataEntity entity, MapperSQLService mapper) {
		super();
		update(entity, mapper);
	}
	
	public boolean isValid() {
		return (idCol != null) && (table != null);
	}
	
	public void update(MetaDataEntity entity, MapperSQLService mapper) {
		sql_harddelete = null;
		sql_delete = null;
		sql_undelete = null;
		// FIXME as this cache depends on other entities it must be cleared when other entities are modified !
		sql_links = new HashMap<>(); 
		BeanMap md = entity.getMetadata();
		if (md != null) {
			idCol = md.getString(METADATA_IDCOL);
			if ((idCol != null) && (idCol.length() == 0)) {
				idCol = null;
			}
			table = md.getString(METADATA_TABLE);
			if ((table != null) && (table.length() == 0)) {
				table = null;
			}
			deleteCol = md.getString(METADATA_DELETECOL);
			if ((deleteCol != null) && (deleteCol.length() == 0)) {
				deleteCol = null;
			}
			colPrefix = md.getString(METADATA_COLPREFIX);
			if ((colPrefix != null) && (colPrefix.length() == 0)) {
				colPrefix = null;
			}
			updateCol = md.getString(METADATA_UPDATECOL);
			if ((updateCol != null) && (updateCol.length() == 0)) {
				updateCol = null;
			}
			lockCol = md.getString(METADATA_LOCKCOL);
			if ((lockCol != null) && (lockCol.length() == 0)) {
				lockCol = null;
			}
			lockDateCol = md.getString(METADATA_LOCKDATECOL);
			if ((lockDateCol != null) && (lockDateCol.length() == 0)) {
				lockDateCol = null;
			}
			groupTable = md.getString(METADATA_GROUPTABLE);
			if ((groupTable != null) && (groupTable.length() == 0)) {
				groupTable = null;
			}
			groupMinCol = md.getString(METADATA_GROUPMINCOL);
			if ((groupMinCol != null) && (groupMinCol.length() == 0)) {
				groupMinCol = null;
			}
			groupMaxCol = md.getString(METADATA_GROUPMAXCOL);
			if ((groupMaxCol != null) && (groupMaxCol.length() == 0)) {
				groupMaxCol = null;
			}
		}
		attributesCols = new HashMap<String, String>();
		for (Entry<String, MetaDataAttribute> e: entity.getAttributes().entrySet()) {
			String col = e.getValue().getMetadata().getString(METADATA_COLNAME);
			if ((col != null) && (col.length() > 0)) {
				attributesCols.put(e.getKey(), mapper.parseAttributeColumn(e.getValue().getType(),col));
			}
		}
		links = new HashMap<String, LinkInfo>();
		for (Entry<String, MetaDataLink> e: entity.getLinks().entrySet()) {
			LinkInfo i = new LinkInfo(entity, e.getValue());
			if (i.isComplete()) {
				links.put(e.getKey(), i);
			}
		}
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder();
		s.append("Table: "); //$NON-NLS-1$
		s.append(table);
		s.append("\nIdCol: "); //$NON-NLS-1$
		s.append(idCol);
		s.append("\nDeleteCol: "); //$NON-NLS-1$
		s.append(deleteCol);
		s.append("\nUpdateCol: "); //$NON-NLS-1$
		s.append(updateCol);
		s.append("\nLockCol: "); //$NON-NLS-1$
		s.append(lockCol);
		s.append("\nGroupTable: "); //$NON-NLS-1$
		s.append(groupTable);
		s.append("\nAttributes: "); //$NON-NLS-1$
		s.append(attributesCols);
		s.append("\nLinks: "); //$NON-NLS-1$
		s.append(links);
		return s.toString();
	}
	
}