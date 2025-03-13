/*******************************************************************************
 * Copyright (c) 2025 ARCAD Software.
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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map.Entry;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.sql.internal.Activator;
import com.arcadsoftware.osgi.ISODateFormater;

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

	public String table;
	public String idCol;
	public String deleteCol;
	public String colPrefix;
	public String updateCol;
	public String lockCol;
	public String lockDateCol;

	public HashMap<String, String> attributesCols;
	public HashMap<String, LinkInfo> links;
	
	//SQL Cache
	public String sql_harddelete;
	public String sql_delete;
	public String sql_undelete;
	public String sql_subselect;
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
		}
		attributesCols = new HashMap<String, String>();
		for (Entry<String, MetaDataAttribute> e: entity.getAttributes().entrySet()) {
			String col = e.getValue().getMetadata().getString(METADATA_COLNAME);
			if ((col != null) && (col.length() > 0)) {
				attributesCols.put(e.getKey(), parseAttributeColumn(mapper, e.getValue().getType(), col));
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

	/**
	 * Parse complex Attributes column names.
	 * 
	 * @param type
	 * @param col
	 * @return
	 */
	protected String parseAttributeColumn(final MapperSQLService mapper, final String type, String col) {
		int trunc = 0;
		// Truncate long Strings: format is "xxx^l" where xxx may be any operation on columns and l is a integer.
		if (col.indexOf('^') > 0) {
			try {
				trunc = Integer.parseInt(col.substring(col.indexOf('^') + 1).trim());
				col = col.substring(0, col.indexOf('^') - 1).trim();
				if (col.length() == 0) {
					return "'Olume definition Format ERROR !!!'"; //$NON-NLS-1$ 
				}
				if ((col.indexOf('+') < 0) && !(col.charAt(0) == MapperSQLService.COLUMNPREFIX_PLACEHOLDER)) {
					return String.format(mapper.fg.trunc_string, MapperSQLService.COLUMNPREFIX_PLACEHOLDER + col, trunc);
				}
			} catch (NumberFormatException e) {
				Activator.getInstance().debug(e);
			}
		}
		// Concat or sum columns values.
		if (col.indexOf('+') > 0) {
			String concat = mapper.fg.concat;
			char quote = '0';
			if (MetaDataAttribute.TYPE_STRING.equals(type)) {
				concat = mapper.fg.concat_string;
				quote = mapper.fg.quote.charAt(0);
			} else if (MetaDataAttribute.TYPE_DATE.equals(type)) {
				concat = mapper.fg.concat_days;
				quote = 'D';
			}
			String[] cols = col.split("\\+"); //$NON-NLS-1$
			String result = formatConcat(mapper, null, cols[0], concat, quote);
			for (int i = 1; i < (cols.length - 1); i++) {
				result = formatConcat(mapper, result, cols[i], concat, quote);
			}
			col = formatConcat(mapper, result, cols[cols.length - 1], null, quote);
		}
		if (trunc > 0) {
			return String.format(mapper.fg.trunc_string, col, trunc);
		}
		return col;
	}

	private String formatConcat(final MapperSQLService mapper, final String result, final String col, final String concat, final char constant) {
		String c = col.trim();
		if (c.length() == 0) {
			if (result == null) {
				if (concat != null) {
					return "%1$s"; //$NON-NLS-1$
				}
				return ""; //$NON-NLS-1$
			}
			return result;
		}
		// a Constant...
		if (constant == '0') { // Integer constants...
			try {
				Integer.parseInt(c);
				if (concat != null) {
					c = String.format(concat, c, "%1$s"); //$NON-NLS-1$
				}
				if (result != null) {
					return String.format(result, c);
				}
				return c; 
			} catch (NumberFormatException e) {}
		} else if (constant == 'D') { // Date constants...
			if (ISODateFormater.mayIsoDate(c)) {
				try {
					Date d = ISODateFormater.toDate(c);
					c = mapper.fg.quote + new SimpleDateFormat(mapper.fg.dateformat).format(d) + mapper.fg.quote;
					Integer.parseInt(c);
					if (concat != null) {
						c = String.format(concat, c, "%1$s"); //$NON-NLS-1$
					}
					if (result != null) {
						return String.format(result, c);
					}
					return c; 
				} catch (ParseException e) {}
			}
		} else if (c.charAt(0) == constant) {
			if (concat != null) {
				c = String.format(concat, c, "%1$s"); //$NON-NLS-1$
			}
			if (result != null) {
				return String.format(result, c);
			}
			return c; 
		}
		// a Column name...
		if (c.charAt(0) != MapperSQLService.COLUMNPREFIX_PLACEHOLDER) {
			c = MapperSQLService.COLUMNPREFIX_PLACEHOLDER + c;
		}
		if (concat != null) {
			c = String.format(concat, c, "%1$s"); //$NON-NLS-1$
		}
		if (result != null) {
			return String.format(result, c);
		}
		return c; 
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
		s.append("\nAttributes: "); //$NON-NLS-1$
		s.append(attributesCols);
		s.append("\nLinks: "); //$NON-NLS-1$
		s.append(links);
		return s.toString();
	}
	
}