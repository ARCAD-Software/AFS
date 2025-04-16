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

import java.math.BigInteger;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import javax.sql.DataSource;

import org.apache.commons.dbutils.handlers.ScalarHandler;
import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapPartialList;
import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.dbutils.QueryRunnerEx;
import com.arcadsoftware.metadata.AbstractMapperService;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.criteria.AndCriteria;
import com.arcadsoftware.metadata.criteria.ConstantCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.criteria.IdEqualCriteria;
import com.arcadsoftware.metadata.criteria.InListCriteria;
import com.arcadsoftware.metadata.sql.internal.Activator;
import com.arcadsoftware.metadata.sql.internal.BeanMapHandler;
import com.arcadsoftware.metadata.sql.internal.BeanMapListHandler;
import com.arcadsoftware.metadata.sql.internal.Messages;
import com.arcadsoftware.metadata.sql.internal.Escapes;
import com.arcadsoftware.metadata.sql.internal.Fragments;
import com.arcadsoftware.metadata.sql.internal.PartialBeanMapListHandler;
import com.arcadsoftware.osgi.ISODateFormater;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * This class define an Entity <---> SQL relational Database mapping using the specific metadata 
 * defined in the entities. This <strong>mapper</strong> use a limited/generic SQL syntax which is
 * adapted depending to the targeted database type.
 * 
 * <p>
 * Any mapper provider may extends this class to implement specifics SQL dynamic mapping facilities.  
 * 
 * <p>
 * Creation Date: 2011-2-3
 */
public class MapperSQLService extends AbstractMapperService<SQLCriteriaContext> {
	
	private static final boolean RETURNEMPTYBEAMMAP = Boolean.getBoolean("com.arcadsoftware.mapper.sql.empty.beanmap"); //$NON-NLS-1$
	protected static final char COLUMNPREFIX_PLACEHOLDER = '~';
	protected static final String COLUMNPREFIX_PLACEHOLDERS = "~"; //$NON-NLS-1$
	protected static final String DEFAULT_TABLEALIAS = "x"; //$NON-NLS-1$
	
	private final DataSource ds;
	final Fragments fg;
	final Escapes esc;
	final SimpleDateFormat sdf;
	private volatile String hasRightrecursiveRequest;
	private final ConcurrentHashMap<MetaDataEntity, EntityInfo> infos;
	private final QueryRunnerEx runner;
	private final boolean convertValues;
	
	public MapperSQLService(String domain, DataSource datasource, String dialect) {
		super();
		addDomain(domain);
		convertValues = "postgresql".equalsIgnoreCase(dialect); //$NON-NLS-1$
		ds = datasource;
		fg = new Fragments(dialect);
		esc = new Escapes(dialect);
		infos = new ConcurrentHashMap<MetaDataEntity, EntityInfo>();
		runner = new QueryRunnerEx(ds);
		sdf = new SimpleDateFormat(fg.dateformat);
	}

	/**
	 * @return The associated DataSource
	 */
	public DataSource getDataSource() {
		return ds;
	}
	
	/**
	 * Create or reuse a with SQL clause which can be used to get the list of profiles containing a given right+param.
	 * <ul>
	 * <li>%1$s = is the name of the recursive table of profile ids.
	 * <li>%2$s = is the right value test ("is null" or "= X".
	 * <li>%3$s = is the param value test ("is null" or "= X".
	 * </ul> 
	 * @return
	 */
	protected String generateHashRightPrequery() {
		if (hasRightrecursiveRequest == null) {
			// This code should be synchronized, but there is no problem to generate this request twice...
			MetaDataEntity pre = MetaDataEntity.loadEntity("profileRight"); //$NON-NLS-1$
			if (pre.getMapper() != this) {
				return ((MapperSQLService) pre.getMapper()).generateHashRightPrequery();
			}
			EntityInfo profileRights = getEntityInfo(pre);
			String profileCol = profileRights.attributesCols.get("profile"); //$NON-NLS-1$
			EntityInfo profiles = getEntityInfo(MetaDataEntity.loadEntity("profile")); //$NON-NLS-1$
			if (profiles == null) {
				return null;
			}
			// We return all profiles containing the given right,
			// plus all profiles containing these profiles, recursively.
			LinkInfo parentProfiles = profiles.links.get("parents"); //$NON-NLS-1$
			if (parentProfiles == null) {
				return null;
			}
			hasRightrecursiveRequest = String.format(fg.rec_alt, "%1$s", //$NON-NLS-1$
					String.format(fg.select,
							profileCol, // selected column
							profileRights.table + // table + join on undeleted profiles:
							String.format(fg.join_inner, profiles.table, "p", profiles.idCol, //$NON-NLS-1$ 
									profileCol + fg.and + profiles.deleteCol + fg.equaldelfalse), //
							fg.parin + profileRights.deleteCol + fg.equaldelfalse + fg.parout + fg.and + // where clause: profileRight undeleted and:
							fg.parin + profileRights.attributesCols.get("right") + "%2$s" + fg.parout + fg.and + //$NON-NLS-1$ //$NON-NLS-2$
							fg.parin + profileRights.attributesCols.get("parameter") + "%3$s" + fg.parout), //$NON-NLS-1$ //$NON-NLS-2$
					parentProfiles.table, // from parents link table,
					parentProfiles.destCol, // select the parent,
					parentProfiles.sourceCol, // join to the recursive table, and only if the parent profile is undeleted:
					String.format(fg.join_inner, profiles.table, "p", profiles.idCol, //$NON-NLS-1$ 
							parentProfiles.destCol + fg.and + profiles.deleteCol + fg.equaldelfalse));
		}
		return hasRightrecursiveRequest;
	}
	
	@Override
	protected SQLCriteriaContext getContext(MetaDataEntity entity, IConnectionUserBean currentUser) {
		return new SQLCriteriaContext(this, entity, currentUser);
	}

	/**
	 * Construct the columns list an filter the attributes list according to the updated attributes from the entity.
	 * 
	 * @param e entity information
	 * @param attributes Attributes list
	 * @param cols The generated associated Table columns list
	 * @param values The values to be updated.
	 * @return The updated list of values.
	 */
	protected List<Object> filterValues(EntityInfo e, List<MetaDataAttribute> attributes, List<String> cols, List<Object> values, IConnectionUserBean user) {
		ArrayList<Object> result = new ArrayList<Object>(values.size());
		int index = 0;
		if ((attributes != null) && (attributes.size() > 0)) {
			Iterator<MetaDataAttribute> itt = attributes.iterator();
			while (itt.hasNext()) {
				MetaDataAttribute att = itt.next();
				if (att ==  null) {
					itt.remove();
				} else {
					String col = e.attributesCols.get(att.getCode());
					if ((col == null) || (col.indexOf(COLUMNPREFIX_PLACEHOLDER) >= 0)) {
						// Blindage: Les colonnes composées devrait toujours être déclarées comme readonly.
						itt.remove();
					} else {
						Object value = values.get(index);
						if (value instanceof Date) {
							value = new Timestamp(((Date) value).getTime());
						} else if (value instanceof Calendar) {
							value = new Timestamp(((Calendar) value).getTimeInMillis());
						} else if (value instanceof Boolean) {
							if ((Boolean) value) {
								value = fg.true_val;
							} else {
								value = fg.false_val;
							}
						} else if (isEncrypted(att)) {
							if (value instanceof String) {
								value = Crypto.encrypt(((String) value).toCharArray());
							} else if (value instanceof char[]) {
								value = Crypto.encrypt((char[]) value);
							}
						}
						if (convertValues && (value != null)) {
							if (att.isReference() || //
									MetaDataAttribute.TYPE_INTEGER.equalsIgnoreCase(att.getType()) || //
									MetaDataAttribute.TYPE_INT.equalsIgnoreCase(att.getType())) {
								if (!(value instanceof Integer)) {
									try {
										value = Integer.valueOf(value.toString().trim());
									} catch (Exception ee) {
										Activator.getInstance().error("The attribute " + att.getParent() + "." + att.getCode() + " value can not be converted to Integer: \"" + value + '"');
									}
								}
							} else if (MetaDataAttribute.TYPE_EMAIL.equalsIgnoreCase(att.getType()) || //
									MetaDataAttribute.TYPE_STRING.equalsIgnoreCase(att.getType()) || //
									MetaDataAttribute.TYPE_ICON.equalsIgnoreCase(att.getType()) || //
									MetaDataAttribute.TYPE_URL.equalsIgnoreCase(att.getType())) {
								if (!(value instanceof String)) {
									value = value.toString();
								}
							} else if (MetaDataAttribute.TYPE_BOOLEAN.equalsIgnoreCase(att.getType())) {
								if (!(value instanceof Integer)) {
									if (value instanceof String) {
										if (Boolean.valueOf((String) value)) {
											value = fg.true_val;
										} else {
											value = fg.false_val;
										}
									}
									// Other conversions ?
								}							
							} else if (MetaDataAttribute.TYPE_DATE.equalsIgnoreCase(att.getType())) {
								if (value instanceof Integer) {
									value = new Timestamp((Integer) value);
								} else if (value instanceof Long) {
									value = new Timestamp((Long) value);
								} else {
									String s = value.toString();
									if (ISODateFormater.mayIsoDate(s)) {
										try {
											value = new Timestamp(ISODateFormater.toDate(s).getTime());
										} catch (ParseException ee) {
											Activator.getInstance().error("The attribute " + att.getParent() + "." + att.getCode() + " value can not be converted to TimeStamp: \"" + s + '"');
										}
									} else {
										try {
											value = new Timestamp(sdf.parse(s).getTime());
										} catch (ParseException ee) {
											Activator.getInstance().error("The attribute " + att.getParent() + "." + att.getCode() + " value can not be converted to TimeStamp: \"" + s + '"');
										}
									}
								}
							} else if (MetaDataAttribute.TYPE_FLOAT.equalsIgnoreCase(att.getType())) {
								if (!(value instanceof Float)) {
									try {
										value = Float.valueOf(value.toString().trim());
									} catch (Exception ee) {
										Activator.getInstance().error("The attribute " + att.getParent() + "." + att.getCode() + " value can not be converted to Float: \"" + value + '"');
									}
								}
							} else if (MetaDataAttribute.TYPE_LONG.equalsIgnoreCase(att.getType())) {
								if (!(value instanceof Long)) {
									try {
										value = Long.valueOf(value.toString().trim());
									} catch (Exception ee) {
										Activator.getInstance().error("The attribute " + att.getParent() + "." + att.getCode() + " value can not be converted to Long: \"" + value + '"');
									}
								}
							} else if (MetaDataAttribute.TYPE_BIGINTEGER.equalsIgnoreCase(att.getType())) {
								if (!(value instanceof BigInteger)) {
									try {
										value = new BigInteger(value.toString().trim());
									} catch (Exception ee) {
										Activator.getInstance().error("The attribute " + att.getParent() + "." + att.getCode() + " value can not be converted to BigInteger: \"" + value + '"');
									}
								}
							}
						}
						result.add(value);
						cols.add(col);
					}
				}
				index++;
			}
		}
		if (e.updateCol != null) {
			result.add(new Timestamp(System.currentTimeMillis()));
			cols.add(e.updateCol);
		}
		if (e.muidCol != null) {
			if ((user != null) && (user.getId() > 0)) {
				result.add(user.getId());
			} else {
				result.add(null);
			}
			cols.add(e.muidCol);
		}
		return result;
	}
	
	/**
	 * Generate SQL columns list ready to Update operation.
	 * @param cols
	 * @return
	 */
	protected String listUpdateCols(List<String> cols) {
		StringBuilder s = new StringBuilder();
		for(String col:cols) {
			if (s.length() > 0) {
				s.append(fg.columnsep);
			}
			s.append(col);
			s.append(fg.paramset);	
		}
		return s.toString();
	}

	/**
	 * Generate SQL columns list ready to Insert operation.
	 * 
	 * @param cols
	 * @param valCols
	 * @return
	 */
	protected String listCreateCols(List<String> cols, StringBuilder valCols) {
		StringBuilder s = new StringBuilder();
		for(String col: cols) {
			if (s.length() > 0) {
				s.append(fg.columnsep);
				valCols.append(fg.columnsep);
			}
			s.append(col);
			valCols.append(fg.paramex);	
		}
		return s.toString();
	}

	/**
	 * pretty print of Array... 
	 * @param values
	 * @return
	 */
	protected Object arrayToString(Object[] values) {
		if ((values == null) || (values.length == 0)) {
			return Messages.MapperSQLService_Empty;
		}
		StringBuilder s = new StringBuilder("["); //$NON-NLS-1$
		for(Object o:values) {
			if (s.length() > 1) {
				s.append(", "); //$NON-NLS-1$
			}
			if (o instanceof String) {
				s.append('"');
				s.append(o);
				s.append('"');
			} else {
				s.append(o);
			}
		}
		s.append(']');
		return null;
	}
	
	/**
	 * Run an SQL Update.
	 * 
	 * @param query
	 * @param values
	 * @return
	 */
	protected int update(String query, Object[] values) {
		long t = System.currentTimeMillis();
		try {
			return runner.update(query, values);
		} catch (SQLException e) {
			Activator.getInstance().error(Messages.MapperSQLService_Error_Update + e.getLocalizedMessage(), e);
			Activator.getInstance().debug(String.format(Messages.MapperSQLService_SQLError, query, arrayToString(values)));
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, Messages.MapperSQLService_Error_Update + e.getLocalizedMessage(), e);
		} finally {
			Activator.getInstance().trace(query, t);
		} 
	}

	/**
	 * Run an SQL Insert.
	 * 
	 * @param query
	 * @param values
	 * @param idCol
	 * @return
	 */
	protected int insert(String query, Object[] values, String idCol) {
		long t = System.currentTimeMillis();
		try {
			return runner.insert(query, values, idCol);
		} catch (SQLException e) {
			Activator.getInstance().error(Messages.MapperSQLService_Error_Insert + e.getLocalizedMessage(), e);
			Activator.getInstance().debug(String.format(Messages.MapperSQLService_SQLError, query, arrayToString(values)));
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, Messages.MapperSQLService_Error_Insert + e.getLocalizedMessage(), e);
		} finally {
			Activator.getInstance().trace(query, t);
		} 
	}

	/**
	 * Run an SQL count selection.
	 * 
	 * @param query
	 * @param values
	 * @return
	 */
	protected int count(String query, Object[] values) {
		long t = System.currentTimeMillis();
		try {
			Object o = runner.query(query, new ScalarHandler<Object>(), values);
			if (o == null) {
				return 0;
			}
			if (o instanceof Integer) {
				return (Integer)o;
			} 
			if (o instanceof Long) {
				return Math.round((Long)o);
			} 
			try {
				return Integer.valueOf(o.toString());
			} catch (NumberFormatException e) {
				return -1;
			}
		} catch (SQLException e) {
			Activator.getInstance().error(Messages.MapperSQLService_Error_Count + e.getLocalizedMessage(), e);
			Activator.getInstance().debug(String.format(Messages.MapperSQLService_SQLError, query, arrayToString(values)));
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, Messages.MapperSQLService_Error_Count + e.getLocalizedMessage(), e);
		} finally {
			Activator.getInstance().trace(query, t);
		} 
	}
	
	/**
	 * Run an SQL item selection.
	 * 
	 * @param query
	 * @param type
	 * @param values
	 * @return
	 */
	protected BeanMap query(String query, String type, Object[] values) {
		long t = System.currentTimeMillis();
		try {
			return (BeanMap) runner.query(query, new BeanMapHandler(type, RETURNEMPTYBEAMMAP), values);
		} catch (SQLException e) {
			Activator.getInstance().error(Messages.MapperSQLService_ItemSelection + e.getLocalizedMessage(), e);
			Activator.getInstance().debug(String.format(Messages.MapperSQLService_SQLError, query, arrayToString(values)));
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, Messages.MapperSQLService_Error_ItemSelection + e.getLocalizedMessage(), e);
		} finally {
			Activator.getInstance().trace(query, t);
		}
	}

	/**
	 * Run an SQL selection.
	 * 
	 * @param query
	 * @param type
	 * @param list
	 * @param values
	 * @param offset
	 * @param limit
	 * @return
	 */
	protected BeanMapList query(String query, String type, BeanMapList list, Object[] values, int offset, int limit) {
		if ((limit < 0) && (offset == 0)) {
			return query(query, type, list, values);
		}
		long t = System.currentTimeMillis();
		try {
			return (BeanMapList) runner.query(query, new PartialBeanMapListHandler(type, list, offset, limit), values);
		} catch (SQLException e) {
			Activator.getInstance().error(Messages.MapperSQLService_Error_Selection + e.getLocalizedMessage(), e);
			Activator.getInstance().debug(String.format(Messages.MapperSQLService_SQLError, query, arrayToString(values)));
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, Messages.MapperSQLService_Error_Selection + e.getLocalizedMessage(), e);
		} finally {
			Activator.getInstance().trace(query, t);
		}
	}

	/**
	 * Run an SQL selection.
	 * 
	 * @param query
	 * @param type
	 * @param list
	 * @param values
	 * @return
	 */
	protected BeanMapList query(String query, String type, BeanMapList list, Object[] values) {
		long t = System.currentTimeMillis();
		try {
			return (BeanMapList) runner.query(query, new BeanMapListHandler(type, list), values);
		} catch (SQLException e) {
			Activator.getInstance().error(Messages.MapperSQLService_Error_Selection + e.getLocalizedMessage(), e);
			Activator.getInstance().debug(String.format(Messages.MapperSQLService_SQLError, query, arrayToString(values)));
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, Messages.MapperSQLService_Error_Count + e.getLocalizedMessage(), e);
		} finally {
			Activator.getInstance().trace(query, t);
		}
	}

	/**
	 * Get the Database related information and SQL template pregenerated for the given Entity.
	 *  
	 * @param entity
	 * @return
	 */
	protected EntityInfo getEntityInfo(MetaDataEntity entity) {
		EntityInfo info = infos.get(entity);
		if (info == null) {
			if ((entity == null) || !sameDomain(entity)) {
				return null;
			}
			info = new EntityInfo(entity, this);
			if (info.table == null) {
				return null;
			}
			infos.put(entity, info);
		}
		return info;
	}

	/**
	 * Remove cached entity info.
	 * 
	 * @param entity
	 */
	public void purgeCache(MetaDataEntity entity) {
		infos.remove(entity);
		if ("profile".equals(entity.getType()) || "profileRight".equals(entity.getType())) { //$NON-NLS-1$ //$NON-NLS-2$
			hasRightrecursiveRequest = null;
		}
	}
	
	@Override
	public boolean delete(MetaDataEntity entity, int itemId, boolean hardDelete, IConnectionUserBean currentUser) {
		EntityInfo e = getEntityInfo(entity);
		if (e == null) {
			return false;
		}
		if (hardDelete || (e.deleteCol == null)) {
			if (e.sql_harddelete == null) {
				e.sql_harddelete = String.format(fg.delete_hard, e.table, e.idCol); 
			}
			return update(e.sql_harddelete, new Object[] {itemId}) > 0;
		}
		final Object[] vals;
		if (e.updateCol == null) {
			if (e.muidCol == null) {
				vals = new Object[] {itemId};
				if (e.sql_delete == null) {
					e.sql_delete = String.format(fg.delete, e.table, e.deleteCol, e.idCol);
				}
			} else {
				vals = new Object[] {null, itemId};
				if ((currentUser != null) && (currentUser.getId() > 0)) {
					vals[0] = currentUser.getId();
				}
				if (e.sql_delete == null) {
					e.sql_delete = String.format(fg.delete_update, e.table, e.deleteCol, e.idCol, e.muidCol);
				}
			}
		} else if (e.muidCol == null) {
			vals = new Object[] {new Timestamp(System.currentTimeMillis()), itemId};
			if (e.sql_delete == null) {
				e.sql_delete = String.format(fg.delete_update, e.table, e.deleteCol, e.idCol, e.updateCol);
			}				
		} else {
			vals = new Object[] {new Timestamp(System.currentTimeMillis()), null, itemId};
			if ((currentUser != null) && (currentUser.getId() > 0)) {
				vals[1] = currentUser.getId();
			}
			if (e.sql_delete == null) {
				e.sql_delete = String.format(fg.delete_update2, e.table, e.deleteCol, e.idCol, e.updateCol, e.muidCol);
			}				
		}
		return update(e.sql_delete, vals) > 0;
	}

	@Override
	public int delete(MetaDataEntity entity, ISearchCriteria criteria, IConnectionUserBean currentUser, boolean hardDelete) {
		final SQLCriteriaContext context = getContext(entity, currentUser);
		if (!context.isValid()) {
			return 0;
		}
		final EntityInfo e = context.getEntityInfo();
		criteria = criteria.reduce(context);
		if (ConstantCriteria.FALSE.equals(criteria)) {
			return 0;
		}
		HashMap<String, String> colNames = new HashMap<String, String>();
		if (hardDelete || (e.deleteCol == null)) {
			// Generate where clause before the joins...
			String where = context.generateCriteria(criteria, true).toString();
			return update(String.format(fg.delete_hardex, context.generateJoins(true), where), new Object[0]);
		}
		// An SQL limitation may prevent the usage of joins in updates...
		final Boolean hasjoins = context.hasReferences();
		if (fg.update_join.isEmpty() && hasjoins) {
			// we abandon the update by single request, the criterion involves other elements...
			// We have to use a pre-selection of the updated items.
			BeanMapList list = doSelection(new ArrayList<ReferenceLine>(), false, criteria, false, null, 0, -1, context);
			Activator.getInstance().debug(String.format(Messages.MapperSQLService_Info_MultiUpdateWithComplexCriteria,list.size(),context.getEntity().getType()));
			int result = 0;
			final Object[] vals;
			if (e.updateCol == null) {
				if (e.muidCol == null) {
					vals = new Object[] {0};
					if (e.sql_delete == null) {
						e.sql_delete = String.format(fg.delete, e.table, e.deleteCol, e.idCol);
					}
				} else {
					vals = new Object[] {null, 0};
					if ((currentUser != null) && (currentUser.getId() > 0)) {
						vals[0] = currentUser.getId();
					}
					if (e.sql_delete == null) {
						e.sql_delete = String.format(fg.delete_update, e.table, e.deleteCol, e.idCol, e.muidCol);
					}
				}
			} else if (e.muidCol == null) {
				vals = new Object[] {new Timestamp(System.currentTimeMillis()), 0};
				if (e.sql_delete == null) {
					e.sql_delete = String.format(fg.delete_update, e.table, e.deleteCol, e.idCol, e.updateCol);
				}
			} else {
				vals = new Object[] {new Timestamp(System.currentTimeMillis()), null, 0};
				if ((currentUser != null) && (currentUser.getId() > 0)) {
					vals[1] = currentUser.getId();
				}
				if (e.sql_delete == null) {
					e.sql_delete = String.format(fg.delete_update2, e.table, e.deleteCol, e.idCol, e.updateCol, e.muidCol);
				}
			}
			for (BeanMap bean: list) {
				vals[vals.length - 1] = bean.getId();
				result += update(e.sql_delete, vals);
			}
			return result;
		}
		final ArrayList<Object> vals = new ArrayList<>();
		vals.add(fg.delete_val);
		final StringBuilder lcols = new StringBuilder(DEFAULT_TABLEALIAS); 
		lcols.append('.');
		lcols.append(e.deleteCol);
		lcols.append(fg.paramset);
		if (e.updateCol != null) {
			vals.add(new Timestamp(System.currentTimeMillis()));
			lcols.append(fg.columnsep);
			lcols.append(DEFAULT_TABLEALIAS);
			lcols.append('.');
			lcols.append(e.updateCol);
			lcols.append(fg.paramset);
		}
		if (e.muidCol != null) {
			if ((currentUser != null) && (currentUser.getId() > 0)) {
				vals.add(currentUser.getId());
			} else {
				vals.add(null);
			}
			lcols.append(fg.columnsep);
			lcols.append(DEFAULT_TABLEALIAS);
			lcols.append('.');
			lcols.append(e.muidCol);
			lcols.append(fg.paramset);
		}
		if (criteria == null) {
			return update(String.format(fg.updateex, e.table, DEFAULT_TABLEALIAS, lcols.toString(), fg.true_cond), vals.toArray(new Object[0]));
		}
		if (hasjoins) {
			// Generate where clause before the joins...
			StringBuilder where = context.generateCriteria(criteria, true);
			return update(String.format(fg.update_join, DEFAULT_TABLEALIAS, lcols.toString(), context.generateJoins(true), where.toString()), vals.toArray(new Object[0]));
		}
		for (ReferenceLine ref: context.getReferences()) {
			String code = ref.getCode();
			String col = e.attributesCols.get(code);
			if (col != null) {
				if (col.contains(COLUMNPREFIX_PLACEHOLDERS)) {
					col = col.replace(COLUMNPREFIX_PLACEHOLDERS, ""); //$NON-NLS-1$
				}
				colNames.put(code, col);
			}
		}
		return update(String.format(fg.updateex, e.table, DEFAULT_TABLEALIAS, lcols.toString(), context.generateCriteria(criteria, true).toString()), vals.toArray(new Object[0]));
	}

	@Override
	public int undelete(MetaDataEntity entity, ISearchCriteria criteria, IConnectionUserBean currentUser) {
		EntityInfo e = getEntityInfo(entity);
		if ((e == null) || (e.deleteCol == null)) {
			return 0;
		}
		SQLCriteriaContext context = getContext(entity, currentUser);
		criteria = criteria.reduce(context);
		if (ConstantCriteria.FALSE.equals(criteria)) {
			return 0;
		}
		// An SQL limitation may prevent the usage of joins in updates...
		final Boolean hasjoins = context.hasReferences();
		if (fg.update_join.isEmpty() && hasjoins) {
			// on abandonne l'update par requête unique, le critère fait intervenir d'autres éléments.
			// Il faut passer par une présélection
			BeanMapList list = doSelection(new ArrayList<ReferenceLine>(), true, criteria, false, null, 0, -1, context);
			Activator.getInstance().debug(String.format(Messages.MapperSQLService_Info_MultiUpdateWithComplexCriteria,list.size(),context.getEntity().getType()));
			int result = 0;
			final Object[] vals;
			if (e.updateCol == null) {
				if (e.muidCol == null) {
					vals = new Object[] {0};
					if (e.sql_undelete == null) {
						e.sql_undelete = String.format(fg.undelete, e.table, e.deleteCol, e.idCol);
					}
				} else {
					vals = new Object[] {null, 0};
					if ((currentUser != null) && (currentUser.getId() > 0)) {
						vals[0] = currentUser.getId();
					}
					if (e.sql_undelete == null) {
						e.sql_undelete = String.format(fg.undelete_update, e.table, e.deleteCol, e.idCol, e.muidCol);
					}
				}
			} else if (e.muidCol == null) {
				vals = new Object[] {new Timestamp(System.currentTimeMillis()), 0};
				if (e.sql_undelete == null) {
					e.sql_undelete = String.format(fg.undelete_update, e.table, e.deleteCol, e.idCol, e.updateCol);
				}
			} else {
				vals = new Object[] {new Timestamp(System.currentTimeMillis()), null, 0};
				if ((currentUser != null) && (currentUser.getId() > 0)) {
					vals[1] = currentUser.getId();
				}
				if (e.sql_undelete == null) {
					e.sql_undelete = String.format(fg.undelete_update2, e.table, e.deleteCol, e.idCol, e.updateCol, e.muidCol);
				}
			}
			for (BeanMap bean: list) {
				vals[vals.length - 1] = bean.getId();
				result += update(e.sql_undelete, vals);
			}
			return result;
		}
		final ArrayList<Object> vals = new ArrayList<>();
		vals.add(fg.undelete_val);
		final StringBuilder lcols = new StringBuilder(DEFAULT_TABLEALIAS);
		lcols.append('.');
		lcols.append(e.deleteCol);
		lcols.append(fg.paramset);
		if (e.updateCol != null) {
			vals.add(new Timestamp(System.currentTimeMillis()));
			lcols.append(fg.columnsep);
			lcols.append(DEFAULT_TABLEALIAS);
			lcols.append('.');
			lcols.append(e.updateCol);
			lcols.append(fg.paramset);
		}
		if ((e.muidCol != null) && (currentUser != null) && (currentUser.getId() > 0)) {
			vals.add(currentUser.getId());
			lcols.append(fg.columnsep);
			lcols.append(DEFAULT_TABLEALIAS);
			lcols.append('.');
			lcols.append(e.muidCol);
			lcols.append(fg.paramset);
		}
		if (criteria == null) {
			return update(String.format(fg.updateex, e.table, DEFAULT_TABLEALIAS, lcols.toString(), fg.true_cond), vals.toArray(new Object[0]));
		}
		StringBuilder where = context.generateCriteria(criteria, true);
		if (hasjoins) {
			return update(String.format(fg.update_join, DEFAULT_TABLEALIAS, lcols.toString(), context.generateJoins(true), where.toString()), vals.toArray(new Object[0]));
		}
		return update(String.format(fg.updateex, e.table, DEFAULT_TABLEALIAS, lcols.toString(), where.toString()), vals.toArray(new Object[0]));
	}

	@Override
	public boolean undelete(MetaDataEntity entity, int itemId, IConnectionUserBean currentUser) {
		EntityInfo e = getEntityInfo(entity);
		if ((e == null) || (e.deleteCol == null)) {
			return false;
		}
		final Object[] vals;
		if (e.updateCol == null) {
			if (e.muidCol == null) {
				vals = new Object[] {itemId};
				if (e.sql_undelete == null) {
					e.sql_undelete = String.format(fg.undelete, e.table, e.deleteCol, e.idCol);
				}
			} else {
				vals = new Object[] {null, itemId};
				if ((currentUser != null) && (currentUser.getId() > 0)) {
					vals[0] = currentUser.getId();
				}
				if (e.sql_undelete == null) {
					e.sql_undelete = String.format(fg.undelete_update, e.table, e.deleteCol, e.idCol, e.muidCol);
				}
			}
		} else if (e.muidCol == null) {
			vals = new Object[] {new Timestamp(System.currentTimeMillis()), itemId};
			if (e.sql_undelete == null) {
				e.sql_undelete = String.format(fg.undelete_update, e.table, e.deleteCol, e.idCol, e.updateCol);
			}
		} else {
			vals = new Object[] {new Timestamp(System.currentTimeMillis()), null, itemId};
			if ((currentUser != null) && (currentUser.getId() > 0)) {
				vals[1] = currentUser.getId();
			}
			if (e.sql_undelete == null) {
				e.sql_undelete = String.format(fg.undelete_update2, e.table, e.deleteCol, e.idCol, e.updateCol, e.muidCol);
			}
		}
		return update(e.sql_undelete, vals) > 0;
	}

	@Override
	public boolean update(MetaDataEntity entity, int itemId, List<MetaDataAttribute> attributes, List<Object> values, IConnectionUserBean currentUser) {
		EntityInfo e = getEntityInfo(entity);
		if (e == null) {
			return false;
		}
		ArrayList<String> cols = new ArrayList<String>();
		values = filterValues(e, attributes, cols, values, currentUser);
		if (values.size() == 0) {
			return false;
		}
		values.add(itemId);
		return update(String.format(fg.update, e.table, listUpdateCols(cols), e.idCol),values.toArray(new Object[values.size()])) > 0;
	}

	@Override
	protected boolean doUpdate(List<MetaDataAttribute> attributes, List<Object> values, ISearchCriteria criteria, SQLCriteriaContext context) {
		if (!context.isValid()) {
			return false;
		}
		EntityInfo e = context.getEntityInfo();
		ArrayList<String> cols = new ArrayList<String>();
		values = filterValues(e, attributes, cols, values, context.getCurrentUser());
		if (values.size() == 0) {
			return false;
		}
		// An SQL limitation may prevent the usage of joins in updates...
		final Boolean hasjoins = context.hasReferences();
		String lcols = listUpdateCols(cols);
		if ((fg.update_join.isEmpty() && hasjoins)) {
			// on abandonne l'update par requête unique, le critère fait intervenir d'autres éléments.
			// Il faut passer par une présélection
			BeanMapList list = doSelection(new ArrayList<ReferenceLine>(), false, criteria, false, null, 0, -1, context);
			Activator.getInstance().debug(String.format(Messages.MapperSQLService_Info_MultiUpdateWithComplexCriteria,list.size(),context.getEntity().getType()));
			int result = 0;
			values.add(null); // prépare la place de l'id !
			Object[] vals = values.toArray(new Object[values.size()]);
			for (BeanMap bean:list) {
				vals[vals.length - 1] = bean.getId();
				result += update(String.format(fg.update, e.table, lcols, e.idCol), vals);
			}
			return result > 0;
		}
		StringBuilder where = context.generateCriteria(criteria, false);
		if (where.isEmpty()) {
			return update(String.format(fg.updateex, e.table, DEFAULT_TABLEALIAS, lcols, fg.true_cond), values.toArray(new Object[values.size()])) > 0;
		}
		if (hasjoins) {
			return update(String.format(fg.update_join, DEFAULT_TABLEALIAS, lcols, context.generateJoins(false), where.toString()), //
					values.toArray(new Object[values.size()])) > 0;
		}
		// Joins map is not used here (the criteria reduction does not own any joins... it is only used to avoid NPE.
		return update(String.format(fg.updateex, e.table, DEFAULT_TABLEALIAS, lcols, where.toString()), values.toArray(new Object[values.size()])) > 0;
	}

	@Override
	public boolean doLinkAdd(MetaDataLink link, int sourceId, int destId) {
		EntityInfo e = getEntityInfo(link.getParent());
		if (e == null) {
			return false;
		}
		LinkInfo l = e.links.get(link.getCode());
		if ((l == null) ||!l.isComplete()) {
			return false;
		}
		if (link.isRecursive()) {
			// Test if this new element does not create a cycle...
			if (l.sql_rectest == null) {
				String rec_alias = "r_" + link.getCode(); //$NON-NLS-1$
				if (l.sql_rec == null) {
					l.sql_rec = fg.rec_first + String.format(fg.rec, rec_alias, l.table, l.destCol, l.sourceCol);
				}
				l.sql_rectest = l.sql_rec + ' ' +
						String.format(fg.select, "*", rec_alias, rec_alias + ".r = ?"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			if (count(l.sql_rectest, new Object[] {destId, sourceId}) > 0) {
				return false;
			}
		}
		if (l.sql_add == null) {
			l.sql_add = String.format(fg.insert_hard, l.table, l.sourceCol + fg.columnsep + l.destCol, fg.param2);
		}
		return update(l.sql_add,new Object[] {sourceId, destId}) > 0;
	}

	@Override
	protected boolean doLinkRemove(MetaDataLink link, int sourceId, int destId) {
		EntityInfo e = getEntityInfo(link.getParent());
		if (e == null) {
			return false;
		}
		List<MetaDataLink> links = link.getLinkChain();
		if ((links == null) || (links.size() > 1)) {
			return false;
		}
		LinkInfo l = e.links.get(link.getCode());
		if (l == null) {
			return false;
		}
		if (l.sql_delete == null) {
			StringBuilder clause = new StringBuilder(l.sourceCol);
			clause.append(fg.paramset);
			clause.append(fg.and);
			clause.append(l.destCol);
			clause.append(fg.paramset);
			if (l.deleteCol == null) {
				l.sql_delete = String.format(fg.delete_hardex, l.table, clause.toString());
			} else {
				l.sql_delete = String.format(fg.deleteex, l.table, l.deleteCol, clause.toString());
			}
		}
		return update(l.sql_delete, new Object[] {sourceId, destId}) > 0;
	}

	@Override
	public BeanMap create(MetaDataEntity entity, List<MetaDataAttribute> attributes, List<Object> values, IConnectionUserBean currentUser) {
		EntityInfo e = getEntityInfo(entity);
		if (e == null) {
			return null;
		}
		ArrayList<String> cols = new ArrayList<String>();
		values = filterValues(e, attributes, cols, values, currentUser);
		if (e.deleteCol != null) {
			cols.add(e.deleteCol);
			values.add(fg.undelete_val);
		}
		StringBuilder valCols = new StringBuilder();
		return selection(entity, insert(String.format(fg.create, e.table, e.idCol, listCreateCols(cols, valCols), valCols.toString()), //
				values.toArray(new Object[values.size()]), e.idCol), entity.getAllAttributes(), true);
	}
	
	@Override
	public BeanMap selection(MetaDataEntity entity, int itemId, List<ReferenceLine> attributes, boolean deleted) {
		SQLCriteriaContext context = getContext(entity, null);
		if (!context.isValid()) {
			return null;
		}
		if (attributes == null) {
			attributes = entity.getAllAttributes();
		}
		StringBuilder cols = context.generateColumns(attributes, deleted);
		StringBuilder where = context.generateCriteria(new IdEqualCriteria(itemId), deleted);
		return completeForeignAttributes(attributes, query(String.format(fg.select, cols.toString(), context.generateJoins(deleted), where.toString()), entity.getType(), null));
	}

	@Override
	protected BeanMapList doSelection(List<ReferenceLine> attributes, boolean deleted, ISearchCriteria criteria,
			boolean distinct, List<ReferenceLine> orders, int page, int limit, SQLCriteriaContext context) {
		if (!context.isValid()) {
			return new BeanMapPartialList();
		}
		StringBuilder cols = context.generateColumns(attributes, deleted);
		if (distinct) {
			cols.insert(0, fg.distinct);
		}
		String orderCols = context.generateOrders(orders, deleted);
		StringBuilder where = context.generateCriteria(criteria, deleted);
		String query;
		if (((page == 0) && (limit <= 0)) || (fg.partial == null) || (fg.partial.length() == 0)) {
			// Pas de pagination (pas nécessaire ou not supporté par le SGDB).
			if (where.length() == 0) {
				if (orderCols.length() == 0) {
					query = ((SQLCriteriaContext) context).formatQuery(fg.selectall, cols.toString(), context.generateJoins(deleted));
				} else {
					query = ((SQLCriteriaContext) context).formatQuery(fg.selectallorder, cols.toString(), context.generateJoins(deleted), orderCols);
				}
			} else if (orderCols.length() == 0) {
				query = ((SQLCriteriaContext) context).formatQuery(fg.select, cols.toString(), context.generateJoins(deleted), where.toString());
			} else {
				query = ((SQLCriteriaContext) context).formatQuery(fg.selectorder, cols.toString(), context.generateJoins(deleted), where.toString(), orderCols);
			}
		} else {
			if (where.length() == 0) {
				if (orderCols.length() == 0) {
					query = ((SQLCriteriaContext) context).formatQuery(fg.partialall, cols.toString(), context.generateJoins(deleted), //
							page, // first element to return
							limit, // number of element to return
							page + limit + 1, // first element to not return.
							context.getEntityInfo().idCol);
				} else {
					query = ((SQLCriteriaContext) context).formatQuery(fg.partialallorder, cols.toString(), context.generateJoins(deleted), //
							page, // first element to return
							limit, // number of element to return
							page + limit + 1, // first element to not return.
							orderCols.toString(), context.getEntityInfo().idCol);
				}
			} else if (orderCols.length() == 0) {
				query = ((SQLCriteriaContext) context).formatQuery(fg.partial, cols.toString(), context.generateJoins(deleted), where.toString(), //
				page, // first element to return
				limit, // number of element to return
				page + limit + 1, // first element to not return.
				context.getEntityInfo().idCol);
			} else {
				query = ((SQLCriteriaContext) context).formatQuery(fg.partialorder, cols.toString(), context.generateJoins(deleted), where.toString(), //
						page, // first element to return
						limit, // number of element to return
						page + limit + 1, // first element to not return.
						orderCols.toString(), context.getEntityInfo().idCol);
			}
		}
		BeanMapList result;
		if (limit == -1) {
			result = new BeanMapPartialList();
		} else {
			result = new BeanMapPartialList(limit);
		}
		((BeanMapPartialList) result).setRank(page);
		if (((page == 0) && (limit <= 0)) || ((fg.partial != null) && (fg.partial.length() > 0))) {
			result = query(query, context.getEntity().getType(), result, null);
		} else {
			result = query(query, context.getEntity().getType(), result, null, page, limit);
		}
		return completeForeignAttributes(attributes, result);
	}

	@Override
	protected BeanMap doSelectionFirst(List<ReferenceLine> attributes, boolean deleted, ISearchCriteria criteria,
			SQLCriteriaContext context) {
		if (!context.isValid()) {
			return null;
		}
		final StringBuilder cols = context.generateColumns(attributes, deleted);
		final StringBuilder where = context.generateCriteria(criteria, deleted);
		final String query;
		if (where.length() == 0) {
			query = ((SQLCriteriaContext) context).formatQuery(fg.selectall, cols.toString(), context.generateJoins(deleted));
		} else {
			query = ((SQLCriteriaContext) context).formatQuery(fg.select, cols.toString(), context.generateJoins(deleted), where.toString());
		}
		return completeForeignAttributes(attributes, query(query, context.getEntity().getType(), null));
	}

	@Override
	protected int doCount(boolean deleted, ISearchCriteria criteria, boolean distinct, SQLCriteriaContext context) {
		if (!context.isValid()) {
			return 0;
		}
		// utilisation de distinct
		String col;
		if (distinct) {
			col = String.format(fg.count_distinct, DEFAULT_TABLEALIAS + '.' + context.getEntityInfo().idCol);
		} else {
			col = fg.count;
		}
		StringBuilder where = context.generateCriteria(criteria, deleted);
		if (where.isEmpty()) {
			return count(((SQLCriteriaContext) context).formatQuery(fg.selectall, col, context.generateJoins(deleted)), null);
		}
		return count(((SQLCriteriaContext) context).formatQuery(fg.select, col, context.generateJoins(deleted), where.toString()), null);
	}

	@Override
	protected BeanMapList doLinkSelection(List<MetaDataLink> links, int sourceId, List<ReferenceLine> attributes,
			boolean deleted, ISearchCriteria criteria, boolean distinct, boolean ignoreSubdivision, 
			List<ReferenceLine> orders, int page, int limit, SQLCriteriaContext context) {
		EntityInfo ei = getEntityInfo(links.get(0).getParent());
		if (ei == null) {
			return null;
		}
		final String mlqc = MultiLinkQuery.getCode(links, deleted, ignoreSubdivision);
		MultiLinkQuery mlq = ei.sql_links.get(mlqc);
		if (mlq == null) {
			mlq = MultiLinkQuery.generate(this, links, deleted, ignoreSubdivision);
			if (mlq == null) {
				return new BeanMapList();
			}
			ei.sql_links.put(mlqc, mlq);
		}
		if (mlq.rec_alias != null) {
			context.addQueryContext(mlq.rec_alias, mlq.rec_query);
		}
		final MetaDataEntity targetEntity = context.getEntity();
		if (targetEntity == null) {
			return new BeanMapPartialList();
		}
		final StringBuilder cols;
		final StringBuilder where;
		final String orderCols;
		final EntityInfo tei = getEntityInfo(targetEntity); 
		JoinElement join = context.initJoinTree(mlq.linkAlias, mlq.join, tei, mlq.linkCol, deleted);
		if (tei == null) {
			// Just get the list of ID to pass to the foreign mapper...
			cols = new StringBuilder();
			cols.append(mlq.linkAlias);
			cols.append('.');
			cols.append(mlq.linkCol);
			cols.append(fg.asid);
			where = new StringBuilder(mlq.where);
			orderCols = ""; //$NON-NLS-1$
		} else {
			cols = context.generateColumns(tei, join, attributes, deleted);
			orderCols = context.generateOrders(orders, deleted);
			where = context.generateCriteria(criteria, deleted);
			if (!mlq.where.isEmpty()) {
				if (!where.isEmpty()) {
					where.append(fg.and);
				}
				where.append(mlq.where);
			}
		}
		if (distinct) {
			cols.insert(0, fg.distinct);
		}
		String query;
		boolean softPagination = false;
		if (((page == 0) && (limit <= 0)) || (fg.partial == null) || (fg.partial.length() == 0) || (tei == null)) {
			softPagination = (fg.partial == null) || (fg.partial.length() == 0);
			// Pas de pagination (pas nécessaire ou non supporté par le SGDB).
			if (where.isEmpty()) {
				if (orderCols.isEmpty()) {
					query = ((SQLCriteriaContext) context).formatQuery(fg.selectall, cols.toString(), context.generateJoins(deleted));
				} else {
					query = ((SQLCriteriaContext) context).formatQuery(fg.selectallorder, cols.toString(), context.generateJoins(deleted), orderCols);
				}
			} else if (orderCols.isEmpty()) {
				query = ((SQLCriteriaContext) context).formatQuery(fg.select, cols.toString(), context.generateJoins(deleted), where.toString());
			} else {
				query = ((SQLCriteriaContext) context).formatQuery(fg.selectorder, cols.toString(), context.generateJoins(deleted), where.toString(), orderCols);
			}
		} else if (where.isEmpty()) {
			if (orderCols.isEmpty()) {
				query = ((SQLCriteriaContext) context).formatQuery(fg.partialall, cols.toString(), context.generateJoins(deleted), //
						page, // first element to return
						limit, // number of element to return
						page + limit + 1, // first element to not return.
						tei.idCol);
			} else {
				query = ((SQLCriteriaContext) context).formatQuery(fg.partialallorder, cols.toString(), context.generateJoins(deleted), //
						page, // first element to return
						limit, // number of element to return
						page + limit + 1, // first element to not return.
						orderCols, tei.idCol);
			}
		} else if (orderCols.isEmpty()) {
			query = ((SQLCriteriaContext) context).formatQuery(fg.partial, cols.toString(), context.generateJoins(deleted), where.toString(), //
			page, // first element to return
			limit, // number of element to return
			page + limit + 1, // first element to not return.
			tei.idCol);
		} else {
			query = ((SQLCriteriaContext) context).formatQuery(fg.partialorder, cols.toString(), context.generateJoins(deleted), where.toString(), //
					page, // first element to return
					limit, // number of element to return
					page + limit + 1, // first element to not return.
					orderCols, tei.idCol);
		}
		BeanMapList result;
		if (limit < 0) {
			result = new BeanMapPartialList();
		} else {
			result = new BeanMapPartialList(limit);
		}
		if (softPagination) {
			result = query(query, context.getEntity().getType(), result, new Object[] {sourceId}, page, limit);
		} else {
			result = query(query, context.getEntity().getType(), result, new Object[] {sourceId});
		}
		// Post treatment of foreign elements:
		if (tei != null) {
			// Result list completed, add the foreigns attributes...
			return completeForeignAttributes(attributes, result);
		}
		// select the foreign entity using the list of ID...
		if (result.size() == 0) {
			return result;
		}
		return context.getEntity().getMapper().selection(attributes, //
				deleted, //
				new AndCriteria(new InListCriteria(result), criteria), //
				distinct, //
				orders, //
				context.getCurrentUser(), //
				0, -1);
	}


	@Override
	public boolean linkTest(List<MetaDataLink> links, int sourceId, int destId, boolean ignoreSubdivision) {
		// This method (and all "link tests") assume that soft-deleted items are not linked.
		if ((links == null) || links.isEmpty()) {
			return false;
		}
		EntityInfo ei = getEntityInfo(links.get(0).getParent());
		if (ei == null) {
			return false;
		}
		final String mlqc = MultiLinkQuery.getCode(links, false, ignoreSubdivision);
		MultiLinkQuery mlq = ei.sql_links.get(mlqc);
		if (mlq == null) {
			mlq = MultiLinkQuery.generate(this, links, false, ignoreSubdivision);
			if (mlq == null) {
				return false;
			}
			ei.sql_links.put(mlqc, mlq);
		}
		final StringBuilder joins = new StringBuilder(mlq.join);
		// Take into account soft deletion of the target entity... only if this entity depends on this mapper.
		ei = getEntityInfo(links.get(links.size() - 1).getRefEntity());
		if ((ei != null) && (ei.deleteCol != null)) {
			joins.append(String.format(fg.join_inner, ei.table, DEFAULT_TABLEALIAS, ei.idCol, mlq.linkAlias + '.' + mlq.linkCol));
			joins.append(fg.and);
			joins.append(DEFAULT_TABLEALIAS);
			joins.append('.');
			joins.append(ei.deleteCol);
			joins.append(fg.equaldelfalse);
		}
		// Add the final test on the last link target column.
		final StringBuilder where = new StringBuilder(mlq.where);
		if (!where.isEmpty()) {
			where.append(fg.and);
		}
		where.append(mlq.linkAlias);
		where.append('.');
		where.append(mlq.linkCol);
		where.append(fg.paramequal);
		final StringBuilder query = new StringBuilder(); 
		if (mlq.rec_alias != null) {
			query.append(mlq.rec_query);
		}
		query.append(String.format(fg.select, fg.count, joins.toString(), where.toString()));
		return count(query.toString(), new Object[] {sourceId, destId}) > 0;
	}
	
	@Override
	protected int doLinkCount(List<MetaDataLink> links, int sourceId, boolean deleted,  boolean ignoreSubdivision, ISearchCriteria criteria,
			boolean distinct, SQLCriteriaContext context) {
		EntityInfo ei = getEntityInfo(links.get(0).getParent());
		if (ei == null) {
			return 0;
		}
		final String mlqc = MultiLinkQuery.getCode(links, deleted, ignoreSubdivision);
		MultiLinkQuery mlq = ei.sql_links.get(mlqc);
		if (mlq == null) {
			mlq = MultiLinkQuery.generate(this, links, deleted, ignoreSubdivision);
			if (mlq == null) {
				return 0;
			}
			ei.sql_links.put(mlqc, mlq);
		}
		if (mlq.rec_alias != null) {
			context.addQueryContext(mlq.rec_alias, mlq.rec_query);
		}
		final EntityInfo e = getEntityInfo(context.getEntity());
		String alias = context.initJoinTree(mlq.linkAlias, mlq.join, e, mlq.linkCol, deleted).getAlias();
		final StringBuilder where;
		final String col;
		if (e == null) {
			// Link to foreign entity...
			col = fg.count;
			if (!mlq.where.isEmpty()) {
				where = new StringBuilder(mlq.where);
			} else {
				where = new StringBuilder(fg.true_cond);
			}
			// FIXME This ignore the distinct constraint !!!
			// FIXME Ignore the criteria clause !!!
			// FIXME This assume that the linked data are not deleted !!!
		} else {
			// A distinct clause need columns... but as long as we select the ID primary key in each request
			// this information is enough to count distinct selection.
			if (distinct) {
				col = String.format(fg.count_distinct, alias + '.' + e.idCol);
			} else {
				col = fg.count;
			}
			where = context.generateCriteria(criteria, true);
			if (!mlq.where.isEmpty()) {
				if (!where.isEmpty()) {
					where.append(fg.and);
				}
				where.append(mlq.where);
			}
		}
		return count(((SQLCriteriaContext) context).formatQuery(fg.select, col, context.generateJoins(deleted), where.toString()), new Object[] {sourceId});
	}
}
