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
import java.util.Map;
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
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.criteria.AbstractLinkTestCriteria;
import com.arcadsoftware.metadata.criteria.AfterCriteria;
import com.arcadsoftware.metadata.criteria.AndCriteria;
import com.arcadsoftware.metadata.criteria.AttributeEqualsCriteria;
import com.arcadsoftware.metadata.criteria.AttributeLowerCriteria;
import com.arcadsoftware.metadata.criteria.AttributeLowerOrEqualsCriteria;
import com.arcadsoftware.metadata.criteria.BeforeCriteria;
import com.arcadsoftware.metadata.criteria.BetweenCriteria;
import com.arcadsoftware.metadata.criteria.ChangedCriteria;
import com.arcadsoftware.metadata.criteria.ConstantCriteria;
import com.arcadsoftware.metadata.criteria.ContainCriteria;
import com.arcadsoftware.metadata.criteria.DeletedCriteria;
import com.arcadsoftware.metadata.criteria.EndCriteria;
import com.arcadsoftware.metadata.criteria.EqualCriteria;
import com.arcadsoftware.metadata.criteria.EqualICCriteria;
import com.arcadsoftware.metadata.criteria.GreaterStrictCriteria;
import com.arcadsoftware.metadata.criteria.GreaterThanCriteria;
import com.arcadsoftware.metadata.criteria.HasRightCriteria;
import com.arcadsoftware.metadata.criteria.ICriteriaContext;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.criteria.IdEqualCriteria;
import com.arcadsoftware.metadata.criteria.IdGreaterStrictCriteria;
import com.arcadsoftware.metadata.criteria.IdGreaterThanCriteria;
import com.arcadsoftware.metadata.criteria.InListCriteria;
import com.arcadsoftware.metadata.criteria.IdLowerStrictCriteria;
import com.arcadsoftware.metadata.criteria.IdLowerThanCriteria;
import com.arcadsoftware.metadata.criteria.IsNullCriteria;
import com.arcadsoftware.metadata.criteria.IsTrueCriteria;
import com.arcadsoftware.metadata.criteria.LinkContainCriteria;
import com.arcadsoftware.metadata.criteria.LinkCriteria;
import com.arcadsoftware.metadata.criteria.LinkEndCriteria;
import com.arcadsoftware.metadata.criteria.LinkEqualCriteria;
import com.arcadsoftware.metadata.criteria.LinkGreaterStrictCriteria;
import com.arcadsoftware.metadata.criteria.LinkGreaterThanCriteria;
import com.arcadsoftware.metadata.criteria.LinkStartCriteria;
import com.arcadsoftware.metadata.criteria.LowerStrictCriteria;
import com.arcadsoftware.metadata.criteria.LowerThanCriteria;
import com.arcadsoftware.metadata.criteria.NotCriteria;
import com.arcadsoftware.metadata.criteria.OrCriteria;
import com.arcadsoftware.metadata.criteria.PreGeneratedCriteria;
import com.arcadsoftware.metadata.criteria.StartCriteria;
import com.arcadsoftware.metadata.criteria.SubstCriteria;
import com.arcadsoftware.metadata.criteria.UnlinkCriteria;
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
public class MapperSQLService extends AbstractMapperService {
	
	private static final boolean RETURNEMPTYBEAMMAP = Boolean.getBoolean("com.arcadsoftware.mapper.sql.empty.beanmap"); //$NON-NLS-1$
	private static final String SQL_JAVA_PREFIX = "j_"; //$NON-NLS-1$
	private static final String SQL_JAVA_CRYPT_PREFIX = "z_"; //$NON-NLS-1$
	private static final String SQL_DATECOL = SQL_JAVA_PREFIX + "date"; //$NON-NLS-1$
	private static final String SQL_DELETECOL = SQL_JAVA_PREFIX + "deleted"; //$NON-NLS-1$
	private static final char COLUMNPREFIX_PLACEHOLDER = '~';
	private static final String COLUMNPREFIX_PLACEHOLDERS = "~"; //$NON-NLS-1$
	private static final String DEFAULT_TABLEALIAS = "x"; //$NON-NLS-1$
	private static final String DEFAULT_LINKTABLEALIAS = "l"; //$NON-NLS-1$
	
	/**
	 * This class is used to store all the join part of a request, this avoid duplicates joins.
	 * <p>There is diferent sort of join stored into this map. First are the attributes joins (basically "left outer joins"),
	 * but links joins (inner join), and criterion joins are stored into this map.
	 * 
	 *  <p>This map ensure that the joins are constructed is the order they are add to the map.
	 */
	protected class JoinsMap {
		
		private HashMap<String, String> keys;
		private StringBuilder joins;
		
		/**
		 * Create a map.
		 * 
		 * @param table the first tables SQL code to be added to the join SQL code.
		 */
		public JoinsMap(String table) {
			joins = new StringBuilder(table);
			//joins.append(fg.as);
			//joins.append(DEFAULT_TABLEALIAS);
			keys = new HashMap<String, String>();
		}

		/**
		 * Add a new join operation if and only if there not already a same key into the map. 
		 * @param key a String key
		 * @param sqlCode a join SQL code.
		 */
		public void add(String key, String sqlCode) {
			if (keys.get(key) == null) {
				joins.append(sqlCode);
				keys.put(key, sqlCode);
			}
		}

		/**
		 * Return true if this key is not already associated to a join code.
		 * 
		 * @param key a String key.
		 * @return true if the key is not found into the map.
		 */
		public boolean doNotExists(String key) {
			return keys.get(key) == null;
		}
		
		@Override
		public String toString() {
			return joins.toString();
		}
	}
	
	private final DataSource ds;
	private final Fragments fg;
	private final Escapes esc;
	private final ConcurrentHashMap<MetaDataEntity, EntityInfo> infos;
	private final QueryRunnerEx runner;
	private final SimpleDateFormat sdf;
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
	 * Construct the columns list an filter the attributes list according to the updated attributes from the entity.
	 * 
	 * @param e entity information
	 * @param attributes Attributes list
	 * @param cols The generated associated Table columns list
	 * @param values The values to be updated.
	 * @return The updated list of values.
	 */
	protected List<Object> filterValues(EntityInfo e, List<MetaDataAttribute> attributes, List<String> cols, List<Object> values) {
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
	 * Get real column name from prefix and abstract column name.
	 *  
	 * @param colName
	 * @param prefix
	 * @return
	 */
	protected String getCol(String colName, String prefix) {
		if (colName.indexOf(COLUMNPREFIX_PLACEHOLDER) >= 0) {
			return colName.replace(Character.toString(COLUMNPREFIX_PLACEHOLDER), prefix + fg.prefix);
		}
		return prefix + fg.prefix + colName;
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
	}

	/**
	 * Return a local version of the given Criteria (Execute remote tests if possible).
	 * @param criteria
	 * @param context
	 * @return
	 */
	protected ISearchCriteria getLocalCriteria(ISearchCriteria criteria, ICriteriaContext context) {
		if (context.isMapperUnique()) {
			return criteria;
		}
		return completeForeignCriteria(criteria, context);
	}
	
	/**
	 * Generate an alias from a code, prefix (if code is null) and suffix.
	 * 
	 * @param code
	 * @param defaultPrefix used only if code is null.
	 * @param suffix optional added to the code or to the defaultPrefix
	 * @return
	 */
	private String getAlias(String code, String defaultPrefix, String suffix) {
		StringBuilder prefix = new StringBuilder();
		if (code == null) {
			prefix.append(defaultPrefix);
		} else {
			prefix.append(code.replace(fg.prefix.charAt(0), '_'));
		}
		if (suffix != null) {
			prefix.append(suffix);
		}
		return prefix.toString();
	}
	
	/**
	 * Generate an SQL where clause from a Criteria.
	 *  
	 * @param entityInfo The base Entity SQL informations.
	 * @param criteria The search criteria to compute.
	 * @param context The context object used to reduce the criteria.
	 * @param colNames The used colons names prefix with their table alias. 
	 * @param joinMap The map of join to add to the request.
	 * @param result The SQL "where" clause. 
	 * @return the "result" param.
	 */
	protected StringBuilder generateCriteria(EntityInfo entityInfo, ISearchCriteria criteria, ICriteriaContext context,
			Map<String, String> colNames, JoinsMap joinMap, StringBuilder result) {
		if (criteria instanceof AfterCriteria) {
			result.append(String.format(fg.greater, 
				colNames.get(((AfterCriteria) criteria).getAttribute()),
				String.format(fg.datefunction, sdf.format(((AfterCriteria) criteria).getCalendar().getTime()))));
		} else if (criteria instanceof NotCriteria) {
			result.append(String.format(fg.not, generateCriteria(entityInfo, ((NotCriteria) criteria).getCriteria(), context, colNames, joinMap, new StringBuilder()).toString()));
		} else if (criteria instanceof OrCriteria) {
			Iterator<ISearchCriteria> itt = ((OrCriteria) criteria).getCriterias().iterator();
			result.append(fg.parin);
			generateCriteria(entityInfo, itt.next(), context, colNames, joinMap, result);
			while (itt.hasNext()) {
				result.append(fg.or);
				generateCriteria(entityInfo, itt.next(), context, colNames, joinMap, result);
			}
			result.append(fg.parout);
		} else if (criteria instanceof AndCriteria) {
			Iterator<ISearchCriteria> itt = ((AndCriteria) criteria).getCriterias().iterator();
			result.append(fg.parin);
			generateCriteria(entityInfo, itt.next(), context, colNames, joinMap, result);
			while (itt.hasNext()) {
				result.append(fg.and);
				generateCriteria(entityInfo, itt.next(), context, colNames, joinMap, result);
			}
			result.append(fg.parout);
		} else if (criteria instanceof BeforeCriteria) {
			result.append(String.format(fg.lower, 
					colNames.get(((BeforeCriteria) criteria).getAttribute()),
					String.format(fg.datefunction, sdf.format(((BeforeCriteria) criteria).getCalendar().getTime()))));
		} else if (criteria instanceof BetweenCriteria) {
			result.append(fg.parin);
			result.append(String.format(fg.greater, 
					colNames.get(((BetweenCriteria) criteria).getAttribute()),
				String.format(fg.datefunction, sdf.format(((BetweenCriteria) criteria).getAfterCalendar().getTime()))));
			result.append(fg.and);
			result.append(String.format(fg.lower, 
					colNames.get(((BetweenCriteria) criteria).getAttribute()),
					String.format(fg.datefunction, sdf.format(((BetweenCriteria) criteria).getBeforeCalendar().getTime()))));
			result.append(fg.parout);
		} else if (criteria instanceof ConstantCriteria) {
			if (((ConstantCriteria) criteria).isValue()) {
				result.append(fg.true_cond);
			} else {
				result.append(fg.false_cond);
			}
		} else if (criteria instanceof ContainCriteria) {
			if (((ContainCriteria) criteria).isCasesensitive()) {
				result.append(String.format(fg.contain, 
						colNames.get(((ContainCriteria) criteria).getAttribute()),
						esc.escape(((ContainCriteria) criteria).getValue())));
			} else {
				result.append(String.format(fg.contain, 
						String.format(fg.lowercase, colNames.get(((ContainCriteria) criteria).getAttribute())),
						esc.escape(((ContainCriteria) criteria).getValue().toLowerCase())));
			}
		} else if (criteria instanceof DeletedCriteria) {
			if (((DeletedCriteria) criteria).getAttribute() == null) {
				if (entityInfo.deleteCol == null) {
					result.append(fg.false_cond);
				} else {
					result.append(fg.parin);
					result.append(DEFAULT_TABLEALIAS);
					result.append(fg.prefix);
					result.append(entityInfo.deleteCol);
					result.append(fg.equaldeltrue);
					result.append(fg.parout);
				}
			} else {
				String attribute = ((DeletedCriteria) criteria).getAttribute();
				EntityInfo ae = getEntityInfo(context.getReference(attribute).getLastAttribute().getRefEntity());
				if (ae.deleteCol == null) {
					result.append(fg.false_cond);
				} else {
					String alias = getAlias(attribute, "dl", "_d"); //$NON-NLS-1$ //$NON-NLS-2$
					if (joinMap.doNotExists(alias)) {
						joinMap.add(alias, String.format(fg.join, ae.table, alias, ae.idCol, colNames.get(attribute)));
					}
					result.append(fg.parin);
					result.append(alias);
					result.append(fg.prefix);
					result.append(ae.deleteCol);
					result.append(fg.equaldeltrue);
					result.append(fg.parout);
				}
			}
		} else if (criteria instanceof EndCriteria) {
			if (((EndCriteria) criteria).isCasesensitive()) {
				result.append(String.format(fg.endwith, 
						colNames.get(((EndCriteria) criteria).getAttribute()),
						esc.escape(((EndCriteria) criteria).getValue())));
			} else {
				result.append(String.format(fg.endwith, 
						String.format(fg.lowercase, colNames.get(((EndCriteria) criteria).getAttribute())),
						esc.escape(((EndCriteria) criteria).getValue().toLowerCase())));
			}
		} else if (criteria instanceof AttributeEqualsCriteria) {
			if (((AttributeEqualsCriteria) criteria).isCasesensitive()) {			
				result.append(String.format(fg.equal,
						colNames.get(((AttributeEqualsCriteria) criteria).getAttribute()),
						colNames.get(((AttributeEqualsCriteria) criteria).getSecondAttribute())));
			} else {
				result.append(String.format(fg.equal,
						String.format(fg.lowercase,colNames.get(((AttributeEqualsCriteria) criteria).getAttribute())),
								String.format(fg.lowercase,colNames.get(((AttributeEqualsCriteria) criteria).getSecondAttribute()))));
			}
		} else if (criteria instanceof AttributeLowerCriteria) {
			result.append(String.format(fg.lower,
					colNames.get(((AttributeLowerCriteria) criteria).getAttribute()),
					colNames.get(((AttributeLowerCriteria) criteria).getSecondAttribute())));
		} else if (criteria instanceof AttributeLowerOrEqualsCriteria) {
			result.append(String.format(fg.lowerorequal,
					colNames.get(((AttributeLowerOrEqualsCriteria) criteria).getAttribute()),
					colNames.get(((AttributeLowerOrEqualsCriteria) criteria).getSecondAttribute())));
		} else if (criteria instanceof EqualCriteria) {
			if (((EqualCriteria) criteria).getIntval() != null) {
				if (context.getReference(((EqualCriteria) criteria).getAttribute()).isNumericType()) {
					result.append(String.format(fg.equal,
							colNames.get(((EqualCriteria) criteria).getAttribute()),
							((EqualCriteria) criteria).getIntval().toString()));
				} else {
					result.append(String.format(fg.equal,
							colNames.get(((EqualCriteria) criteria).getAttribute()),
							enquote(((EqualCriteria) criteria).getIntval().toString())));
				}
			} else if (context.getReference(((EqualCriteria) criteria).getAttribute()).isNumericType()) {
				try {
					Integer.parseInt(((EqualCriteria) criteria).getValue());
					result.append(String.format(fg.equal,
							colNames.get(((EqualCriteria) criteria).getAttribute()),
							((EqualCriteria) criteria).getValue()));
				} catch (NumberFormatException e) {
					result.append(String.format(fg.equal,
							colNames.get(((EqualCriteria) criteria).getAttribute()),
							enquote(((EqualCriteria) criteria).getValue())));
				}
			} else {
				result.append(String.format(fg.equal,
						colNames.get(((EqualCriteria) criteria).getAttribute()),
						enquote(((EqualCriteria) criteria).getValue())));
			}
		} else if (criteria instanceof EqualICCriteria) {
			result.append(String.format(fg.equalignorecase,
					colNames.get(((EqualICCriteria) criteria).getAttribute()),
				enquote(((EqualICCriteria) criteria).getValue().toUpperCase())));
		} else if (criteria instanceof GreaterStrictCriteria) {
			if (context.getReference(((GreaterStrictCriteria)criteria).getAttribute()).isNumericType()) {
				try {
					Integer.parseInt(((GreaterStrictCriteria) criteria).getValue());
					result.append(String.format(fg.greater, 
							colNames.get(((GreaterStrictCriteria) criteria).getAttribute()),
							((GreaterStrictCriteria) criteria).getValue()));
				} catch (NumberFormatException e) {
					result.append(String.format(fg.greater,
							colNames.get(((GreaterStrictCriteria) criteria).getAttribute()),
							enquote(((GreaterStrictCriteria) criteria).getValue())));
				}
			} else {
				result.append(String.format(fg.greater,
						colNames.get(((GreaterStrictCriteria) criteria).getAttribute()),
						enquote(((GreaterStrictCriteria) criteria).getValue())));
			}
		} else if (criteria instanceof GreaterThanCriteria) {
			if (context.getReference(((GreaterThanCriteria) criteria).getAttribute()).isNumericType()) {
				try {
					Integer.parseInt(((GreaterThanCriteria) criteria).getValue());
					result.append(String.format(fg.greaterorequal, 
							colNames.get(((GreaterThanCriteria) criteria).getAttribute()),
							((GreaterThanCriteria) criteria).getValue()));
				} catch (NumberFormatException e) {
					result.append(String.format(fg.greaterorequal,
							colNames.get(((GreaterThanCriteria) criteria).getAttribute()),
							enquote(((GreaterThanCriteria) criteria).getValue())));
				}
			} else {
				result.append(String.format(fg.greaterorequal,
						colNames.get(((GreaterThanCriteria) criteria).getAttribute()),
						enquote(((GreaterThanCriteria) criteria).getValue())));
			}
		} else if (criteria instanceof HasRightCriteria) {
			StringBuilder suffix = new StringBuilder();
			if (((HasRightCriteria) criteria).getRight() != null) {
				suffix.append(Integer.toHexString(((HasRightCriteria) criteria).getRight()));
			}
			if (((HasRightCriteria) criteria).getParam() != null) {
				suffix.append('p');
				suffix.append(Integer.toHexString(((HasRightCriteria) criteria).getParam().hashCode()));
			}
			String alias = getAlias(((HasRightCriteria) criteria).getAttribute(), "r", suffix.toString()); //$NON-NLS-1$
			String col;
			if (((HasRightCriteria) criteria).getAttribute() != null) {
				col = colNames.get(((HasRightCriteria) criteria).getAttribute());
			} else {
				col = DEFAULT_TABLEALIAS + fg.prefix + entityInfo.idCol;
			}
			if (joinMap.doNotExists(alias)) {
				joinMap.add(alias, String.format(fg.join, "USER_RIGHTS", alias, "URI_USER", col)); //$NON-NLS-1$ //$NON-NLS-2$
			}
			// test
			result.append(fg.parin);
			if (((HasRightCriteria) criteria).getRight() != null) {
				result.append(String.format(fg.equal, alias + fg.prefix + "URI_RIGHT", ((HasRightCriteria) criteria).getRight().toString())); //$NON-NLS-1$
			}
			if (((HasRightCriteria) criteria).getParam() != null) {
				if (((HasRightCriteria) criteria).getRight() != null) {
					result.append(fg.and);
				}
				// Param could be an integer, "." or a referenceline.
				if (((HasRightCriteria) criteria).getParam().equals(".")) { //$NON-NLS-1$
					result.append(String.format(fg.equal, alias + fg.prefix + "URI_PARAM", DEFAULT_TABLEALIAS + fg.prefix + entityInfo.idCol)); //$NON-NLS-1$
				} else {
					col = colNames.get(((HasRightCriteria) criteria).getParam());
					if (col == null) {
						// assume that the parameter value is an integer !
						result.append(String.format(fg.equal, alias + fg.prefix + "URI_PARAM", ((HasRightCriteria) criteria).getParam())); //$NON-NLS-1$
					} else {
						result.append(String.format(fg.equal, alias + fg.prefix + "URI_PARAM", col)); //$NON-NLS-1$
					}
				}
			}
			result.append(fg.parout);
		} else if (criteria instanceof IdEqualCriteria) {
			String col = DEFAULT_TABLEALIAS + fg.prefix + entityInfo.idCol;
			result.append(String.format(fg.equal,  col, ((IdEqualCriteria) criteria).getId()));
		} else if (criteria instanceof IdGreaterThanCriteria) {
			String col = DEFAULT_TABLEALIAS + fg.prefix + entityInfo.idCol;
			result.append(String.format(fg.greaterorequal,  col, ((IdGreaterThanCriteria) criteria).getId()));	
		} else if (criteria instanceof IdGreaterStrictCriteria) {
			String col = DEFAULT_TABLEALIAS + fg.prefix + entityInfo.idCol;
			result.append(String.format(fg.greater,  col, ((IdGreaterStrictCriteria) criteria).getId()));	
		} else if (criteria instanceof IdLowerThanCriteria) {
			String col = DEFAULT_TABLEALIAS + fg.prefix + entityInfo.idCol;
			result.append(String.format(fg.lowerorequal,  col, ((IdLowerThanCriteria) criteria).getId()));	
		} else if (criteria instanceof IdLowerStrictCriteria) {
			String col = DEFAULT_TABLEALIAS + fg.prefix + entityInfo.idCol;
			result.append(String.format(fg.lower,  col, ((IdLowerStrictCriteria) criteria).getId()));				
		} else if (criteria instanceof IsNullCriteria) {
			result.append(String.format(fg.isnull, colNames.get(((IsNullCriteria) criteria).getAttribute())));
		} else if (criteria instanceof IsTrueCriteria) {
			result.append(String.format(fg.istrue, colNames.get(((IsTrueCriteria) criteria).getAttribute())));
		} else if (criteria instanceof LinkCriteria) {
			// On construit une jointure externe vers la table d'association
			// Puis on compare l'ID de destination à l'ID passé dans le critère.
			String lc = ((LinkCriteria) criteria).getLinkCode();
			LinkInfo l;
			String alias;
			String attcn = colNames.get(((LinkCriteria) criteria).getAttribute());
			if (attcn == null) {
				alias = getAlias(lc, null, "_a"); //$NON-NLS-1$
				l = entityInfo.links.get(lc);
				if (joinMap.doNotExists(alias)) {
					joinMap.add(alias, String.format(fg.joinref, l.table, alias, l.sourceCol, DEFAULT_TABLEALIAS, entityInfo.idCol));
				}
			} else {
				alias = getAlias(((LinkCriteria) criteria).getAttribute(), lc, "_a"); //$NON-NLS-1$
				l = getEntityInfo(context.getReference(((LinkCriteria) criteria).getAttribute()).getLastAttribute().getRefEntity()).links.get(lc);
				if (joinMap.doNotExists(alias)) {
					joinMap.add(alias, String.format(fg.join, l.table, alias, l.sourceCol, attcn));
				}
				// On ne teste pas que l'entité cible ne soit pas supprimée.
				// La cible est référencée et son id est passé en paramètre. Même si elle supprimée, on l'utilise expressément
				// donc on doit en tenir compte comme un résultat valide.  
			}
			if (l == null) {
				result.append(fg.false_cond);
			} else {
				// prise en compte des association avec suppression logique.
				if (l.deleteCol != null) {
					result.append(fg.parin);
				}
				result.append(String.format(fg.equal, alias + fg.prefix + l.destCol, Integer.toString(((LinkCriteria) criteria).getId())));
				if (l.deleteCol != null) {
					result.append(fg.and);
					result.append(alias);
					result.append(fg.prefix);
					result.append(l.deleteCol);
					result.append(fg.equaldelfalse);
					result.append(fg.parout);
				}
			}
		} else if (criteria instanceof UnlinkCriteria) {
			String attcn = colNames.get(((UnlinkCriteria) criteria).getAttribute());
			LinkInfo l;
			if (attcn == null) {
				attcn = entityInfo.idCol;
				l = entityInfo.links.get(((UnlinkCriteria) criteria).getLinkCode());
			} else {
				l = getEntityInfo(context.getReference(((UnlinkCriteria) criteria).getAttribute()).getLastAttribute().getRefEntity()).links.get(((UnlinkCriteria) criteria).getLinkCode());
			}
			if (l == null) {
				result.append(fg.false_cond);
			} else {
				StringBuilder condition = new StringBuilder();
				int id = ((UnlinkCriteria) criteria).getId();
				if (id > 0) {
					condition.append(String.format(fg.equal, l.destCol, Integer.toString(id)));
				}
				if (l.deleteCol != null) {
					if (id > 0) {
						condition.append(fg.and);
					}
					condition.append(l.deleteCol);
					condition.append(fg.equaldelfalse);
				}
				if (condition.length() == 0) {
					condition.append(fg.true_cond);
				}
				result.append(String.format(fg.notintoselect, attcn, l.sourceCol, l.table, condition.toString()));
			}
		} else if (criteria instanceof AbstractLinkTestCriteria) {
			// We build a join to the target entity, through the association table.
			// Then we follow the joins of the attributes up to the attribute to be tested.
			MetaDataLink link = null;
			LinkInfo l; // = SQL information about the association.
			String alias; // = alias used for the association.
			String refcn = colNames.get(((AbstractLinkTestCriteria) criteria).getReference());
			String lc = ((AbstractLinkTestCriteria) criteria).getLinkCode();
			if (refcn == null) {
				alias = getAlias(lc, null, "_a"); //$NON-NLS-1$
				l = entityInfo.links.get(lc);
				if (l != null) {
					link = (MetaDataLink) context.getEntity().getLink(lc);
					if (joinMap.doNotExists(alias)) {
						joinMap.add(alias, String.format(fg.joinref, l.table, alias, l.sourceCol, DEFAULT_TABLEALIAS, entityInfo.idCol));
					}
				}
			} else {
				alias = getAlias(((AbstractLinkTestCriteria) criteria).getReference(), lc, "_a"); //$NON-NLS-1$
				MetaDataEntity refEntity = context.getReference(((AbstractLinkTestCriteria) criteria).getReference()).getLastAttribute().getRefEntity();
				l = getEntityInfo(refEntity).links.get(lc);
				if (l != null) {
					link = (MetaDataLink) refEntity.getLink(lc);
					if (joinMap.doNotExists(alias)) {
						joinMap.add(alias, String.format(fg.join, l.table, alias, l.sourceCol, refcn));
					}
				}
			}
			if ((l == null) || (link == null)) {
				result.append(fg.false_cond);
			} else {
				EntityInfo ei = getEntityInfo(link.getRefEntity());
				// Specific alias for this reference table.
				String refalias = "d_" + alias; //$NON-NLS-1$
				if (joinMap.doNotExists(refalias)) {
					joinMap.add(refalias, String.format(fg.joinref, ei.table, refalias, ei.idCol, alias, l.destCol));
				}
				// Taking into account associations with logical deletion.
				if ((l.deleteCol != null) || (ei.deleteCol != null)) {
					result.append(fg.parin);
				}
				String attcn = buildAttributeColName(ei, context.getReference(link, ((AbstractLinkTestCriteria) criteria).getAttribute()), joinMap, refalias);
				// Implement the actual SQL test depending on the real criteria type...
				if (criteria instanceof LinkEqualCriteria) {
					if (((LinkEqualCriteria) criteria).getSecondAttribute() != null) {
						if (((LinkEqualCriteria) criteria).isCasesensitive()) {
							result.append(String.format(fg.equal, attcn, colNames.get(((LinkEqualCriteria) criteria).getSecondAttribute())));
						} else {
							result.append(String.format(fg.equal, String.format(fg.lowercase, attcn),
									String.format(fg.lowercase, colNames.get(((LinkEqualCriteria) criteria).getSecondAttribute()))));
						}
					} else if (context.getReference(link, ((LinkEqualCriteria) criteria).getAttribute()).isNumericType()) {
						try {
							Integer.parseInt(((LinkEqualCriteria) criteria).getValue());
							result.append(String.format(fg.equal, attcn, ((LinkEqualCriteria) criteria).getValue()));
						} catch (NumberFormatException e) {
							result.append(String.format(fg.equal, attcn, enquote(((LinkEqualCriteria) criteria).getValue())));
						}
					} else if (((LinkEqualCriteria) criteria).isCasesensitive()) {
						result.append(String.format(fg.equal, attcn, enquote(((LinkEqualCriteria) criteria).getValue())));
					} else {
						result.append(String.format(fg.equalignorecase, attcn, enquote(((EqualICCriteria) criteria).getValue().toUpperCase())));
					}
				} else if (criteria instanceof LinkContainCriteria) {
					if (((LinkContainCriteria) criteria).isCasesensitive()) {
						result.append(String.format(fg.contain, attcn, esc.escape(((LinkContainCriteria) criteria).getValue())));
					} else {
						result.append(String.format(fg.contain, String.format(fg.lowercase, attcn),
								esc.escape(((LinkContainCriteria) criteria).getValue().toLowerCase())));
					}
				} else if (criteria instanceof LinkEndCriteria) {
					if (((LinkEndCriteria) criteria).isCasesensitive()) {
						result.append(String.format(fg.endwith, attcn, esc.escape(((LinkEndCriteria) criteria).getValue())));
					} else {
						result.append(String.format(fg.endwith, String.format(fg.lowercase, attcn),
								esc.escape(((LinkEndCriteria) criteria).getValue().toLowerCase())));
					}
				} else if (criteria instanceof LinkGreaterStrictCriteria) {
					if (context.getReference(link, ((LinkGreaterStrictCriteria) criteria).getAttribute()).isNumericType()) {
						try {
							Integer.parseInt(((LinkGreaterStrictCriteria) criteria).getValue());
							result.append(String.format(fg.greater, attcn, ((LinkGreaterStrictCriteria) criteria).getValue()));
						} catch (NumberFormatException e) {
							result.append(String.format(fg.greater, attcn, enquote(((LinkGreaterStrictCriteria) criteria).getValue())));
						}
					} else {
						result.append(String.format(fg.greater, attcn, enquote(((LinkGreaterStrictCriteria) criteria).getValue())));
					}
				} else if (criteria instanceof LinkGreaterThanCriteria) {
					if (context.getReference(link, ((LinkGreaterThanCriteria) criteria).getAttribute()).isNumericType()) {
						try {
							Integer.parseInt(((LinkGreaterThanCriteria) criteria).getValue());
							result.append(String.format(fg.greaterorequal, attcn, ((LinkGreaterThanCriteria) criteria).getValue()));
						} catch (NumberFormatException e) {
							result.append(String.format(fg.greaterorequal, attcn, enquote(((LinkGreaterThanCriteria) criteria).getValue())));
						}
					} else {
						result.append(String.format(fg.greaterorequal, attcn, enquote(((LinkGreaterThanCriteria) criteria).getValue())));
					}
				} else if (criteria instanceof LinkStartCriteria) {
					if (((LinkStartCriteria) criteria).isCasesensitive()) {
						result.append(String.format(fg.startwith, attcn, esc.escape(((LinkStartCriteria) criteria).getValue())));
					} else {
						result.append(String.format(fg.startwith, String.format(fg.lowercase, attcn), esc.escape(((LinkStartCriteria) criteria).getValue().toLowerCase())));
					}
				}
				if (ei.deleteCol != null) {
					result.append(fg.and);
					result.append(refalias);
					result.append(fg.prefix);
					result.append(ei.deleteCol);
					result.append(fg.equaldelfalse);
				}
				if (l.deleteCol != null) {
					result.append(fg.and);
					result.append(alias);
					result.append(fg.prefix);
					result.append(l.deleteCol);
					result.append(fg.equaldelfalse);
				}
				if ((l.deleteCol != null) || (ei.deleteCol != null)) {
					result.append(fg.parout);
				}
			}
		} else if (criteria instanceof LowerStrictCriteria) {
			if (context.getReference(((LowerStrictCriteria) criteria).getAttribute()).isNumericType()) {
				try {
					Integer.parseInt(((LowerStrictCriteria) criteria).getValue());
					result.append(String.format(fg.lower, 
							colNames.get(((LowerStrictCriteria) criteria).getAttribute()),
							((LowerStrictCriteria) criteria).getValue()));
				} catch (NumberFormatException e) {
					result.append(String.format(fg.lower,
							colNames.get(((LowerStrictCriteria) criteria).getAttribute()),
							enquote(((LowerStrictCriteria) criteria).getValue())));
				}
			} else {
				result.append(String.format(fg.lower,
						colNames.get(((LowerStrictCriteria) criteria).getAttribute()),
						enquote(((LowerStrictCriteria) criteria).getValue())));
			}
		} else if (criteria instanceof LowerThanCriteria) {
			if (context.getReference(((LowerThanCriteria) criteria).getAttribute()).isNumericType()) {
				try {
					Integer.parseInt(((LowerThanCriteria) criteria).getValue());
					result.append(String.format(fg.lowerorequal, 
							colNames.get(((LowerThanCriteria) criteria).getAttribute()),
							((LowerThanCriteria) criteria).getValue()));
				} catch (NumberFormatException e) {
					result.append(String.format(fg.lowerorequal,
							colNames.get(((LowerThanCriteria) criteria).getAttribute()),
							enquote(((LowerThanCriteria) criteria).getValue())));
				}
			} else {
				result.append(String.format(fg.lowerorequal,
						colNames.get(((LowerThanCriteria) criteria).getAttribute()),
						enquote(((LowerThanCriteria) criteria).getValue())));
			}
		} else if (criteria instanceof PreGeneratedCriteria) {
			// FIXME Analyze the SQL code to avoid SQL injection/
			result.append(((PreGeneratedCriteria) criteria).getSql());
		} else if (criteria instanceof StartCriteria) {
			if (((StartCriteria) criteria).isCasesensitive()) {
				result.append(String.format(fg.startwith, 
						colNames.get(((StartCriteria) criteria).getAttribute()),
						esc.escape(((StartCriteria) criteria).getValue())));
			} else {
				result.append(String.format(fg.startwith, 
						String.format(fg.lowercase, colNames.get(((StartCriteria) criteria).getAttribute())),
						esc.escape(((StartCriteria) criteria).getValue().toLowerCase())));
			}
		} else if (criteria instanceof SubstCriteria) {
			Activator.getInstance().debug(Messages.MapperSQLService_InternalError_CriteriaNotReduced, new ResourceException(Status.SERVER_ERROR_INTERNAL));
		} else if (criteria instanceof ChangedCriteria) {
			String dateCol;
			String attribute = ((ChangedCriteria) criteria).getAttribute();
			if (attribute == null) {
				dateCol = DEFAULT_TABLEALIAS + fg.prefix + entityInfo.updateCol;
			} else {
				EntityInfo ae = getEntityInfo(context.getReference(attribute).getLastAttribute().getRefEntity());
				if (ae.updateCol == null) {
					result.append(fg.false_cond);
					return result;
				}
				String alias = getAlias(attribute, "ud", "_u"); //$NON-NLS-1$ //$NON-NLS-2$
				if (joinMap.doNotExists(alias)) {
					joinMap.add(alias, String.format(fg.join, ae.table, alias, ae.idCol, colNames.get(attribute)));
				}
				dateCol = alias + fg.prefix + ae.updateCol;
			}
			result.append(fg.parin);
			result.append(String.format(fg.greater,dateCol,
					String.format(fg.datefunction, sdf.format(((ChangedCriteria) criteria).getAfterCalendar().getTime()))));
			result.append(fg.and);
			result.append(String.format(fg.lower,dateCol,
					String.format(fg.datefunction, sdf.format(((ChangedCriteria) criteria).getBeforeCalendar().getTime()))));			
			result.append(fg.parout);
		} else if (criteria instanceof InListCriteria) {
			String col;
			if (((InListCriteria) criteria).getAttribute() == null) {
				col = DEFAULT_TABLEALIAS + fg.prefix + entityInfo.idCol;
			} else {
				col = colNames.get(((InListCriteria) criteria).getAttribute());
			}
			result.append(String.format(fg.inset,  col, ((InListCriteria) criteria).getIds(fg.columnsep)));	
		} else {
			Activator.getInstance().debug(String.format(Messages.MapperSQLService_UnknownCriteria, criteria.toString(), criteria.getClass().getName()));
		}
		return result;
	}
	
	/**
	 * Create the SQL column name associated to the given ReferenceLine, with the corresponding joins clauses.. 
	 * 
	 * @param entityInfo the entity database information.
	 * @param reference the Attribute reference line to convert.
	 * @param joinMap Will store the join clauses.
	 * @param prefix is an Alias prefix, or an empty string.
	 * @return The SQL column representation.
	 */
	protected String buildAttributeColName(EntityInfo entityInfo, ReferenceLine reference, JoinsMap joinMap, String prefix) {
		// Prérequis: les attribut de type référence ne sont pas composés !
		String code = null;
		StringBuilder currentCode = null;
		EntityInfo lastInfo = null;
		String lastPrefix = null;
		for(int i = 0; i < reference.size(); i++) {
			Element e = reference.get(i);
			if (e instanceof MetaDataAttribute) {
				if (((i + 1) < reference.size()) && ((MetaDataAttribute) e).isLocalReference()) {
					// Jointure de type "outer".
					String col = entityInfo.attributesCols.get(e.getCode());
					if (col == null) {
						break;
					}
					EntityInfo ne = getEntityInfo(((MetaDataAttribute) e).getRefEntity());
					if (ne == null) {
						// Si l'entité n'est pas déclarée en BD.
						break;
					}
					code = e.getCode();
					if (currentCode == null) {
						currentCode = new StringBuilder(code);
					} else {
						currentCode.append('.');
						currentCode.append(code);
					}
					String np = prefix + '_' + code;
					if (joinMap.doNotExists(currentCode.toString())) {
						if (col.indexOf(COLUMNPREFIX_PLACEHOLDER) >= 0) {
							joinMap.add(currentCode.toString(), String.format(fg.join, ne.table, np, ne.idCol, col.replace(COLUMNPREFIX_PLACEHOLDERS, prefix + fg.prefix)));
						} else {
							joinMap.add(currentCode.toString(), String.format(fg.joinref, ne.table, np, ne.idCol, prefix, col));
						}
					}
					// On ne filtre pas les éléments référencés qui serait marqués comme supprimé.
					lastInfo = entityInfo;
					entityInfo = ne;
					lastPrefix = prefix;
					prefix = np;
				} else {
					// Ce noeud n'est pas une référence
					// Ou il est une référence nécessitant une jointure externe
					code = e.getCode();
					lastInfo = entityInfo;
					lastPrefix = prefix;
					//  on s'arrête ici.
					break;
				}
			} else if (e instanceof MetaDataLink) {
				// Jointure de type "inner" (multi-sélection de tous les éléments liés).
				if (!((MetaDataLink) e).isLocal()) {
					break;
					// On s'arrète à l'étape précédente.
				}
				EntityInfo ne = getEntityInfo(((MetaDataLink) e).getRefEntity());
				if (ne == null) {// Si l'entité n'est pas déclarée en BD.
					// On s'arrète à l'étape précédente.
					break;
				}
				LinkInfo l = ne.links.get(e.getCode());
				if (l == null) {
					// On s'arrète à l'étape précédente.
					break;
				}
				// jointure inner
				code = e.getCode();
				String np = prefix + '_' + code;
				String pl = "i_" + np; //$NON-NLS-1$
				if (joinMap.doNotExists(pl)) {
					joinMap.add(pl, String.format(fg.join_inner, l.table, pl, l.sourceCol, prefix, entityInfo.idCol));
				}
				if (currentCode == null) {
					currentCode = new StringBuilder(code);
				} else {
					currentCode.append('.');
					currentCode.append(code);
				}
				// jonture outer sur entityref.
				if (joinMap.doNotExists(currentCode.toString())) {
					joinMap.add(currentCode.toString(), String.format(fg.joinref, ne.table, np, ne.idCol, pl, l.destCol));
				}
				lastInfo = entityInfo;
				entityInfo = ne;
				lastPrefix = prefix;
				prefix = np;
			} else if (e == null) { // Blindage...
				Activator.getInstance().debug("Error into SQL Mapper: A reference line to a 'null' attribute type..."); //$NON-NLS-1$
				break; 
			} else { // on s'arrête ici.
				break;
			}
		}
		// On renvois le code SQL de la colone (i.e. alias.COL_NAME).
		// Le nom de la colone d'un attribut est toujours une chaine à formater.
		if (lastInfo == null) {
			return null;
		}
		String col = lastInfo.attributesCols.get(code);
		if (col == null) {
			return null;
		}
		if (col.contains(COLUMNPREFIX_PLACEHOLDERS)) {
			return col.replace(COLUMNPREFIX_PLACEHOLDERS, lastPrefix + fg.prefix);
		}
		return  lastPrefix + fg.prefix + col;
	}

	/**
	 * Generate the Select columns clause.
	 * 
	 * @param entity
	 * @param entityInfo
	 * @param attributes
	 * @param joins
	 * @param colsNames
	 * @param result
	 * @param prefix
	 */
	protected void generateColumns(MetaDataEntity entity, EntityInfo entityInfo, List<ReferenceLine> attributes, JoinsMap joins, Map<String, String> colsNames, StringBuilder result, String prefix) {
		result.append(prefix);
		result.append(fg.prefix);
		result.append(entityInfo.idCol);
		result.append(fg.asid);
		for(ReferenceLine att:attributes) {
			String cn = buildAttributeColName(entityInfo, att, joins, prefix);
			if (cn != null) {
				colsNames.put(att.getCode(),cn);
				result.append(fg.columnsep);
				result.append(cn);
				result.append(fg.as);
				if (isEncrypted(att.getLast())) {
					result.append(SQL_JAVA_CRYPT_PREFIX);
				} else {
					result.append(SQL_JAVA_PREFIX);
				}
				result.append(att.getCode().replace('.', '_'));
			}
		}
		if (entityInfo.updateCol != null) {
			result.append(fg.columnsep);
			result.append(prefix);
			result.append(fg.prefix);
			result.append(entityInfo.updateCol);
			result.append(fg.as);
			result.append(SQL_DATECOL);
		}
	}
	
	/**
	 * Add a "is deleted" test if required to the given pre-generated Where clause.
	 * 
	 * @param e
	 * @param deleted
	 * @param cols
	 * @param where
	 */
	protected void generateDeleteTest(EntityInfo e, boolean deleted, StringBuilder cols, StringBuilder where) {
		if (e.deleteCol != null) {
			if (deleted) {
				cols.append(fg.columnsep);
				cols.append(DEFAULT_TABLEALIAS);
				cols.append(fg.prefix);
				cols.append(e.deleteCol);
				cols.append(fg.as);
				cols.append(SQL_DELETECOL);
			} else {
				if (where.length() > 0) {
					where.append(fg.and);
				}
				where.append(DEFAULT_TABLEALIAS);
				where.append(fg.prefix);
				where.append(e.deleteCol);
				where.append(fg.equaldelfalse);
			}
		}
	}

	/**
	 * Generate the SQL Order clause.
	 * 
	 * @param orders
	 * @param colNames
	 * @param result
	 */
	protected void generateOrders(List<ReferenceLine> orders, Map<String, String> colNames, StringBuilder result) {
		// FIXME ATTENTION: On ne fait aucun test pour valider que les order ne sont pas des colones complèxes (genre "A+B") !
		if (orders != null) {
			for(ReferenceLine order: orders) {
				String col = colNames.get(order.getCode());
				if (col != null) {
					// on ne trie que par les éléments de même domaine et qui sont effectivement sélectionnés !
					if (result.length() > 0) {
						result.append(fg.columnsep);
					}
					if (order.isFlaged()) { //$NON-NLS-1$
						result.append(String.format(fg.orderdesc, col));
					} else {
						result.append(String.format(fg.orderasc, col));
					}
				}
			}
		}
	}

	private void generateContextCols(EntityInfo entityInfo, ICriteriaContext context, JoinsMap joins, Map<String, String> colsNames, String prefix) {
		for (ReferenceLine att: context.getReferences()) {
			String cn = buildAttributeColName(entityInfo, att, joins, prefix);
			if (cn != null) {
				colsNames.put(att.getCode(), cn);
			}
		}
	}

	/**
	 * Quote an SQL string.
	 *  
	 * @param value
	 * @return
	 */
	protected String enquote(String value) {
		return fg.quote + esc.escape(value) + fg.quote;
	}

	/**
	 * Escape SQL String delimiters.
	 * 
	 * @param value
	 * @return
	 */
	protected String escape(String value) {
		return esc.escape(value);
	}
	
	@Override
	public boolean delete(MetaDataEntity entity, int itemId, boolean hardDelete) {
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
		if (e.updateCol == null) {
			if (e.sql_delete == null) {
				e.sql_delete = String.format(fg.delete, e.table, e.deleteCol, e.idCol);
			}
			return update(e.sql_delete, new Object[] {itemId}) > 0;
		}
		if (e.sql_delete == null) {
			e.sql_delete = String.format(fg.delete_update, e.table, e.deleteCol, e.idCol, e.updateCol);
		}
		return update(e.sql_delete, new Object[] {new Timestamp(System.currentTimeMillis()), itemId}) > 0;
	}

	@Override
	public int delete(MetaDataEntity entity, ISearchCriteria criteria, IConnectionUserBean currentUser, boolean hardDelete) {
		EntityInfo e = getEntityInfo(entity);
		if (e == null) {
			return 0;
		}
		ICriteriaContext context = getContext(entity, currentUser);
		criteria = criteria.reduce(context);
		if (ConstantCriteria.FALSE.equals(criteria)) {
			return 0;
		}
		HashMap<String, String> colNames = new HashMap<String, String>();
		if (hardDelete || (e.deleteCol == null)) {
			JoinsMap joins = new JoinsMap(String.format(fg.tablealias,e.table, DEFAULT_TABLEALIAS));
			generateContextCols(e, context, joins, colNames, DEFAULT_TABLEALIAS);
			StringBuilder where = new StringBuilder();
			generateCriteria(e, criteria, context, colNames, joins, where);
			return update(String.format(fg.delete_hardex, joins.toString(), where.toString()), new Object[0]);
		}
		// An SQL limitation may prevent the usage of joins in updates...
		boolean hasjoins = !context.getLinks().isEmpty();
		if (!hasjoins) {
			for (ReferenceLine ref: context.getReferences()) {
				if (!ref.isSimple()) {
					hasjoins = true;
					break;
				}
			}
		}
		if (fg.update_join.isEmpty() && hasjoins) {
			// on abandonne l'update par requête unique, le critère fait intervenir d'autres éléments.
			// Il faut passer par une présélection
			BeanMapList list = doSelection(new ArrayList<ReferenceLine>(), false, criteria, false, null, 0, -1, context);
			Activator.getInstance().debug(String.format(Messages.MapperSQLService_Info_MultiUpdateWithComplexCriteria,list.size(),context.getEntity().getType()));
			int result = 0;
			Object[] vals;
			int idx = 0;
			if (e.updateCol == null) {
				vals = new Object[] {0};
				if (e.sql_delete == null) {
					e.sql_delete = String.format(fg.delete, e.table, e.deleteCol, e.idCol);
				}
			} else {
				vals = new Object[] {new Timestamp(System.currentTimeMillis()), 0};
				if (e.sql_delete == null) {
					e.sql_delete = String.format(fg.delete_update, e.table, e.deleteCol, e.idCol, e.updateCol);
				}
			}
			for (BeanMap bean: list) {
				vals[idx] = bean.getId();
				result += update(e.sql_delete, vals);
			}
			return result;
		}
		if (criteria != null) {
			criteria = getLocalCriteria(criteria, context);
			if (ConstantCriteria.FALSE.equals(criteria)) {
				return 0;
			}
			if (ConstantCriteria.TRUE.equals(criteria)) {
				criteria = null;
			}
		}
		Object[] vals;
		String lcols = DEFAULT_TABLEALIAS + '.' + e.deleteCol + fg.paramset;
		if (e.updateCol == null) {
			vals = new Object[] {fg.delete_val};
		} else {
			vals = new Object[] {fg.delete_val, new Timestamp(System.currentTimeMillis())};
			lcols += fg.columnsep + DEFAULT_TABLEALIAS + '.' + e.updateCol + fg.paramset;
		}
		if (criteria == null) {
			return update(String.format(fg.updateex, e.table, lcols, fg.true_cond), vals);
		}
		if (hasjoins) {
			JoinsMap joins = new JoinsMap(String.format(fg.tablealias,e.table, DEFAULT_TABLEALIAS));
			generateContextCols(e, context, joins, colNames, DEFAULT_TABLEALIAS);
			StringBuilder where = new StringBuilder();
			generateCriteria(e, criteria, context, colNames, joins, where);
			return update(String.format(fg.update_join, DEFAULT_TABLEALIAS, lcols, joins.toString(), where.toString()), //
					vals);
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
		return update(String.format(fg.updateex, e.table, lcols, //
				generateCriteria(e, criteria, context, colNames, new JoinsMap(""), new StringBuilder()).toString()), //$NON-NLS-1$
				vals);
	}

	@Override
	public int undelete(MetaDataEntity entity, ISearchCriteria criteria, IConnectionUserBean currentUser) {
		EntityInfo e = getEntityInfo(entity);
		if ((e == null) || (e.deleteCol == null)) {
			return 0;
		}
		ICriteriaContext context = getContext(entity, currentUser);
		criteria = criteria.reduce(context);
		if (ConstantCriteria.FALSE.equals(criteria)) {
			return 0;
		}
		// An SQL limitation may prevent the usage of joins in updates...
		boolean hasjoins = !context.getLinks().isEmpty();
		if (!hasjoins) {
			for (ReferenceLine ref: context.getReferences()) {
				if (!ref.isSimple()) {
					hasjoins = true;
					break;
				}
			}
		}
		if (fg.update_join.isEmpty() && hasjoins) {
			// on abandonne l'update par requête unique, le critère fait intervenir d'autres éléments.
			// Il faut passer par une présélection
			BeanMapList list = doSelection(new ArrayList<ReferenceLine>(), true, criteria, false, null, 0, -1, context);
			Activator.getInstance().debug(String.format(Messages.MapperSQLService_Info_MultiUpdateWithComplexCriteria,list.size(),context.getEntity().getType()));
			int result = 0;
			Object[] vals;
			int idx = 0;
			if (e.updateCol == null) {
				vals = new Object[] {0};
				if (e.sql_undelete == null) {
					e.sql_undelete = String.format(fg.undelete, e.table, e.deleteCol, e.idCol);
				}
			} else {
				vals = new Object[] {new Timestamp(System.currentTimeMillis()), 0};
				if (e.sql_undelete == null) {
					e.sql_undelete = String.format(fg.undelete_update, e.table, e.deleteCol, e.idCol, e.updateCol);
				}
			}
			for (BeanMap bean: list) {
				vals[idx] = bean.getId();
				result += update(e.sql_undelete, vals);
			}
			return result;
		}
		if (criteria != null) {
			criteria = getLocalCriteria(criteria, context);
			if (ConstantCriteria.FALSE.equals(criteria)) {
				return 0;
			}
			if (ConstantCriteria.TRUE.equals(criteria)) {
				criteria = null;
			}
		}
		Object[] vals;
		String lcols = DEFAULT_TABLEALIAS + '.' + e.deleteCol + fg.paramset;
		if (e.updateCol == null) {
			vals = new Object[] {fg.undelete_val};
		} else {
			vals = new Object[] {fg.undelete_val, new Timestamp(System.currentTimeMillis())};
			lcols += fg.columnsep + DEFAULT_TABLEALIAS + '.' + e.updateCol + fg.paramset;
		}
		if (criteria == null) {
			return update(String.format(fg.updateex, e.table, lcols, fg.true_cond), vals);
		}
		HashMap<String, String> colNames = new HashMap<String, String>();
		JoinsMap joins = new JoinsMap(String.format(fg.tablealias,e.table, DEFAULT_TABLEALIAS));
		StringBuilder where = new StringBuilder();
		if (hasjoins) {
			generateContextCols(e, context, joins, colNames, DEFAULT_TABLEALIAS);
			generateCriteria(e, criteria, context, colNames, joins, where);
			return update(String.format(fg.update_join, DEFAULT_TABLEALIAS, lcols, joins.toString(), where.toString()), //
					vals);
		}
		for (ReferenceLine ref: context.getReferences()) {
			String code = ref.getCode();
			String col = e.attributesCols.get(code);
			if (col != null) {
				if (col.contains(COLUMNPREFIX_PLACEHOLDERS)) {
					col = col.replace(COLUMNPREFIX_PLACEHOLDERS, "");
				}
				colNames.put(code, col);
			}
		}
		generateCriteria(e, criteria, context, colNames, joins, where);
		return update(String.format(fg.updateex, e.table, lcols, where.toString()), vals);
	}

	@Override
	public boolean undelete(MetaDataEntity entity, int itemId) {
		EntityInfo e = getEntityInfo(entity);
		if ((e == null) || (e.deleteCol == null)) {
			return false;
		}
		if (e.updateCol == null) {
			if (e.sql_undelete == null) {
				e.sql_undelete = String.format(fg.undelete, e.table, e.deleteCol, e.idCol);
			}
			return update(e.sql_undelete, new Object[]{itemId}) > 0;
		}
		if (e.sql_undelete == null) {
			e.sql_undelete = String.format(fg.undelete_update, e.table, e.deleteCol, e.idCol, e.updateCol);
		}
		return update(e.sql_undelete, new Object[]{new Timestamp(System.currentTimeMillis()), itemId}) > 0;
	}

	@Override
	public boolean update(MetaDataEntity entity, int itemId, List<MetaDataAttribute> attributes, List<Object> values) {
		EntityInfo e = getEntityInfo(entity);
		if (e == null) {
			return false;
		}
		ArrayList<String> cols = new ArrayList<String>();
		values = filterValues(e, attributes, cols, values);
		if (values.size() == 0) {
			return false;
		}
		values.add(itemId);
		return update(String.format(fg.update, e.table, listUpdateCols(cols), e.idCol),values.toArray(new Object[values.size()])) > 0;
	}

	@Override
	protected boolean doUpdate(List<MetaDataAttribute> attributes, List<Object> values, ISearchCriteria criteria, ICriteriaContext context) {
		EntityInfo e = getEntityInfo(context.getEntity());
		if (e == null) {
			return false;
		}
		ArrayList<String> cols = new ArrayList<String>();
		values = filterValues(e, attributes, cols, values);
		if (values.size() == 0) {
			return false;
		}
		// An SQL limitation may prevent the usage of joins in updates...
		boolean hasjoins = !context.getLinks().isEmpty();
		if (!hasjoins) {
			for (ReferenceLine ref: context.getReferences()) {
				if (!ref.isSimple()) {
					hasjoins = true;
					break;
				}
			}
		}
		String lcols = listUpdateCols(cols);
		if (fg.update_join.isEmpty() && hasjoins) {
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
		if (criteria != null) {
			criteria = getLocalCriteria(criteria, context);
			if (ConstantCriteria.FALSE.equals(criteria)) {
				return false;
			}
			if (ConstantCriteria.TRUE.equals(criteria)) {
				criteria = null;
			}
		}
		if (criteria == null) {
			return update(String.format(fg.updateex, e.table, lcols, fg.true_cond), values.toArray(new Object[values.size()])) > 0;
		}
		HashMap<String, String> colNames = new HashMap<String, String>();
		JoinsMap joins = new JoinsMap(String.format(fg.tablealias,e.table, DEFAULT_TABLEALIAS));
		if (hasjoins) {
			generateContextCols(e, context, joins, colNames, DEFAULT_TABLEALIAS);
			StringBuilder where = new StringBuilder();
			generateCriteria(e, criteria, context, colNames, joins, where);
			return update(String.format(fg.update_join, DEFAULT_TABLEALIAS, lcols, joins.toString(), where.toString()), //
					values.toArray(new Object[values.size()])) > 0;
		}
		for (ReferenceLine ref: context.getReferences()) {
			String code = ref.getCode();
			String col = e.attributesCols.get(code);
			if (col != null) {
				if (col.contains(COLUMNPREFIX_PLACEHOLDERS)) {
					col = col.replace(COLUMNPREFIX_PLACEHOLDERS, "");
				}
				colNames.put(code, col);
			}
		}
		// joins map is not used here (the criteria reduction does not own any joins... it is only used to avoid NPE.
		return update(String.format(fg.updateex, e.table, lcols, //
				generateCriteria(e, criteria, context, colNames, joins, new StringBuilder()).toString()),//
				values.toArray(new Object[values.size()])) > 0;
	}

	@Override
	public boolean doLinkAdd(MetaDataLink link, int sourceId, int destId) {
		EntityInfo e = getEntityInfo(link.getParent());
		if (e == null) {
			return false;
		}
		LinkInfo l = e.links.get(link.getCode());
		if (l == null) {
			return false;
		}
		if (l.sql_add == null) {
			l.sql_add = String.format(fg.insert_hard, l.table, l.sourceCol + fg.columnsep + l.destCol, fg.param2);
		}
		return update(l.sql_add,new Object[] {sourceId, destId}) > 0;
	}

	@Override
	protected boolean doLinkTest(MetaDataLink link, int sourceId, int destId) {
		EntityInfo e = getEntityInfo(link.getParent());
		if (e == null) {
			return false;
		}
		LinkInfo l = e.links.get(link.getCode());
		if (l == null) {
			return false;
		}
		if (l.sql_test == null) {
			StringBuilder clause = new StringBuilder();
			clause.append(String.format(fg.equal, l.sourceCol, fg.param));
			clause.append(fg.and);
			clause.append(String.format(fg.equal, l.destCol, fg.param));
			if (l.deleteCol != null) {
				clause.append(fg.and);
				clause.append(String.format(fg.equal, l.deleteCol, fg.false_val));
			}
			l.sql_test = String.format(fg.select, fg.count, l.table, clause.toString());
		}
		return count(l.sql_test,new Object[] {sourceId, destId}) > 0;
	}

	@Override
	protected boolean doLinkRemove(MetaDataLink link, int sourceId, int destId) {
		EntityInfo e = getEntityInfo(link.getParent());
		if (e == null) {
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
	public BeanMap create(MetaDataEntity entity, List<MetaDataAttribute> attributes, List<Object> values) {
		EntityInfo e = getEntityInfo(entity);
		if (e == null) {
			return null;
		}
		ArrayList<String> cols = new ArrayList<String>();
		values = filterValues(e, attributes, cols, values);
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
		EntityInfo e = getEntityInfo(entity);
		if (e == null) {
			return null;
		}
		if (attributes == null) {
			attributes = entity.getAllAttributes();
		}
		JoinsMap joins = new JoinsMap(String.format(fg.tablealias,e.table, DEFAULT_TABLEALIAS));
		StringBuilder cols = new StringBuilder();
		generateColumns(entity, e, attributes, joins, new HashMap<String,String>(), cols, DEFAULT_TABLEALIAS);
		StringBuilder where = new StringBuilder(DEFAULT_TABLEALIAS);
		where.append(fg.prefix);
		where.append(e.idCol);
		where.append(fg.paramequal);
		generateDeleteTest(e, deleted, cols, where);
		return completeForeignAttributes(attributes, query(String.format(fg.select, cols.toString(), joins.toString(), where.toString()), entity.getType(), new Object[] {itemId}));
	}

	@Override
	protected BeanMapList doSelection(List<ReferenceLine> attributes, boolean deleted, ISearchCriteria criteria,
			boolean distinct, List<ReferenceLine> orders, int page, int limit, ICriteriaContext context) {
		EntityInfo e = getEntityInfo(context.getEntity());
		if (e == null) {
			return new BeanMapPartialList();
		}
		StringBuilder cols = new StringBuilder();
		if (distinct) {
			cols.append(fg.distinct);
		}
		JoinsMap joins = new JoinsMap(String.format(fg.tablealias,e.table, DEFAULT_TABLEALIAS));
		HashMap<String, String> colNames = new HashMap<String, String>();
		generateColumns(context.getEntity(), e, attributes, joins, colNames, cols, DEFAULT_TABLEALIAS);
		StringBuilder orderCols = new StringBuilder();
		generateOrders(orders, colNames, orderCols);
		generateContextCols(e, context, joins, colNames, DEFAULT_TABLEALIAS);
		StringBuilder where = new StringBuilder();
		if ((criteria != null) && !ConstantCriteria.TRUE.equals(criteria)) {
			criteria = getLocalCriteria(criteria, context);
			if (ConstantCriteria.FALSE.equals(criteria)) {
				return new BeanMapPartialList();
			}
			if (!ConstantCriteria.TRUE.equals(criteria)) {
				generateCriteria(e, criteria, context, colNames, joins, where);
			}
		}
		generateDeleteTest(e, deleted, cols, where);
		String query;
		if (((page == 0) && (limit <= 0)) || (fg.partial == null) || (fg.partial.length() == 0)) {
			// Pas de pagination (pas nécessaire ou not supporté par le SGDB).
			if (where.length() == 0) {
				if (orderCols.length() == 0) {
					query = String.format(fg.selectall, cols.toString(), joins.toString());
				} else {
					query = String.format(fg.selectallorder, cols.toString(), joins.toString(), orderCols.toString());
				}
			} else if (orderCols.length() == 0) {
				query = String.format(fg.select, cols.toString(), joins.toString(), where.toString());
			} else {
				query = String.format(fg.selectorder, cols.toString(), joins.toString(), where.toString(), orderCols.toString());
			}
		} else {
			if (where.length() == 0) {
				if (orderCols.length() == 0) {
					query = String.format(fg.partialall, cols.toString(), joins.toString(), //
							page, // first element to return
							limit, // number of element to return
							page + limit + 1, // first element to not return.
							e.idCol);
				} else {
					query = String.format(fg.partialallorder, cols.toString(), joins.toString(), //
							page, // first element to return
							limit, // number of element to return
							page + limit + 1, // first element to not return.
							orderCols.toString(), e.idCol);
				}
			} else if (orderCols.length() == 0) {
				query = String.format(fg.partial, cols.toString(), joins.toString(), where.toString(), //
				page, // first element to return
				limit, // number of element to return
				page + limit + 1, // first element to not return.
				e.idCol);
			} else {
				query = String.format(fg.partialorder, cols.toString(), joins.toString(), where.toString(), //
						page, // first element to return
						limit, // number of element to return
						page + limit + 1, // first element to not return.
						orderCols.toString(), e.idCol);
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
			ICriteriaContext context) {
		EntityInfo e = getEntityInfo(context.getEntity());
		if (e == null) {
			return null;
		}
		StringBuilder cols = new StringBuilder();
		JoinsMap joins = new JoinsMap(String.format(fg.tablealias,e.table, DEFAULT_TABLEALIAS));
		HashMap<String, String> colNames = new HashMap<String, String>();
		generateColumns(context.getEntity(), e, attributes, joins, colNames, cols, DEFAULT_TABLEALIAS);
		generateContextCols(e, context, joins, colNames, DEFAULT_TABLEALIAS);
		StringBuilder where = new StringBuilder();
		if ((criteria != null) && !ConstantCriteria.TRUE.equals(criteria)) {
			criteria = getLocalCriteria(criteria, context);
			if (ConstantCriteria.FALSE.equals(criteria)) {
				return null;
			}
			if (!ConstantCriteria.TRUE.equals(criteria)) {
				generateCriteria(e, criteria, context, colNames, joins, where);
			}
		}
		generateDeleteTest(e, deleted, cols, where);
		String query;
		if (where.length() == 0) {
			query = String.format(fg.selectall, cols.toString(), joins.toString());
		} else {
			query = String.format(fg.select, cols.toString(), joins.toString(), where.toString());
		}
		return completeForeignAttributes(attributes, query(query, context.getEntity().getType(), null));
	}

	@Override
	protected int doCount(boolean deleted, ISearchCriteria criteria, boolean distinct, ICriteriaContext context) {
		EntityInfo e = getEntityInfo(context.getEntity());
		if (e == null) {
			return 0;
		}
		// utilisation de distinct
		String col;
		if (distinct) {
			col = String.format(fg.count_distinct, DEFAULT_TABLEALIAS + fg.prefix + e.idCol);
		} else {
			col = fg.count;
		}
		JoinsMap joins = new JoinsMap(String.format(fg.tablealias,e.table, DEFAULT_TABLEALIAS));
		HashMap<String, String> colNames = new HashMap<String, String>();
		generateContextCols(e, context, joins, colNames, DEFAULT_TABLEALIAS);
		StringBuilder where = new StringBuilder();
		if ((criteria != null) && !ConstantCriteria.TRUE.equals(criteria)) {
			criteria = getLocalCriteria(criteria,context);
			if (ConstantCriteria.FALSE.equals(criteria)) {
				return 0;
			}
			if (!ConstantCriteria.TRUE.equals(criteria)) {
				generateCriteria(e, criteria, context, colNames, joins, where);
			}
		}
		if (!deleted) {
			generateDeleteTest(e, deleted, null, where);
		}
		if (where.length() > 0) {
			return count(String.format(fg.select, col, joins.toString(), where.toString()), null);
		}
		return count(String.format(fg.selectall, col, joins.toString()), null);
	}

	@Override
	protected BeanMapList doLinkSelection(MetaDataLink link, int sourceId, List<ReferenceLine> attributes,
			boolean deleted, ISearchCriteria criteria, boolean distinct, List<ReferenceLine> orders, int page,
			int limit, ICriteriaContext context) {
		EntityInfo e = getEntityInfo(link.getParent());
		LinkInfo l;
		if (e == null) {
			e = getEntityInfo(context.getEntity());
			if (e == null) { // not foreign entity, try to revert the link !
				// We should send the request to the mapper of the source entity... but there is no reason why the SQL mapper was used
				// to generate a link selection on to foreign entities !!!
				return new BeanMapPartialList();
			}
			// We assume that the link table is always into the Database source !
			l = new LinkInfo(link.getRefEntity(), link);
			if (!l.isComplete()) {
				l = null;
			}
			// invert the link !
			String ref = l.destCol;
			l.destCol = l.sourceCol;
			l.sourceCol = ref;
		} else {
			l = e.links.get(link.getCode());
			e = getEntityInfo(context.getEntity());
		}
		if (l == null) {
			return new BeanMapPartialList();
		}
		JoinsMap joins = new JoinsMap(String.format(fg.tablealias, l.table, DEFAULT_LINKTABLEALIAS));
		StringBuilder where = new StringBuilder(String.format(fg.equal, DEFAULT_LINKTABLEALIAS + fg.prefix + l.sourceCol, fg.param));
		StringBuilder cols = new StringBuilder();
		if (distinct) {
			cols.append(fg.distinct);
		}
		HashMap<String, String> colNames = new HashMap<String, String>();
		StringBuilder orderCols = new StringBuilder();
		if (e == null) { // FIXME Non support des link intra-mappers
			if (context.getEntity() == null) {
				return new BeanMapPartialList();
			}
			// Get the list of ID to get from the foreign mapper...
			cols.append(DEFAULT_LINKTABLEALIAS);
			cols.append(fg.prefix);
			cols.append(l.destCol);
			cols.append(fg.asid);
		} else {
			joins.add(DEFAULT_TABLEALIAS, String.format(fg.joinref, e.table, DEFAULT_TABLEALIAS, e.idCol, 
					DEFAULT_LINKTABLEALIAS, l.destCol));
			generateColumns(context.getEntity(), e, attributes, joins, colNames, cols, DEFAULT_TABLEALIAS);
			generateOrders(orders, colNames, orderCols);
			generateContextCols(e, context, joins, colNames, DEFAULT_TABLEALIAS);
			if ((criteria != null) && !ConstantCriteria.TRUE.equals(criteria)) {
				criteria = getLocalCriteria(criteria,context);
				if (ConstantCriteria.FALSE.equals(criteria)) {
					return new BeanMapPartialList();
				}
				if (!ConstantCriteria.TRUE.equals(criteria)) {
					where.append(fg.and);
					generateCriteria(e, criteria, context, colNames, joins, where);
				}
			}
			generateDeleteTest(e, deleted, cols, where);
		}
		String query;
		boolean softPagination = false;
		if (((page == 0) && (limit <= 0)) || (fg.partial == null) || (fg.partial.length() == 0) || (e == null)) {
			softPagination = (fg.partial == null) || (fg.partial.length() == 0);
			// Pas de pagination (pas nécessaire ou non supporté par le SGDB).
			if (where.length() == 0) {
				if (orderCols.length() == 0) {
					query = String.format(fg.selectall, cols.toString(), joins.toString());
				} else {
					query = String.format(fg.selectallorder, cols.toString(), joins.toString(), orderCols.toString());
				}
			} else if (orderCols.length() == 0) {
				query = String.format(fg.select, cols.toString(), joins.toString(), where.toString());
			} else {
				query = String.format(fg.selectorder, cols.toString(), joins.toString(), where.toString(), orderCols.toString());
			}
		} else if (where.length() == 0) {
			if (orderCols.length() == 0) {
				query = String.format(fg.partialall, cols.toString(), joins.toString(), //
						page, // first element to return
						limit, // number of element to return
						page + limit + 1, // first element to not return.
						e.idCol);
			} else {
				query = String.format(fg.partialallorder, cols.toString(), joins.toString(), //
						page, // first element to return
						limit, // number of element to return
						page + limit + 1, // first element to not return.
						orderCols.toString(), e.idCol);
			}
		} else if (orderCols.length() == 0) {
			query = String.format(fg.partial, cols.toString(), joins.toString(), where.toString(), //
			page, // first element to return
			limit, // number of element to return
			page + limit + 1, // first element to not return.
			e.idCol);
		} else {
			query = String.format(fg.partialorder, cols.toString(), joins.toString(), where.toString(), //
					page, // first element to return
					limit, // number of element to return
					page + limit + 1, // first element to not return.
					orderCols.toString(), e.idCol);
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
		if (e != null) {
			// Result list completed, add the foreigns attributes...
			return completeForeignAttributes(attributes, result);
		}
		// select the foreign entities
		if (result.size() == 0) {
			return result;
		}
		if (result.size() < 50) {
			OrCriteria presel = new OrCriteria();
			for (BeanMap b: result) {
				presel.add(new IdEqualCriteria(b.getId()));
			}
			return link.getRefEntity().getMapper().selection(attributes, //
					deleted, //
					new AndCriteria(criteria, presel), //
					distinct, //
					orders, //
					context.getCurrentUser(), //
					0, -1);
		}
		BeanMapList presel = new BeanMapList(result.size());
		int z = result.size();
		while (z > 0) {
			OrCriteria subsel = new OrCriteria();
			int zz = z - 40;
			if (zz < 0) {
				zz = 0;
			}
			for (int i = zz; i < z; i++) {
				subsel.add(new IdEqualCriteria(result.get(i).getId()));
			}
			z = zz;
			presel.addAll(link.getRefEntity().getMapper().selection(attributes, //
					deleted, //
					new AndCriteria(criteria, subsel), //
					distinct, //
					orders, //
					context.getCurrentUser(), //
					0, -1));
		}
		// FIXME The order is not globally respected !!!
		return new BeanMapPartialList(presel, page, limit);
	}

	@Override
	protected int doLinkCount(MetaDataLink link, int sourceId, boolean deleted, ISearchCriteria criteria,
			boolean distinct, ICriteriaContext context) {
		EntityInfo e = getEntityInfo(link.getParent());
		LinkInfo l = null;
		if (e == null) { // foreign entity source... try the revert the link !
			e = getEntityInfo(context.getEntity());
			if (e == null) { // not foreign entity !
				// We should send the request to the mapper of the source entity... but there is no reason why this SQL mapper was used
				// to generate a link selection on to foreign entities !!!
				return 0;
			}
			// We assume that the link table is always into the Database source !
			l = new LinkInfo(link.getRefEntity(), link);
			if (!l.isComplete()) {
				l = null;
			}
			// invert the link !
			String ref = l.destCol;
			l.destCol = l.sourceCol;
			l.sourceCol = ref;
		} else {
			l = e.links.get(link.getCode());
			e = getEntityInfo(context.getEntity());
		}
		if (l == null) {
			return 0;
		}
		JoinsMap joins = new JoinsMap(String.format(fg.tablealias, l.table, DEFAULT_LINKTABLEALIAS));
		String col;
		StringBuilder where = new StringBuilder(String.format(fg.equal, DEFAULT_LINKTABLEALIAS + fg.prefix + l.sourceCol, fg.param));
		HashMap<String, String> colNames = new HashMap<String, String>();
		if (e == null) {
			if (context.getEntity() == null) {
				return 0;
			}
			col = fg.count;
			// Link "extra-mapper"...
			// FIXME Ignore the distinct constraint !!!
			// FIXME Ignore the criteria clause !!!
			// FIXME This assume that the linked data are not deleted !!!
		} else {
			joins.add(DEFAULT_TABLEALIAS, String.format(fg.joinref, e.table, DEFAULT_TABLEALIAS, e.idCol, 
					DEFAULT_LINKTABLEALIAS, l.destCol));
			// utilisation de distinct
			if (distinct) {
				col = String.format(fg.count_distinct, DEFAULT_TABLEALIAS + fg.prefix + e.idCol);
			} else {
				col = fg.count;
			}
			generateContextCols(e, context, joins, colNames, DEFAULT_TABLEALIAS);
			if ((criteria != null) && !ConstantCriteria.TRUE.equals(criteria)) {
				criteria = getLocalCriteria(criteria, context);
				if (ConstantCriteria.FALSE.equals(criteria)) {
					return 0;
				}
				if (!ConstantCriteria.TRUE.equals(criteria)) {
					where.append(fg.and);
					generateCriteria(e, criteria, context, colNames, joins, where);
				}
			}
			if (!deleted) {
				generateDeleteTest(e, deleted, null, where);
			}
		}
		return count(String.format(fg.select, col, joins.toString(), where.toString()), new Object[] {sourceId});
	}

	/**
	 * Parse complex Attributes column names.
	 * 
	 * @param type
	 * @param col
	 * @return
	 */
	protected String parseAttributeColumn(String type, String col) {
		int trunc = 0;
		// Truncate long Strings.
		if (col.indexOf('^') > 0) {
			try {
				trunc = Integer.parseInt(col.substring(col.indexOf('^') + 1));
				col = col.substring(0, col.indexOf('^') - 1);
				if (col.indexOf('+') < 0) {
					col = COLUMNPREFIX_PLACEHOLDER + col;
				}
			} catch (NumberFormatException e) {
				Activator.getInstance().debug(e);
			}
		} 
		// Concat or sum columns values.
		if (col.indexOf('+') > 0) {
			String concat = fg.concat;
			if (MetaDataAttribute.TYPE_STRING.equals(type)) {
				concat = fg.concat_string;
			} else if (MetaDataAttribute.TYPE_DATE.equals(type)) {
				concat = fg.concat_days;
			}
			char quote = fg.quote.charAt(0);
			String[] cols = col.split("\\+"); //$NON-NLS-1$
			String result;
			if (cols[0].charAt(0) == quote) {
				result = String.format(concat, cols[0], "%1$s"); //$NON-NLS-1$
			} else {
				result = String.format(concat, COLUMNPREFIX_PLACEHOLDER + cols[0], "%1$s"); //$NON-NLS-1$
			}
			for (int i = 1; i < (cols.length - 1); i++) {
				if (cols[i].charAt(0) == quote) {
					result = String.format(result, String.format(concat, cols[i], "%1$s")); //$NON-NLS-1$
				} else {
					result = String.format(result, String.format(concat, COLUMNPREFIX_PLACEHOLDER + cols[i], "%1$s")); //$NON-NLS-1$
				}
			}
			if (cols[cols.length - 1].charAt(0) == quote) {
				col = String.format(result, cols[cols.length - 1]);
			} else {
				col = String.format(result, COLUMNPREFIX_PLACEHOLDER + cols[cols.length - 1]);
			}
		}
		if (trunc > 0) {
			return String.format(fg.trunc_string, col, trunc);
		}
		return col;
	}

}
