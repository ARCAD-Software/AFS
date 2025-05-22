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

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

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
import com.arcadsoftware.metadata.criteria.ChangedByCriteria;
import com.arcadsoftware.metadata.criteria.ChangedCriteria;
import com.arcadsoftware.metadata.criteria.ConstantCriteria;
import com.arcadsoftware.metadata.criteria.ContainCriteria;
import com.arcadsoftware.metadata.criteria.CriteriaContextBasic;
import com.arcadsoftware.metadata.criteria.DeletedCriteria;
import com.arcadsoftware.metadata.criteria.EndCriteria;
import com.arcadsoftware.metadata.criteria.EqualCriteria;
import com.arcadsoftware.metadata.criteria.GreaterStrictCriteria;
import com.arcadsoftware.metadata.criteria.GreaterThanCriteria;
import com.arcadsoftware.metadata.criteria.HasRightCriteria;
import com.arcadsoftware.metadata.criteria.IAttributesCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.criteria.IdEqualCriteria;
import com.arcadsoftware.metadata.criteria.IdGreaterStrictCriteria;
import com.arcadsoftware.metadata.criteria.IdGreaterThanCriteria;
import com.arcadsoftware.metadata.criteria.IdLowerStrictCriteria;
import com.arcadsoftware.metadata.criteria.IdLowerThanCriteria;
import com.arcadsoftware.metadata.criteria.InListCriteria;
import com.arcadsoftware.metadata.criteria.IsNullCriteria;
import com.arcadsoftware.metadata.criteria.IsTrueCriteria;
import com.arcadsoftware.metadata.criteria.LinkContainCriteria;
import com.arcadsoftware.metadata.criteria.LinkCriteria;
import com.arcadsoftware.metadata.criteria.LinkEndCriteria;
import com.arcadsoftware.metadata.criteria.LinkEqualCriteria;
import com.arcadsoftware.metadata.criteria.LinkGreaterStrictCriteria;
import com.arcadsoftware.metadata.criteria.LinkGreaterThanCriteria;
import com.arcadsoftware.metadata.criteria.LinkLowerStrictCriteria;
import com.arcadsoftware.metadata.criteria.LinkLowerThanCriteria;
import com.arcadsoftware.metadata.criteria.LinkStartCriteria;
import com.arcadsoftware.metadata.criteria.LowerStrictCriteria;
import com.arcadsoftware.metadata.criteria.LowerThanCriteria;
import com.arcadsoftware.metadata.criteria.NotCriteria;
import com.arcadsoftware.metadata.criteria.OrCriteria;
import com.arcadsoftware.metadata.criteria.PreGeneratedCriteria;
import com.arcadsoftware.metadata.criteria.StartCriteria;
import com.arcadsoftware.metadata.criteria.SubstCriteria;
import com.arcadsoftware.metadata.sql.internal.Activator;
import com.arcadsoftware.metadata.sql.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * This Mapper operation context is used to extend the basic implementation with SQL specific operations.
 * 
 * <ul>
 * <li>It manage the "with" clauses required to be added before the actual request.
 * <li>Store all the required joins if a tree structure allowing to minimize the joins operations.
 * </ul>
 * <p>
 * TODO Allow to define a "cache" of contexts to be able to optimize the SQL request generation. 
 * 
 * @author ARCAD Software
 */
public class SQLCriteriaContext extends CriteriaContextBasic {

	private static final String SQL_JAVA_PREFIX = "j_"; //$NON-NLS-1$
	private static final String SQL_JAVA_CRYPT_PREFIX = "z_"; //$NON-NLS-1$
	private static final String SQL_DATECOL = SQL_JAVA_PREFIX + "date"; //$NON-NLS-1$
	private static final String SQL_MUIDCOL = SQL_JAVA_PREFIX + "muid"; //$NON-NLS-1$
	private static final String SQL_DELETECOL = SQL_JAVA_PREFIX + "deleted"; //$NON-NLS-1$
	private static final String SQL_RECURCIVE_PREFIX = "rt_"; //$NON-NLS-1$

	private final MapperSQLService mapper;
	private final HashMap<String, String> queryContextes;
	private final EntityInfo entityInfo;
	private final HashMap<String, String> colNames;
	private JoinElement joinTree;
	private int hrpc;
	
	public SQLCriteriaContext(MapperSQLService mapper, MetaDataEntity entity, IConnectionUserBean currentUser) {
		super(entity, currentUser);
		this.mapper = mapper;
		queryContextes = new HashMap<>();
		entityInfo = mapper.getEntityInfo(entity);
		colNames = new HashMap<>();
		hrpc = 1;
	}
	
	/**
	 * @return False if this SQL Context is not operational, basically, if and only if the given original entity, belong to the given SQL mapper.
	 */
	protected boolean isValid() {
		return entityInfo != null;
	}
	
	protected EntityInfo getEntityInfo() {
		return entityInfo;
	}
	
	protected String generateJoins(boolean deleted) {
		init(deleted);
		return joinTree.toString(mapper);
	}
	
	private void init(boolean deleted) {
		if (joinTree == null) {
			joinTree = new JoinElement(MapperSQLService.DEFAULT_TABLEALIAS, //
					String.format(mapper.fg.tablealias, entityInfo.table, MapperSQLService.DEFAULT_TABLEALIAS));
			for (ReferenceLine rf: getReferences()) {
				String code = rf.getCode();
				if (!colNames.containsKey(code)) {
					String col = buildAttributeColName(rf, false, deleted);
					if (col != null) {
						colNames.put(code, col);
					}
				}
			}
		}
	}

	/**
	 * Quote an SQL string.
	 *  
	 * @param value
	 * @return
	 */
	private String enquote(String value) {
		return mapper.fg.quote + mapper.esc.escape(value) + mapper.fg.quote;
	}

	/**
	 * Escape SQL String delimiters.
	 * 
	 * @param value
	 * @return
	 */
	private String escape(String value) {
		return mapper.esc.escape(value);
	}

	/**
	 * If the Form clause require a dedicated initialization, it must be called before any generate methods !
	 * 
	 * <p>
	 * This method is mainly used for links selection where the first table od the selection is a link table of a recursive selection.
	 * 
	 * @param sql
	 */
	protected JoinElement initJoinTree(String alias, String sql, EntityInfo entity, String parentCol, boolean deleted) {
		if (joinTree == null) {
			joinTree = new JoinElement(alias, sql);
			JoinElement result;
			if (entity == null) {
				result = joinTree;
				entity = entityInfo;
			} else {
				result = joinTree.add(entity, alias + '.' + parentCol, deleted);
			}
			for (ReferenceLine rf: getReferences()) {
				String code = rf.getCode();
				if (!colNames.containsKey(code)) {
					// We use left outer join for columns used in where clause.
					String col = buildAttributeColName(rf, entity, result, false, deleted);
					if (col != null) {
						colNames.put(code, col);
					}
				}
			}
			return result;
		}
		Activator.getInstance().error("Error into SQL Mapper: the Form clause is initialized after a generation is done..."); //$NON-NLS-1$
		return null;
	}

	/**
	 * Generate the Selected columns clause.
	 * 
	 * @param entity
	 * @param entityInfo
	 * @param attributes
	 * @param joins
	 * @param colsNames
	 * @param result
	 * @param prefix
	 */
	protected StringBuilder generateColumns(List<ReferenceLine> attributes, boolean deleted) {
		init(deleted);
		return generateColumns(entityInfo, joinTree, attributes, deleted);
	}
	
	/**
	 * Generate the Selected columns clause, without initialization of the SQL context.
	 * 
	 * @param e
	 * @param join
	 * @param attributes
	 * @param deleted
	 * @return
	 */
	protected StringBuilder generateColumns(EntityInfo e, JoinElement join, List<ReferenceLine> attributes, boolean deleted) {
		StringBuilder result = new StringBuilder(join.getAlias());
		result.append('.');
		result.append(e.idCol);
		result.append(mapper.fg.asid);
		for (ReferenceLine att: attributes) {
			String cn = buildAttributeColName(att, e, join, false, deleted);
			if (cn != null) {
				colNames.put(att.getCode(), cn);
				result.append(mapper.fg.columnsep);
				result.append(cn);
				result.append(mapper.fg.as);
				if (mapper.isEncrypted(att.getLast())) {
					result.append(SQL_JAVA_CRYPT_PREFIX);
				} else {
					result.append(SQL_JAVA_PREFIX);
				}
				result.append(att.getCode().replace('.', '_'));
			}
		}
		if (e.updateCol != null) {
			result.append(mapper.fg.columnsep);
			result.append(join.getAlias());
			result.append('.');
			result.append(e.updateCol);
			result.append(mapper.fg.as);
			result.append(SQL_DATECOL);
		}
		if (e.muidCol != null) {
			result.append(mapper.fg.columnsep);
			result.append(join.getAlias());
			result.append('.');
			result.append(e.muidCol);
			result.append(mapper.fg.as);
			result.append(SQL_MUIDCOL);
		}
		if ((e.deleteCol != null) && deleted) {
			result.append(mapper.fg.columnsep);
			result.append(join.getAlias());
			result.append('.');
			result.append(e.deleteCol);
			result.append(mapper.fg.as);
			result.append(SQL_DELETECOL);
		}
		return result;
	}

	/**
	 * Generate the SQL Order clause.
	 * 
	 * <p>
	 * The Order columns are assumed to be already present in the selected ones. Must be called after generateColumns.
	 * 
	 * @param orders
	 * @return the Order SQL clause.
	 */
	protected String generateOrders(List<ReferenceLine> orders, boolean deleted) {
		return generateOrders(joinTree, orders, deleted);
	}
	/**
	 * Generate the SQL Order clause.
	 * 
	 * <p>
	 * The Order columns are assumed to be already present in the selected ones. Must be called after generateColumns.
	 * 
	 * @param orders
	 * @return the Order SQL clause.
	 */
	protected String generateOrders(JoinElement join, List<ReferenceLine> orders, boolean deleted) {
		init(deleted);
		StringBuilder result = new StringBuilder();
		if (orders != null) {
			for (ReferenceLine order: orders) {
				if (ReferenceLine.ORDERBY_DATE.equals(order.getCode()) && (entityInfo.updateCol != null)) {
					if (result.length() > 0) {
						result.append(mapper.fg.columnsep);
					}
					if (order.isFlaged()) {
						result.append(String.format(mapper.fg.orderdesc, join.getAlias() + '.' + entityInfo.updateCol));
					} else {
						result.append(String.format(mapper.fg.orderasc, join.getAlias() + '.' + entityInfo.updateCol));
					}
				} else if (ReferenceLine.ORDERBY_MUID.equals(order.getCode()) && (entityInfo.muidCol != null)) {
					if (result.length() > 0) {
						result.append(mapper.fg.columnsep);
					}
					if (order.isFlaged()) {
						result.append(String.format(mapper.fg.orderdesc, join.getAlias() + '.' + entityInfo.muidCol));
					} else {
						result.append(String.format(mapper.fg.orderasc, join.getAlias() + '.' + entityInfo.muidCol));
					}
				} else if (ReferenceLine.ORDERBY_ID.equals(order.getCode()) && (entityInfo.idCol != null)) {
					if (result.length() > 0) {
						result.append(mapper.fg.columnsep);
					}
					if (order.isFlaged()) {
						result.append(String.format(mapper.fg.orderdesc, join.getAlias() + '.' + entityInfo.idCol));
					} else {
						result.append(String.format(mapper.fg.orderasc, join.getAlias() + '.' + entityInfo.idCol));
					}
				} else {
					// Assume that the columns are included in the selected columns and generateColumns is called before generateOrders.
					String col = colNames.get(order.getCode());
					// Avoid usage of complex column in the order clause.
					// TODO Support order by complex columns (just remove constants and operators...).
					if ((col != null) && (col.indexOf('+') < 0)) {
						// we only sort by elements of the same domain and which are actually selected
						if (result.length() > 0) {
							result.append(mapper.fg.columnsep);
						}
						if (order.isFlaged()) {
							result.append(String.format(mapper.fg.orderdesc, col));
						} else {
							result.append(String.format(mapper.fg.orderasc, col));
						}
					}
				}
			}
		}
		return result.toString();
	}

	/**
	 * Create the SQL column name associated to the given ReferenceLine, with the corresponding joins clauses.. 
	 * 
	 * @param reference the Attribute reference line to convert.
	 * @param innerJoin if false the reference is computed through "left outer joins" and is then optional. 
	 * @return The SQL column representation.
	 */
	private String buildAttributeColName(ReferenceLine reference, boolean innerJoin, boolean deleted) {
		return buildAttributeColName(reference, entityInfo, joinTree, innerJoin, deleted);
	}
	
	private String buildAttributeColName(ReferenceLine reference, EntityInfo lastInfo, JoinElement join, boolean innerJoin, boolean deleted) {
		String currentCol = null;
		for (int i = 0; i < reference.size(); i++) {
			Element e = reference.get(i);
			if (e instanceof MetaDataAttribute) {
				// Manage non final references...
				currentCol = lastInfo.attributesCols.get(e.getCode());
				if (currentCol == null) {
					return null;
				}
				if (((i + 1) < reference.size()) && ((MetaDataAttribute) e).isLocalReference()) {
					MetaDataEntity entity = ((MetaDataAttribute) e).getRefEntity();
					if (entity == null) {
						///final node...
						break;
					}
					EntityInfo nei = mapper.getEntityInfo(entity);
					if (nei == null) {
						// The entity is not declared in the database.
						break;
					}
					JoinElement x = join.add(nei, join.getAlias() + '.' + currentCol, deleted);
					if (innerJoin) {
						x.setInner();
					}
					lastInfo = nei;
					join = x;
				} else {
					break;
				}
			} else {
				Activator.getInstance().debug("Error into SQL Mapper: A invalid reference line \"" + reference.getCode() + "\" contain " + e); //$NON-NLS-1$
				break; 
			}
		}
		// The result is the unique columns name (i.e. alias.COL_NAME).
		if (currentCol == null) {
			return null;
		}
		if (currentCol.contains(MapperSQLService.COLUMNPREFIX_PLACEHOLDERS)) {
			// This column name may be a complex SQL expression (like a function...) in that
			// case the alias must be placed in replacement of the ~ character. 
			return currentCol.replace(MapperSQLService.COLUMNPREFIX_PLACEHOLDERS, join.getAlias() + '.');
		}
		return join.getAlias() + '.' + currentCol;
	}

	private JoinElement getJoin(ReferenceLine ref, boolean innerJoin, boolean deleted) {
		JoinElement result = joinTree;
		EntityInfo lastInfo = entityInfo;
		for (Element e: ref) {
			if ((e instanceof MetaDataAttribute) && ((MetaDataAttribute) e).isLocalReference()) {
				String col = lastInfo.attributesCols.get(e.getCode());
				if (col == null) {
					return null;
				}
				MetaDataEntity entity = ((MetaDataAttribute) e).getRefEntity();
				if (entity == null) {
					return null;
				}
				lastInfo = mapper.getEntityInfo(entity);
				result = result.add(lastInfo, result.getAlias() + '.' + col, deleted);
				if (innerJoin) {
					result.setInner();
				}
			} else {
				break;
			}
		}
		return result;
	}

	/**
	 * @param entity the entity containing a subdivision.
	 * @param ei The entityInfo of thie entity.
	 * @param deleted false is deleted col must be tested.
	 * @return null if this entity does not contain a subdivision, or return the alias of the resursive table.
	 */
	private String getSubDivision(MetaDataEntity entity, EntityInfo ei, boolean deleted) {
		final MetaDataLink link = entity.getFirstRecursiveLink();
		if ((link == null) || (ei == null)) {
			return null;
		}
		final String alias = SQL_RECURCIVE_PREFIX + ei.table;
		if (ei.sql_subselect == null) {
			LinkInfo l = ei.links.get(link.getCode());
			if (l == null) {
				return null;
			}
			if ((ei.deleteCol == null) && (l.deleteCol == null)) {
				// selection independent from delete cols
				ei.sql_subselect = String.format(mapper.fg.rec_link, alias, ei.table, ei.idCol, l.table, l.sourceCol, l.destCol);
			} else if (deleted) {
				// Do not cache this request !
				addQueryContext(alias, String.format(mapper.fg.rec_link, alias, ei.table, ei.idCol, l.table, l.sourceCol, l.destCol));
				return alias;
			} else {
				final String linkDel;
				if (l.deleteCol == null) {
					linkDel = ""; //$NON-NLS-1$
				} else {
					linkDel = " where z." + l.deleteCol + mapper.fg.equaldelfalse; //$NON-NLS-1$
				}
				final String eDel;
				if (ei.deleteCol == null) {
					eDel = ""; //$NON-NLS-1$
				} else {
					eDel = " where r." + ei.deleteCol + mapper.fg.equaldelfalse; //$NON-NLS-1$
				}
				ei.sql_subselect = String.format(mapper.fg.rec_linkdel, alias, ei.table, ei.idCol, eDel, l.table, l.sourceCol, l.destCol, linkDel);
			}			
		}
		addQueryContext(alias, ei.sql_subselect);
		return alias;
	}

	/**
	 * This approach will generate a "non recursive" table of the subdivisions by expending all transitive relations and include a reflective relation x -> x
	 * to start with.
	 * 
	 * <p>
	 * Note: this could be simplified with an initialization with id -> id where ID is the actual targeted element, when it is known !
	 *  
	 * @param links The chain of links to proceed.
	 * @param where will content the where clause of the selection.
	 * @param lastJoinCol If required, get the entity latest ID column, with correct prefix. 
	 * @param endWithEntity if true the last join is targeting the latest entity table.
	 * @param deleted if false the deletion of intermediaries entities is checked.
	 * @return
	 */
	private JoinElement generateLinkJoins(List<MetaDataLink> links, StringBuilder where, StringBuilder lastJoinCol, boolean endWithEntity, boolean deleted) {
		MetaDataEntity e = links.get(0).getParent();
		EntityInfo ei = mapper.getEntityInfo(e);
		JoinElement result = null;
		String a = getSubDivision(e, ei, deleted);
		String parentCol = null;
		if (a != null) {
			result = new JoinElement(a, a, mapper.fg.dest);
			parentCol = a + '.' + mapper.fg.source;
		} else if (!deleted && (ei.deleteCol != null)) {
			result = new JoinElement("xa", ei.table + " xa", ei.idCol); //$NON-NLS-1$ //$NON-NLS-2$
			parentCol = "xa." + ei.idCol; //$NON-NLS-1$
			if (!where.isEmpty()) {
				where.append(mapper.fg.and);
			}
			where.append("xa."); //$NON-NLS-1$
			where.append(ei.deleteCol);
			where.append(mapper.fg.equaldelfalse);
		}
		JoinElement current = result;
		for (MetaDataLink link: links) {
			// If the current link is the subdivision link of the current entity,
			// then we have managed is with a recurvise conversion at the previous step, we must ignore it.
			if (!link.isRecursive()) { 
				LinkInfo li = ei.links.get(link.getCode());
				if (result == null) {
					result = new JoinElement("la", li.table + " la", li.sourceCol);
					parentCol = "la." + li.destCol;
					current = result;
					if (!deleted && (li.deleteCol != null)) {
						if (!where.isEmpty()) {
							where.append(mapper.fg.and);
						}
						where.append(li.deleteCol);
						where.append(mapper.fg.equaldelfalse);
					}
				} else {
					current = current.add(li, parentCol, deleted);
					current.setInner();
					parentCol = current.getAlias() + '.' + li.destCol;
				}
				e = link.getRefEntity();
				ei = mapper.getEntityInfo(e);
				if (ei == null) {
					// TODO support multi domain links...
					break;
				}
				a = getSubDivision(e, ei, deleted);
				if (a != null) {
					current = current.add(a, mapper.fg.dest, parentCol);
					current.setInner();
					parentCol = current.getAlias() + '.' + mapper.fg.source;
				} else if (!deleted && (ei.deleteCol != null)) {
					current = current.add(ei, parentCol, false);
					current.setInner();
					parentCol = current.getAlias() + '.' + ei.idCol;
				}
			}
		}
		if (endWithEntity && !parentCol.endsWith(ei.idCol)) {
			// Ensure that this join is on the last entity... 
			current = current.add(ei, parentCol, false);
			current.setInner();
			// If the lastJoinAlias is required then this is because be need to use it to refer to last entity id.
			if (lastJoinCol != null) {
				lastJoinCol.append(current.getAlias());
				lastJoinCol.append('.');
				lastJoinCol.append(ei.idCol);
			}
		} else if (lastJoinCol != null) {
			// The lastColJoin is the last "id" col used in chain. (the "target" of the last link, here).
			lastJoinCol.append(parentCol);
		}
		return result;
	}

	/**
	 * Generate an SQL where clause from a Criteria.
	 *  
	 * <p>
	 * This criteria must have been reduced, before to be passed to this method.
	 * 
	 * @param criteria The search criteria to compute.
	 * @param deleted False is the deletion flag must also be tested.
	 * @return a non null StringBuilder containing the conversion of the Criteria into a Where Clause.
	 */
	protected StringBuilder generateCriteria(ISearchCriteria criteria, boolean deleted) {
		init(deleted);
		return generateCriteria(joinTree, criteria, deleted);
	}
	
	protected StringBuilder generateCriteria(JoinElement join, ISearchCriteria criteria, boolean deleted) {
		if ((criteria != null) && !ConstantCriteria.TRUE.equals(criteria)) {
			criteria = getLocalCriteria(criteria);
			if (ConstantCriteria.FALSE.equals(criteria)) {
				return null;
			}
			if (ConstantCriteria.TRUE.equals(criteria)) {
				return generateCriteria(join, null, deleted, new StringBuilder());
			}
		}
		return generateCriteria(join, criteria, deleted, new StringBuilder());
	}
	
	/**
	 * Generate an SQL where clause from a Criteria.
	 *  
	 * <p>
	 * This criteria must have been reduced, before to be passed to this method.
	 * 
	 * @param entityInfo The EntityInfo from which the test start from.
	 * @param join The join corresponding to given EntityInfo.
	 * @param criteria The search criteria to compute.
	 * @param deleted False is the deletion flag must also be tested.
	 * @param result The SQL "where" clause, must be not null. 
	 * @return the "result" param.
	 */
	private StringBuilder generateCriteria(JoinElement join, ISearchCriteria criteria, boolean deleted, StringBuilder result) {
		init(deleted);
		if (criteria instanceof AfterCriteria) {
			result.append(String.format(mapper.fg.greater, 
				colNames.get(((AfterCriteria) criteria).getAttribute()),
				String.format(mapper.fg.datefunction, mapper.sdf.format(((AfterCriteria) criteria).getCalendar().getTime()))));
		} else if (criteria instanceof NotCriteria) {
			result.append(String.format(mapper.fg.not, generateCriteria(((NotCriteria) criteria).getCriteria(), true).toString()));
		} else if (criteria instanceof OrCriteria) {
			Iterator<ISearchCriteria> itt = ((OrCriteria) criteria).getCriterias().iterator();
			result.append(mapper.fg.parin);
			generateCriteria(join, itt.next(), true, result);
			while (itt.hasNext()) {
				result.append(mapper.fg.or);
				generateCriteria(join, itt.next(), true, result);
			}
			result.append(mapper.fg.parout);
		} else if (criteria instanceof AndCriteria) {
			Iterator<ISearchCriteria> itt = ((AndCriteria) criteria).getCriterias().iterator();
			result.append(mapper.fg.parin);
			generateCriteria(join, itt.next(), true, result);
			while (itt.hasNext()) {
				result.append(mapper.fg.and);
				generateCriteria(join, itt.next(), true, result);
			}
			result.append(mapper.fg.parout);
		} else if (criteria instanceof BeforeCriteria) {
			result.append(String.format(mapper.fg.lower, 
					colNames.get(((BeforeCriteria) criteria).getAttribute()),
					String.format(mapper.fg.datefunction, mapper.sdf.format(((BeforeCriteria) criteria).getCalendar().getTime()))));
		} else if (criteria instanceof BetweenCriteria) {
			result.append(mapper.fg.parin);
			result.append(String.format(mapper.fg.greater, 
					colNames.get(((BetweenCriteria) criteria).getAttribute()),
				String.format(mapper.fg.datefunction, mapper.sdf.format(((BetweenCriteria) criteria).getAfterCalendar().getTime()))));
			result.append(mapper.fg.and);
			result.append(String.format(mapper.fg.lower, 
					colNames.get(((BetweenCriteria) criteria).getAttribute()),
					String.format(mapper.fg.datefunction, mapper.sdf.format(((BetweenCriteria) criteria).getBeforeCalendar().getTime()))));
			result.append(mapper.fg.parout);
		} else if (criteria instanceof ConstantCriteria) {
			if (((ConstantCriteria) criteria).isValue()) {
				result.append(mapper.fg.true_cond);
			} else {
				result.append(mapper.fg.false_cond);
			}
		} else if (criteria instanceof ContainCriteria) {
			if (((ContainCriteria) criteria).isCasesensitive()) {
				result.append(String.format(mapper.fg.contain, 
						colNames.get(((ContainCriteria) criteria).getAttribute()),
						escape(((ContainCriteria) criteria).getValue())));
			} else {
				result.append(String.format(mapper.fg.contain, 
						String.format(mapper.fg.lowercase, colNames.get(((ContainCriteria) criteria).getAttribute())),
						escape(((ContainCriteria) criteria).getValue().toLowerCase())));
			}
		} else if (criteria instanceof DeletedCriteria) {
			if (((DeletedCriteria) criteria).getAttribute() == null) {
				if (entityInfo.deleteCol == null) {
					result.append(mapper.fg.false_cond);
				} else {
					result.append(mapper.fg.parin);
					result.append(join.getAlias());
					result.append('.');
					result.append(entityInfo.deleteCol);
					result.append(mapper.fg.equaldeltrue);
					result.append(mapper.fg.parout);
				}
			} else {
				ReferenceLine ref = getReference(((DeletedCriteria) criteria).getAttribute());
				EntityInfo ae = mapper.getEntityInfo(ref.getLastAttribute().getRefEntity());
				// The existence of the referenced entity has been checked during reduction of the criteria
				if (ae.deleteCol == null) {
					result.append(mapper.fg.false_cond);
				} else {
					result.append(mapper.fg.parin);
					result.append(getJoin(ref, true, true).getAlias());
					result.append('.');
					result.append(ae.deleteCol);
					result.append(mapper.fg.equaldeltrue);
					result.append(mapper.fg.parout);
				}
			}
		} else if (criteria instanceof EndCriteria) {
			if (((EndCriteria) criteria).isCasesensitive()) {
				result.append(String.format(mapper.fg.endwith, 
						colNames.get(((EndCriteria) criteria).getAttribute()),
						escape(((EndCriteria) criteria).getValue())));
			} else {
				result.append(String.format(mapper.fg.endwith, 
						String.format(mapper.fg.lowercase, colNames.get(((EndCriteria) criteria).getAttribute())),
						escape(((EndCriteria) criteria).getValue().toLowerCase())));
			}
		} else if (criteria instanceof AttributeEqualsCriteria) {
			if (((AttributeEqualsCriteria) criteria).isCasesensitive() || //
					getReference(((AttributeEqualsCriteria) criteria).getAttribute()).isNumericType()) {
				result.append(String.format(mapper.fg.equal,
						colNames.get(((AttributeEqualsCriteria) criteria).getAttribute()),
						colNames.get(((AttributeEqualsCriteria) criteria).getSecondAttribute())));
			} else {
				result.append(String.format(mapper.fg.equal,
						String.format(mapper.fg.lowercase, colNames.get(((AttributeEqualsCriteria) criteria).getAttribute())),
								String.format(mapper.fg.lowercase, colNames.get(((AttributeEqualsCriteria) criteria).getSecondAttribute()))));
			}
		} else if (criteria instanceof AttributeLowerCriteria) {
			result.append(String.format(mapper.fg.lower,
					colNames.get(((AttributeLowerCriteria) criteria).getAttribute()),
					colNames.get(((AttributeLowerCriteria) criteria).getSecondAttribute())));
		} else if (criteria instanceof AttributeLowerOrEqualsCriteria) {
			result.append(String.format(mapper.fg.lowerorequal,
					colNames.get(((AttributeLowerOrEqualsCriteria) criteria).getAttribute()),
					colNames.get(((AttributeLowerOrEqualsCriteria) criteria).getSecondAttribute())));
		} else if (criteria instanceof EqualCriteria) {
			if (getReference(((EqualCriteria) criteria).getAttribute()).isNumericType()) {
				if (((EqualCriteria) criteria).getIntval() != null) {
					result.append(String.format(mapper.fg.equal,
							colNames.get(((EqualCriteria) criteria).getAttribute()),
							((EqualCriteria) criteria).getIntval().toString()));
				} else {
					try {
						Integer.parseInt(((EqualCriteria) criteria).getValue());
						result.append(String.format(mapper.fg.equal,
								colNames.get(((EqualCriteria) criteria).getAttribute()),
								((EqualCriteria) criteria).getValue()));
					} catch (NumberFormatException e) {
						result.append(String.format(mapper.fg.equal,
								colNames.get(((EqualCriteria) criteria).getAttribute()),
								enquote(((EqualCriteria) criteria).getValue())));
					}
				}
			} else {
				String value = ((EqualCriteria) criteria).getValue();
				if (((EqualCriteria) criteria).getIntval() != null) {
					value = ((EqualCriteria) criteria).getIntval().toString();
				}
				if (((EqualCriteria) criteria).isCasesensitive()) {			
					result.append(String.format(mapper.fg.equal,
							colNames.get(((EqualCriteria) criteria).getAttribute()),
							enquote(value)));
				} else {			
					result.append(String.format(mapper.fg.equalignorecase,
							colNames.get(((EqualCriteria) criteria).getAttribute()),
						enquote(value.toUpperCase())));
				}
			}
		} else if (criteria instanceof GreaterStrictCriteria) {
			if (getReference(((GreaterStrictCriteria)criteria).getAttribute()).isNumericType()) {
				try {
					Integer.parseInt(((GreaterStrictCriteria) criteria).getValue());
					result.append(String.format(mapper.fg.greater, 
							colNames.get(((GreaterStrictCriteria) criteria).getAttribute()),
							((GreaterStrictCriteria) criteria).getValue()));
				} catch (NumberFormatException e) {
					result.append(String.format(mapper.fg.greater,
							colNames.get(((GreaterStrictCriteria) criteria).getAttribute()),
							enquote(((GreaterStrictCriteria) criteria).getValue())));
				}
			} else {
				result.append(String.format(mapper.fg.greater,
						colNames.get(((GreaterStrictCriteria) criteria).getAttribute()),
						enquote(((GreaterStrictCriteria) criteria).getValue())));
			}
		} else if (criteria instanceof GreaterThanCriteria) {
			if (getReference(((GreaterThanCriteria) criteria).getAttribute()).isNumericType()) {
				try {
					Integer.parseInt(((GreaterThanCriteria) criteria).getValue());
					result.append(String.format(mapper.fg.greaterorequal, 
							colNames.get(((GreaterThanCriteria) criteria).getAttribute()),
							((GreaterThanCriteria) criteria).getValue()));
				} catch (NumberFormatException e) {
					result.append(String.format(mapper.fg.greaterorequal,
							colNames.get(((GreaterThanCriteria) criteria).getAttribute()),
							enquote(((GreaterThanCriteria) criteria).getValue())));
				}
			} else {
				result.append(String.format(mapper.fg.greaterorequal,
						colNames.get(((GreaterThanCriteria) criteria).getAttribute()),
						enquote(((GreaterThanCriteria) criteria).getValue())));
			}
		} else if (criteria instanceof HasRightCriteria) {
			// Test that the value of the "attribute", a user ID, or the current selected user, is present in the list
			// of the users possessing the given right+param value.
			String table = "hrpc_" + hrpc++;
			// Use the hypothetic double link [profileRight] ---> [profile] --users--> [user]
			String sql = mapper.generateHashRightPrequery();
			if (sql == null) {
				result.append(mapper.fg.false_cond);
			} else {
				if (((HasRightCriteria) criteria).getRight() != null) {
					if (((HasRightCriteria) criteria).getParam() != null) {
						sql = String.format(sql, table, " = " + ((HasRightCriteria) criteria).getRight(), //$NON-NLS-1$
								" = " + ((HasRightCriteria) criteria).getParam()); //$NON-NLS-1$
					} else {
						sql = String.format(sql, table, " = " + ((HasRightCriteria) criteria).getRight(), //$NON-NLS-1$
								mapper.fg.isnullex);
					}
				} else {
					sql = String.format(sql, table, mapper.fg.isnullex, //
							" = " + ((HasRightCriteria) criteria).getParam()); //$NON-NLS-1$
				}
				addQueryContext(table, sql);
				String col;
				if (((HasRightCriteria) criteria).getAttribute() != null) {
					col = colNames.get(((HasRightCriteria) criteria).getAttribute());
				} else {
					col = join.getAlias() + '.' + entityInfo.idCol;
				}
				EntityInfo users = mapper.getEntityInfo(MetaDataEntity.loadEntity("user")); //$NON-NLS-1$
				if (users == null) {
					// We should get the user list from the foreign mapper...
					result.append(mapper.fg.false_cond);
				}
				LinkInfo userProfiles = users.links.get("profiles"); //$NON-NLS-1$
				if (userProfiles == null) {
					// We should get the profile list from the foreign mapper...
					result.append(mapper.fg.false_cond);
				}
				// Add a join on the User table (we do not test that eh users are deleted or not !!!)
				result.append(String.format(mapper.fg.inset, col, //
						String.format(mapper.fg.selectall, userProfiles.sourceCol, userProfiles.table + 
								String.format(mapper.fg.join_inner, table, "r", mapper.fg.id, userProfiles.destCol)))); //$NON-NLS-1$
			}
		} else if (criteria instanceof IdEqualCriteria) {
			result.append(String.format(mapper.fg.equal,  join.getAlias() + '.' + entityInfo.idCol, ((IdEqualCriteria) criteria).getId()));
		} else if (criteria instanceof IdGreaterThanCriteria) {
			result.append(String.format(mapper.fg.greaterorequal,  join.getAlias() + '.' + entityInfo.idCol, ((IdGreaterThanCriteria) criteria).getId()));	
		} else if (criteria instanceof IdGreaterStrictCriteria) {
			result.append(String.format(mapper.fg.greater,  join.getAlias() + '.' + entityInfo.idCol, ((IdGreaterStrictCriteria) criteria).getId()));	
		} else if (criteria instanceof IdLowerThanCriteria) {
			result.append(String.format(mapper.fg.lowerorequal,  join.getAlias() + '.' + entityInfo.idCol, ((IdLowerThanCriteria) criteria).getId()));	
		} else if (criteria instanceof IdLowerStrictCriteria) {
			result.append(String.format(mapper.fg.lower,  join.getAlias() + '.' + entityInfo.idCol, ((IdLowerStrictCriteria) criteria).getId()));				
		} else if (criteria instanceof IsNullCriteria) {
			result.append(String.format(mapper.fg.isnull, colNames.get(((IsNullCriteria) criteria).getAttribute())));
		} else if (criteria instanceof IsTrueCriteria) {
			result.append(String.format(mapper.fg.istrue, colNames.get(((IsTrueCriteria) criteria).getAttribute())));
		} else if (criteria instanceof LinkCriteria) {
			// Criteria using a link use a sub-selection and the "in" operator.
			// If some of the entities contain a subdivision (recursive) link this operation also imply a recursive SQL query.
			String attcn = colNames.get(((LinkCriteria) criteria).getAttribute());
			String code;
			if (attcn == null) {
				attcn = join.getAlias() + '.' + entityInfo.idCol;
				code = ((LinkCriteria) criteria).getLinkCode();
			} else {
				code = ((LinkCriteria) criteria).getAttribute() + '/' + ((LinkCriteria) criteria).getLinkCode();
			}
			StringBuilder subWhere = new StringBuilder();
			StringBuilder lastCol = new StringBuilder();
			JoinElement j = generateLinkJoins(getLinks(code), subWhere, lastCol, false, deleted);
			if (!subWhere.isEmpty()) {
				subWhere.append(mapper.fg.and);
			}
			subWhere.append(String.format(mapper.fg.equal, lastCol.toString(), Integer.toString(((LinkCriteria) criteria).getId())));
			result.append(String.format(mapper.fg.inset, attcn, 
					String.format(mapper.fg.select, j.getAlias() + '.' + j.getId(), j.toString(mapper), subWhere.toString())));
		} else if (criteria instanceof AbstractLinkTestCriteria) {
			String attcn = colNames.get(((AbstractLinkTestCriteria) criteria).getReference());
			String code;
			if (attcn == null) {
				attcn = join.getAlias() + '.' + entityInfo.idCol;
				code = ((AbstractLinkTestCriteria) criteria).getLinkCode();
			} else {
				code = ((AbstractLinkTestCriteria) criteria).getAttribute() + '/' + ((AbstractLinkTestCriteria) criteria).getLinkCode();
			}
			StringBuilder subWhere = new StringBuilder();
			JoinElement j = generateLinkJoins(getLinks(code), subWhere, null, true, deleted);
			// Follow up the join to the targeted attribute used for the final test...
			ReferenceLine refline = getLinkReference(code, ((AbstractLinkTestCriteria) criteria).getAttribute());
			EntityInfo ei = mapper.getEntityInfo(refline.getOriginEntity());
			JoinElement entityJoin = j.getLastet();
			String col = buildAttributeColName(refline, ei, entityJoin, false, deleted);
			if (!subWhere.isEmpty()) {
				subWhere.append(mapper.fg.and);
			}
			String scol = null;
			if (criteria instanceof IAttributesCriteria) {
				scol = colNames.get(((IAttributesCriteria) criteria).getSecondAttribute());
				// FIXME there is an ambiguity here !
				if (scol == null) {
					scol = buildAttributeColName(getLinkReference(code, ((IAttributesCriteria) criteria).getSecondAttribute()), //
						ei, entityJoin, false, deleted);
				}
			}
			String value = ((AbstractLinkTestCriteria) criteria).getValue();
			if (value != null) {
				if (refline.isNumericType()) {
					try {
						Integer.parseInt(value);
					} catch (NumberFormatException e) {
						try {
							Float.parseFloat(value);
						} catch (NumberFormatException ee) {
							value = enquote(((AbstractLinkTestCriteria) criteria).getValue());
						}
					}
				}
			}
			// Implement the actual SQL test depending on the real criteria type...
			if (criteria instanceof LinkEqualCriteria) {
				if (((LinkEqualCriteria) criteria).getSecondAttribute() != null) {
					if (((LinkEqualCriteria) criteria).isCasesensitive()) {
						subWhere.append(String.format(mapper.fg.equal, col, scol));
					} else {
						subWhere.append(String.format(mapper.fg.equal, String.format(mapper.fg.lowercase, col), String.format(mapper.fg.lowercase, scol)));
					}
				} else if (refline.isNumericType() || ((LinkEqualCriteria) criteria).isCasesensitive()) {
					subWhere.append(String.format(mapper.fg.equal, col, value));
				} else {
					subWhere.append(String.format(mapper.fg.equalignorecase, col, value.toUpperCase()));
				}
			} else if (criteria instanceof LinkContainCriteria) {
				if (((LinkContainCriteria) criteria).isCasesensitive()) {
					subWhere.append(String.format(mapper.fg.contain, col, escape(((LinkContainCriteria) criteria).getValue())));
				} else {
					subWhere.append(String.format(mapper.fg.contain, String.format(mapper.fg.lowercase, col),
							escape(((LinkContainCriteria) criteria).getValue().toLowerCase())));
				}
			} else if (criteria instanceof LinkEndCriteria) {
				if (((LinkEndCriteria) criteria).isCasesensitive()) {
					subWhere.append(String.format(mapper.fg.endwith, col, escape(((LinkEndCriteria) criteria).getValue())));
				} else {
					subWhere.append(String.format(mapper.fg.endwith, String.format(mapper.fg.lowercase, col),
							escape(((LinkEndCriteria) criteria).getValue().toLowerCase())));
				}
			} else if (criteria instanceof LinkGreaterStrictCriteria) {
				subWhere.append(String.format(mapper.fg.greater, col, value));
			} else if (criteria instanceof LinkGreaterThanCriteria) {
				subWhere.append(String.format(mapper.fg.greaterorequal, col, value));
			} else if (criteria instanceof LinkLowerStrictCriteria) {
				subWhere.append(String.format(mapper.fg.lower, col, value));
			} else if (criteria instanceof LinkLowerThanCriteria) {
				subWhere.append(String.format(mapper.fg.lowerorequal, col, value));
			} else if (criteria instanceof LinkStartCriteria) {
				if (((LinkStartCriteria) criteria).isCasesensitive()) {
					subWhere.append(String.format(mapper.fg.startwith, col, escape(((LinkStartCriteria) criteria).getValue())));
				} else {
					subWhere.append(String.format(mapper.fg.startwith, String.format(mapper.fg.lowercase, col), escape(((LinkStartCriteria) criteria).getValue().toLowerCase())));
				}
			}
			result.append(String.format(mapper.fg.inset, attcn, 
					String.format(mapper.fg.select, j.getAlias() + j.getId(), j.toString(mapper), subWhere.toString())));
		} else if (criteria instanceof LowerStrictCriteria) {
			if (getReference(((LowerStrictCriteria) criteria).getAttribute()).isNumericType()) {
				try {
					Integer.parseInt(((LowerStrictCriteria) criteria).getValue());
					result.append(String.format(mapper.fg.lower, 
							colNames.get(((LowerStrictCriteria) criteria).getAttribute()),
							((LowerStrictCriteria) criteria).getValue()));
				} catch (NumberFormatException e) {
					result.append(String.format(mapper.fg.lower,
							colNames.get(((LowerStrictCriteria) criteria).getAttribute()),
							enquote(((LowerStrictCriteria) criteria).getValue())));
				}
			} else {
				result.append(String.format(mapper.fg.lower,
						colNames.get(((LowerStrictCriteria) criteria).getAttribute()),
						enquote(((LowerStrictCriteria) criteria).getValue())));
			}
		} else if (criteria instanceof LowerThanCriteria) {
			if (getReference(((LowerThanCriteria) criteria).getAttribute()).isNumericType()) {
				try {
					Integer.parseInt(((LowerThanCriteria) criteria).getValue());
					result.append(String.format(mapper.fg.lowerorequal, 
							colNames.get(((LowerThanCriteria) criteria).getAttribute()),
							((LowerThanCriteria) criteria).getValue()));
				} catch (NumberFormatException e) {
					result.append(String.format(mapper.fg.lowerorequal,
							colNames.get(((LowerThanCriteria) criteria).getAttribute()),
							enquote(((LowerThanCriteria) criteria).getValue())));
				}
			} else {
				result.append(String.format(mapper.fg.lowerorequal,
						colNames.get(((LowerThanCriteria) criteria).getAttribute()),
						enquote(((LowerThanCriteria) criteria).getValue())));
			}
		} else if (criteria instanceof PreGeneratedCriteria) {
			result.append(((PreGeneratedCriteria) criteria).getSql());
		} else if (criteria instanceof StartCriteria) {
			if (((StartCriteria) criteria).isCasesensitive()) {
				result.append(String.format(mapper.fg.startwith, 
						colNames.get(((StartCriteria) criteria).getAttribute()),
						escape(((StartCriteria) criteria).getValue())));
			} else {
				result.append(String.format(mapper.fg.startwith, 
						String.format(mapper.fg.lowercase, colNames.get(((StartCriteria) criteria).getAttribute())),
						escape(((StartCriteria) criteria).getValue().toLowerCase())));
			}
		} else if (criteria instanceof SubstCriteria) {
			Activator.getInstance().debug(Messages.MapperSQLService_InternalError_CriteriaNotReduced, new ResourceException(Status.SERVER_ERROR_INTERNAL));
		} else if (criteria instanceof ChangedCriteria) {
			String dateCol;
			String attribute = ((ChangedCriteria) criteria).getAttribute();
			if (attribute == null) {
				dateCol = join.getAlias() + '.' + entityInfo.updateCol;
			} else {
				ReferenceLine ref = getReference(attribute);
				EntityInfo ae = mapper.getEntityInfo(ref.getLastAttribute().getRefEntity());
				if (ae.updateCol == null) {
					dateCol = null;
				} else {
					dateCol = getJoin(ref, true, deleted).getAlias() + '.' + ae.updateCol;
				}
			}
			if (dateCol == null) {
				result.append(mapper.fg.false_cond);
			} else {
				result.append(mapper.fg.parin);
				result.append(String.format(mapper.fg.greater, dateCol,
						String.format(mapper.fg.datefunction, mapper.sdf.format(((ChangedCriteria) criteria).getAfterCalendar().getTime()))));
				result.append(mapper.fg.and);
				result.append(String.format(mapper.fg.lower, dateCol,
						String.format(mapper.fg.datefunction, mapper.sdf.format(((ChangedCriteria) criteria).getBeforeCalendar().getTime()))));			
				result.append(mapper.fg.parout);
			}
		} else if (criteria instanceof InListCriteria) {
			String col;
			if (((InListCriteria) criteria).getAttribute() == null) {
				col = join.getAlias() + '.' + entityInfo.idCol;
			} else {
				col = colNames.get(((InListCriteria) criteria).getAttribute());
			}
			result.append(String.format(mapper.fg.inset,  col, ((InListCriteria) criteria).getIds(mapper.fg.columnsep)));	
		} else if (criteria instanceof ChangedByCriteria) {
			String muidCol;
			String attribute = ((ChangedByCriteria) criteria).getAttribute();
			if (attribute == null) {
				muidCol = join.getAlias() + '.' + entityInfo.muidCol;
			} else {
				ReferenceLine ref = getReference(attribute);
				EntityInfo ae = mapper.getEntityInfo(ref.getLastAttribute().getRefEntity());
				if (ae.muidCol == null) {
					muidCol = null;
				} else {
					muidCol = getJoin(ref, true, deleted).getAlias() + '.' + ae.muidCol;
				}
			}
			if (muidCol == null) {
				result.append(mapper.fg.false_cond);
			} else {
				int uid = ((ChangedByCriteria) criteria).getUid();
				if ((uid <= 0) && (getCurrentUser() != null)) {
					uid = getCurrentUser().getId();
				}
				if (uid <= 0) {
					result.append(mapper.fg.false_cond);
				} else {
					result.append(String.format(mapper.fg.equal, muidCol, String.valueOf(uid)));
				}
			}
		} else if (criteria != null) {
			Activator.getInstance().debug(String.format(Messages.MapperSQLService_UnknownCriteria, criteria.toString(), criteria.getClass().getName()));
		}
		
		// Add the not deleted test to the where clause.
		if (!deleted && (entityInfo.deleteCol != null) && //
				// if and only if the selected item is not already a join (the test is added to the join).
				(join == joinTree)) {
			if (mapper.fg.true_cond.equals(result.toString())) {
				result.setLength(0);
			} else if (result.length() > 0) {
				result.append(mapper.fg.and);
			}
			result.append(join.getAlias());
			result.append('.');
			result.append(entityInfo.deleteCol);
			result.append(mapper.fg.equaldelfalse);
		}
		return result;
	}
	
	/**
	 * Return a local version of the given Criteria (Execute remote tests if possible).
	 * @param criteria
	 * @param context
	 * @return
	 */
	private ISearchCriteria getLocalCriteria(ISearchCriteria criteria) {
		if (isMapperUnique()) {
			return criteria;
		}
		return mapper.completeForeignCriteria(criteria, this);
	}

	/**
	 * Add a now part of SQL query to be added to the definitive SQL Query.
	 * 
	 * @param key An unique key identifying this SQL code.
	 * @param query The SQL code which will be added before the final query.
	 */
	protected void addQueryContext(String key, String query) {
		queryContextes.put(key, query);
	}
	
	/**
	 * Format the final query.
	 * 
	 * @param format
	 * @param objects
	 * @return
	 */
	protected String formatQuery(String format, Object... objects) {
		StringBuilder sb = new StringBuilder();
		boolean first = true;
		for (String q: queryContextes.values()) {
			if (first) {
				first = false;
				sb.append(mapper.fg.rec_first);
			} else {
				sb.append(mapper.fg.rec_sub);
			}
			sb.append(q);
			sb.append(' ');
		}
		sb.append(String.format(format, objects));
		return sb.toString();
	}
}
