package com.arcadsoftware.metadata.sql;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

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
import com.arcadsoftware.metadata.criteria.UnlinkCriteria;
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
 * TODO Allow to define "cachable" contextes to be able to optinize the SQL request generation. 
 * 
 * @author ARCAD Software
 */
public class SQLCriteriaContext extends CriteriaContextBasic {

	private static final String SQL_JAVA_PREFIX = "j_"; //$NON-NLS-1$
	private static final String SQL_JAVA_CRYPT_PREFIX = "z_"; //$NON-NLS-1$
	private static final String SQL_DATECOL = SQL_JAVA_PREFIX + "date"; //$NON-NLS-1$
	private static final String SQL_DELETECOL = SQL_JAVA_PREFIX + "deleted"; //$NON-NLS-1$

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
	public boolean isValid() {
		return entityInfo != null;
	}
	
	public EntityInfo getEntityInfo() {
		return entityInfo;
	}
	
	public String generateJoins(boolean deleted) {
		init(deleted);
		return joinTree.toString(mapper);
	}
	
	private void init(boolean deleted) {
		if (joinTree == null) {
			joinTree = new JoinElement(MapperSQLService.DEFAULT_TABLEALIAS, String.format(mapper.fg.tablealias, entityInfo.table, MapperSQLService.DEFAULT_TABLEALIAS));
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
	protected String escape(String value) {
		return mapper.esc.escape(value);
	}

	/**
	 * If the Form clause require a dedicated initialization, it must be called before any generate methods !
	 * 
	 * @param sql
	 */
	public void initJoinTree(String alias, String sql, boolean deleted) {
		if (joinTree == null) {
			joinTree = new JoinElement(alias, sql);
			for (ReferenceLine rf: getReferences()) {
				String code = rf.getCode();
				if (!colNames.containsKey(code)) {
					// We use left outer join for columns used in where clause.
					String col = buildAttributeColName(rf, false, deleted);
					if (col != null) {
						colNames.put(code, col);
					}
				}
			}
		} else {
			Activator.getInstance().debug("Error into SQL Mapper: the Form clause is initialized after a generation is done..."); //$NON-NLS-1$
		}
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
			prefix.append(code.replace('.', '_'));
		}
		if (suffix != null) {
			prefix.append(suffix);
		}
		return prefix.toString();
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
	public StringBuilder generateColumns(List<ReferenceLine> attributes, boolean deleted) {
		init(deleted);
		StringBuilder result = new StringBuilder(MapperSQLService.DEFAULT_TABLEALIAS);
		result.append('.');
		result.append(entityInfo.idCol);
		result.append(mapper.fg.asid);
		for (ReferenceLine att: attributes) {
			String cn = buildAttributeColName(att, false, deleted);
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
		if (entityInfo.updateCol != null) {
			result.append(mapper.fg.columnsep);
			result.append(MapperSQLService.DEFAULT_TABLEALIAS);
			result.append('.');
			result.append(entityInfo.updateCol);
			result.append(mapper.fg.as);
			result.append(SQL_DATECOL);
		}
		if ((entityInfo.deleteCol != null) && deleted) {
			result.append(mapper.fg.columnsep);
			result.append(MapperSQLService.DEFAULT_TABLEALIAS);
			result.append('.');
			result.append(entityInfo.deleteCol);
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
	 * @param orders
	 * @return the Order SQL clause.
	 */
	public String generateOrders(List<ReferenceLine> orders, boolean deleted) {
		init(deleted);
		StringBuilder result = new StringBuilder();
		if (orders != null) {
			for(ReferenceLine order: orders) {
				String col = colNames.get(order.getCode());
				// Avoid usage of complex column in the order clause.
				// TODO Support order by complex columns (just remove constants).
				if ((col != null) && (col.indexOf('+') < 0)) {
					// we only sort by elements of the same domain and which are actually selected
					if (result.length() > 0) {
						result.append(mapper.fg.columnsep);
					}
					if (order.isFlaged()) { //$NON-NLS-1$
						result.append(String.format(mapper.fg.orderdesc, col));
					} else {
						result.append(String.format(mapper.fg.orderasc, col));
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
	protected String buildAttributeColName(ReferenceLine reference, boolean innerJoin, boolean deleted) {
		JoinElement j = joinTree;
		EntityInfo lastInfo = entityInfo;
		String currentCol = null;
		String code = null;
		for (int i = 0; i < reference.size(); i++) {
			Element e = reference.get(i);
			if (e instanceof MetaDataAttribute) {
				// Manage non final references...
				currentCol = lastInfo.attributesCols.get(code);
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
					JoinElement x = j.add(nei, j.getAlias() + '.' + currentCol, deleted);
					if (innerJoin) {
						x.setInner();
					}
					lastInfo = nei;
					j = x;
				} else {
					break;
				}
			} else {
				Activator.getInstance().debug("Error into SQL Mapper: A invalid reference line contain " + e); //$NON-NLS-1$
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
			return currentCol.replace(MapperSQLService.COLUMNPREFIX_PLACEHOLDERS, j.getAlias() + '.');
		}
		return j.getAlias() + '.' + currentCol;
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
				result = result.add(lastInfo, result.getAlias() + col, deleted);
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
	 * Generate an SQL where clause from a Criteria.
	 *  
	 * <p>
	 * This criteria must have been reduced, before to be passed to this method.
	 * 
	 * @param entityInfo The base Entity SQL informations.
	 * @param criteria The search criteria to compute.
	 * @param context The context object used to reduce the criteria.
	 * @param colNames The used colons names prefix with their table alias. 
	 * @param joinMap The map of join to add to the request.
	 * @return a non null StringBuilder containing the conversion of the Criteria into a Where Clause.
	 */
	protected StringBuilder generateCriteria(ISearchCriteria criteria, boolean deleted) {
		if ((criteria != null) && !ConstantCriteria.TRUE.equals(criteria)) {
			criteria = getLocalCriteria(criteria);
			if (ConstantCriteria.FALSE.equals(criteria)) {
				return null;
			}
			if (ConstantCriteria.TRUE.equals(criteria)) {
				return generateCriteria(null, deleted, new StringBuilder());
			}
		}
		return generateCriteria(criteria, deleted, new StringBuilder());
	}
	
	/**
	 * Generate an SQL where clause from a Criteria.
	 *  
	 * <p>
	 * This criteria must have been reduced, before to be passed to this method.
	 * 
	 * @param criteria The search criteria to compute.
	 * @param deleted False is the deletion flag must also be tested.
	 * @param result The SQL "where" clause, must be not null. 
	 * @return the "result" param.
	 */
	protected StringBuilder generateCriteria(ISearchCriteria criteria, boolean deleted, StringBuilder result) {
		if (criteria instanceof AfterCriteria) {
			result.append(String.format(mapper.fg.greater, 
				colNames.get(((AfterCriteria) criteria).getAttribute()),
				String.format(mapper.fg.datefunction, mapper.sdf.format(((AfterCriteria) criteria).getCalendar().getTime()))));
		} else if (criteria instanceof NotCriteria) {
			result.append(String.format(mapper.fg.not, generateCriteria(((NotCriteria) criteria).getCriteria(), true).toString()));
		} else if (criteria instanceof OrCriteria) {
			Iterator<ISearchCriteria> itt = ((OrCriteria) criteria).getCriterias().iterator();
			result.append(mapper.fg.parin);
			generateCriteria(itt.next(), true, result);
			while (itt.hasNext()) {
				result.append(mapper.fg.or);
				generateCriteria(itt.next(), true, result);
			}
			result.append(mapper.fg.parout);
		} else if (criteria instanceof AndCriteria) {
			Iterator<ISearchCriteria> itt = ((AndCriteria) criteria).getCriterias().iterator();
			result.append(mapper.fg.parin);
			generateCriteria(itt.next(), true, result);
			while (itt.hasNext()) {
				result.append(mapper.fg.and);
				generateCriteria(itt.next(), true, result);
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
					result.append(MapperSQLService.DEFAULT_TABLEALIAS);
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
			if (((AttributeEqualsCriteria) criteria).isCasesensitive()) {			
				result.append(String.format(mapper.fg.equal,
						colNames.get(((AttributeEqualsCriteria) criteria).getAttribute()),
						colNames.get(((AttributeEqualsCriteria) criteria).getSecondAttribute())));
			} else {
				result.append(String.format(mapper.fg.equal,
						String.format(mapper.fg.lowercase,colNames.get(((AttributeEqualsCriteria) criteria).getAttribute())),
								String.format(mapper.fg.lowercase,colNames.get(((AttributeEqualsCriteria) criteria).getSecondAttribute()))));
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
					col = MapperSQLService.DEFAULT_TABLEALIAS + '.' + entityInfo.idCol;
				}
				EntityInfo users = mapper.getEntityInfo(MetaDataEntity.loadEntity("user")); //$NON-NLS-1$
				if (users == null) {
					// TODO get the user list from the foreign mappser...
					result.append(mapper.fg.false_cond);
				}
				LinkInfo userProfiles = users.links.get("profiles");
				if (userProfiles == null) {
					// TODO get the user list from the foreign mappser...
					result.append(mapper.fg.false_cond);
				}
				// Add a join on the User table (we do not test that eh users are deleted or not !!!)
				result.append(String.format(mapper.fg.inset, col, //
						String.format(mapper.fg.selectall, userProfiles.sourceCol, userProfiles.table + 
								String.format(mapper.fg.join_inner, table, "r", mapper.fg.id, userProfiles.destCol))));
			}
		} else if (criteria instanceof IdEqualCriteria) {
			result.append(String.format(mapper.fg.equal,  getIdCol(), ((IdEqualCriteria) criteria).getId()));
		} else if (criteria instanceof IdGreaterThanCriteria) {
			result.append(String.format(mapper.fg.greaterorequal,  getIdCol(), ((IdGreaterThanCriteria) criteria).getId()));	
		} else if (criteria instanceof IdGreaterStrictCriteria) {
			result.append(String.format(mapper.fg.greater,  getIdCol(), ((IdGreaterStrictCriteria) criteria).getId()));	
		} else if (criteria instanceof IdLowerThanCriteria) {
			result.append(String.format(mapper.fg.lowerorequal,  getIdCol(), ((IdLowerThanCriteria) criteria).getId()));	
		} else if (criteria instanceof IdLowerStrictCriteria) {
			result.append(String.format(mapper.fg.lower,  getIdCol(), ((IdLowerStrictCriteria) criteria).getId()));				
		} else if (criteria instanceof IsNullCriteria) {
			result.append(String.format(mapper.fg.isnull, colNames.get(((IsNullCriteria) criteria).getAttribute())));
		} else if (criteria instanceof IsTrueCriteria) {
			result.append(String.format(mapper.fg.istrue, colNames.get(((IsTrueCriteria) criteria).getAttribute())));
		} else if (criteria instanceof LinkCriteria) {
			
			/*
			
			
			ref in (select l1.source 
					from L1 
  				         join L2 on (l1.dest = l2.source and l2.deleted = 0)   + join sur l'entité deleted =0
				         join L3 on (l2.dest = l3.source and l3.deleted = 0)   
					     join E4 on (l2.dest =  e4.id and e4.deleted = 0)
					where (L1.deleted = 0)
					  and E4.a = 'x'
					)
			
			
			*/
			
			
			// Prepare the link subselection...
			
			// --> Dans le sub-select/with les jointure sont inner !!!
			
			
			
			// Use the col generated for the test (or the id col if it is null !)
			String attcn = colNames.get(((LinkCriteria) criteria).getAttribute());
			if (attcn == null) {
				attcn = getIdCol();
			}
			
			
			
			// Il faut quand même des left outer join pour que les test disjonctif fonctionnent.
			// la listes des incident nomé "X" et dont le "owner" est aussi un des "intervenants":
			// select from "incident" where ("code" = 'X') and (linked through "intervenents" to "owner")
			// ==
			// select from "incident" where ("code" = 'X') and ("owner" in (select id from "intervenant"... ))	
			
			
			
			
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
					joinMap.add(alias, String.format(mapper.fg.joinref, l.table, alias, l.sourceCol, MapperSQLService.DEFAULT_TABLEALIAS, entityInfo.idCol));
				}
			} else {
				alias = getAlias(((LinkCriteria) criteria).getAttribute(), lc, "_a"); //$NON-NLS-1$
				l = mapper.getEntityInfo(getReference(((LinkCriteria) criteria).getAttribute()).getLastAttribute().getRefEntity()).links.get(lc);
				if (joinMap.doNotExists(alias)) {
					joinMap.add(alias, String.format(mapper.fg.join, l.table, alias, l.sourceCol, attcn));
				}
				// On ne teste pas que l'entité cible ne soit pas supprimée.
				// La cible est référencée et son id est passé en paramètre. Même si elle supprimée, on l'utilise expressément
				// donc on doit en tenir compte comme un résultat valide.  
			}
			if (l == null) {
				result.append(mapper.fg.false_cond);
			} else {
				// prise en compte des association avec suppression logique.
				if (l.deleteCol != null) {
					result.append(mapper.fg.parin);
				}
				result.append(String.format(mapper.fg.equal, alias + '.' + l.destCol, Integer.toString(((LinkCriteria) criteria).getId())));
				if (l.deleteCol != null) {
					result.append(mapper.fg.and);
					result.append(alias);
					result.append('.');
					result.append(l.deleteCol);
					result.append(mapper.fg.equaldelfalse);
					result.append(mapper.fg.parout);
				}
			}
			
			
			
		} else if (criteria instanceof UnlinkCriteria) {
			
			
			String attcn = colNames.get(((UnlinkCriteria) criteria).getAttribute());
			LinkInfo l;
			if (attcn == null) {
				attcn = MapperSQLService.DEFAULT_TABLEALIAS + '.' + entityInfo.idCol;
				l = entityInfo.links.get(((UnlinkCriteria) criteria).getLinkCode());
			} else {
				l = mapper.getEntityInfo(getReference(((UnlinkCriteria) criteria).getAttribute()).getLastAttribute().getRefEntity()).links.get(((UnlinkCriteria) criteria).getLinkCode());
			}
			if (l == null) {
				result.append(mapper.fg.false_cond);
			} else {
				StringBuilder condition = new StringBuilder();
				int id = ((UnlinkCriteria) criteria).getId();
				if (id > 0) {
					condition.append(String.format(mapper.fg.equal, l.destCol, Integer.toString(id)));
				}
				if (l.deleteCol != null) {
					if (id > 0) {
						condition.append(mapper.fg.and);
					}
					condition.append(l.deleteCol);
					condition.append(mapper.fg.equaldelfalse);
				}
				if (condition.length() == 0) {
					condition.append(mapper.fg.true_cond);
				}
				result.append(String.format(mapper.fg.notintoselect, attcn, l.sourceCol, l.table, condition.toString()));
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
					link = (MetaDataLink) getEntity().getLink(lc);
					if (joinMap.doNotExists(alias)) {
						joinMap.add(alias, String.format(mapper.fg.joinref, l.table, alias, l.sourceCol, MapperSQLService.DEFAULT_TABLEALIAS, entityInfo.idCol));
					}
				}
			} else {
				alias = getAlias(((AbstractLinkTestCriteria) criteria).getReference(), lc, "_a"); //$NON-NLS-1$
				MetaDataEntity refEntity = getReference(((AbstractLinkTestCriteria) criteria).getReference()).getLastAttribute().getRefEntity();
				l = mapper.getEntityInfo(refEntity).links.get(lc);
				if (l != null) {
					link = (MetaDataLink) refEntity.getLink(lc);
					if (joinMap.doNotExists(alias)) {
						joinMap.add(alias, String.format(mapper.fg.join, l.table, alias, l.sourceCol, refcn));
					}
				}
			}
			if ((l == null) || (link == null)) {
				result.append(mapper.fg.false_cond);
			} else {
				EntityInfo ei = mapper.getEntityInfo(link.getRefEntity());
				// Specific alias for this reference table.
				String refalias = "d_" + alias; //$NON-NLS-1$
				if (joinMap.doNotExists(refalias)) {
					joinMap.add(refalias, String.format(mapper.fg.joinref, ei.table, refalias, ei.idCol, alias, l.destCol));
				}
				// Taking into account associations with logical deletion.
				if ((l.deleteCol != null) || (ei.deleteCol != null)) {
					result.append(mapper.fg.parin);
				}
				String attcn = buildAttributeColName(ei, getReference(link, ((AbstractLinkTestCriteria) criteria).getAttribute()), joinMap, refalias);
				// Implement the actual SQL test depending on the real criteria type...
				if (criteria instanceof LinkEqualCriteria) {
					if (((LinkEqualCriteria) criteria).getSecondAttribute() != null) {
						if (((LinkEqualCriteria) criteria).isCasesensitive()) {
							result.append(String.format(mapper.fg.equal, attcn, colNames.get(((LinkEqualCriteria) criteria).getSecondAttribute())));
						} else {
							result.append(String.format(mapper.fg.equal, String.format(mapper.fg.lowercase, attcn),
									String.format(mapper.fg.lowercase, colNames.get(((LinkEqualCriteria) criteria).getSecondAttribute()))));
						}
					} else if (getReference(link, ((LinkEqualCriteria) criteria).getAttribute()).isNumericType()) {
						try {
							Integer.parseInt(((LinkEqualCriteria) criteria).getValue());
							result.append(String.format(mapper.fg.equal, attcn, ((LinkEqualCriteria) criteria).getValue()));
						} catch (NumberFormatException e) {
							result.append(String.format(mapper.fg.equal, attcn, enquote(((LinkEqualCriteria) criteria).getValue())));
						}
					} else if (((LinkEqualCriteria) criteria).isCasesensitive()) {
						result.append(String.format(mapper.fg.equal, attcn, enquote(((LinkEqualCriteria) criteria).getValue())));
					} else {
						result.append(String.format(mapper.fg.equalignorecase, attcn, enquote(((LinkEqualCriteria) criteria).getValue().toUpperCase())));
					}
				} else if (criteria instanceof LinkContainCriteria) {
					if (((LinkContainCriteria) criteria).isCasesensitive()) {
						result.append(String.format(mapper.fg.contain, attcn, escape(((LinkContainCriteria) criteria).getValue())));
					} else {
						result.append(String.format(mapper.fg.contain, String.format(mapper.fg.lowercase, attcn),
								escape(((LinkContainCriteria) criteria).getValue().toLowerCase())));
					}
				} else if (criteria instanceof LinkEndCriteria) {
					if (((LinkEndCriteria) criteria).isCasesensitive()) {
						result.append(String.format(mapper.fg.endwith, attcn, escape(((LinkEndCriteria) criteria).getValue())));
					} else {
						result.append(String.format(mapper.fg.endwith, String.format(mapper.fg.lowercase, attcn),
								escape(((LinkEndCriteria) criteria).getValue().toLowerCase())));
					}
				} else if (criteria instanceof LinkGreaterStrictCriteria) {
					if (getReference(link, ((LinkGreaterStrictCriteria) criteria).getAttribute()).isNumericType()) {
						try {
							Integer.parseInt(((LinkGreaterStrictCriteria) criteria).getValue());
							result.append(String.format(mapper.fg.greater, attcn, ((LinkGreaterStrictCriteria) criteria).getValue()));
						} catch (NumberFormatException e) {
							result.append(String.format(mapper.fg.greater, attcn, enquote(((LinkGreaterStrictCriteria) criteria).getValue())));
						}
					} else {
						result.append(String.format(mapper.fg.greater, attcn, enquote(((LinkGreaterStrictCriteria) criteria).getValue())));
					}
				} else if (criteria instanceof LinkGreaterThanCriteria) {
					if (getReference(link, ((LinkGreaterThanCriteria) criteria).getAttribute()).isNumericType()) {
						try {
							Integer.parseInt(((LinkGreaterThanCriteria) criteria).getValue());
							result.append(String.format(mapper.fg.greaterorequal, attcn, ((LinkGreaterThanCriteria) criteria).getValue()));
						} catch (NumberFormatException e) {
							result.append(String.format(mapper.fg.greaterorequal, attcn, enquote(((LinkGreaterThanCriteria) criteria).getValue())));
						}
					} else {
						result.append(String.format(mapper.fg.greaterorequal, attcn, enquote(((LinkGreaterThanCriteria) criteria).getValue())));
					}
				} else if (criteria instanceof LinkLowerStrictCriteria) {
					if (getReference(link, ((LinkLowerStrictCriteria) criteria).getAttribute()).isNumericType()) {
						try {
							Integer.parseInt(((LinkLowerStrictCriteria) criteria).getValue());
							result.append(String.format(mapper.fg.lower, attcn, ((LinkLowerStrictCriteria) criteria).getValue()));
						} catch (NumberFormatException e) {
							result.append(String.format(mapper.fg.lower, attcn, enquote(((LinkLowerStrictCriteria) criteria).getValue())));
						}
					} else {
						result.append(String.format(mapper.fg.lower, attcn, enquote(((LinkLowerStrictCriteria) criteria).getValue())));
					}
				} else if (criteria instanceof LinkLowerThanCriteria) {
					if (getReference(link, ((LinkLowerThanCriteria) criteria).getAttribute()).isNumericType()) {
						try {
							Integer.parseInt(((LinkLowerThanCriteria) criteria).getValue());
							result.append(String.format(mapper.fg.lowerorequal, attcn, ((LinkLowerThanCriteria) criteria).getValue()));
						} catch (NumberFormatException e) {
							result.append(String.format(mapper.fg.lowerorequal, attcn, enquote(((LinkLowerThanCriteria) criteria).getValue())));
						}
					} else {
						result.append(String.format(mapper.fg.lowerorequal, attcn, enquote(((LinkLowerThanCriteria) criteria).getValue())));
					}
				} else if (criteria instanceof LinkStartCriteria) {
					if (((LinkStartCriteria) criteria).isCasesensitive()) {
						result.append(String.format(mapper.fg.startwith, attcn, escape(((LinkStartCriteria) criteria).getValue())));
					} else {
						result.append(String.format(mapper.fg.startwith, String.format(mapper.fg.lowercase, attcn), escape(((LinkStartCriteria) criteria).getValue().toLowerCase())));
					}
				}
				if (ei.deleteCol != null) {
					result.append(mapper.fg.and);
					result.append(refalias);
					result.append('.');
					result.append(ei.deleteCol);
					result.append(mapper.fg.equaldelfalse);
				}
				if (l.deleteCol != null) {
					result.append(mapper.fg.and);
					result.append(alias);
					result.append('.');
					result.append(l.deleteCol);
					result.append(mapper.fg.equaldelfalse);
				}
				if ((l.deleteCol != null) || (ei.deleteCol != null)) {
					result.append(mapper.fg.parout);
				}
			}
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
				dateCol = MapperSQLService.DEFAULT_TABLEALIAS + '.' + entityInfo.updateCol;
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
				col = MapperSQLService.DEFAULT_TABLEALIAS + '.' + entityInfo.idCol;
			} else {
				col = colNames.get(((InListCriteria) criteria).getAttribute());
			}
			result.append(String.format(mapper.fg.inset,  col, ((InListCriteria) criteria).getIds(mapper.fg.columnsep)));	
		} else if (criteria != null) {
			Activator.getInstance().debug(String.format(Messages.MapperSQLService_UnknownCriteria, criteria.toString(), criteria.getClass().getName()));
		}
		// Add the not deleted test to the where clause.
		if (!deleted && (entityInfo.deleteCol != null)) {
			if (result.length() > 0) {
				result.append(mapper.fg.and);
			}
			result.append(MapperSQLService.DEFAULT_TABLEALIAS);
			result.append('.');
			result.append(entityInfo.deleteCol);
			result.append(mapper.fg.equaldelfalse);
		}
		return result;
	}

	private String getIdCol() {
		return MapperSQLService.DEFAULT_TABLEALIAS + '.' + entityInfo.idCol;
	}
	
	/**
	 * Return a local version of the given Criteria (Execute remote tests if possible).
	 * @param criteria
	 * @param context
	 * @return
	 */
	protected ISearchCriteria getLocalCriteria(ISearchCriteria criteria) {
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
	public void addQueryContext(String key, String query) {
		queryContextes.put(key, query);
	}
	
	/**
	 * Format the final query.
	 * 
	 * @param format
	 * @param objects
	 * @return
	 */
	public String formatQuery(String format, Object... objects) {
		StringBuilder sb = new StringBuilder();
		for (String q: queryContextes.values()) {
			sb.append(q);
			sb.append(' ');
		}
		sb.append(String.format(format, objects));
		return sb.toString();
	}
	
	/**
	 * This context contain some specific query prefixex that require to be added to the final SQL query.
	 * 
	 * @return
	 */
	public boolean hasQueryContextes() {
		return !queryContextes.isEmpty();
	}
}
