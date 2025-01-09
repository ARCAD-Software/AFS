package com.arcadsoftware.metadata.sql;

import java.util.HashMap;

import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.criteria.CriteriaContextBasic;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * This Mapper operation context is used to extend the basic implementation with SQL specific operations.
 * 
 * <p>
 * TODO Allow to define "cachable" contextes to be able to optinize the SQL request generation. 
 * 
 * @author ARCAD Software
 */
public class SQLCriteriaContext extends CriteriaContextBasic {

	private final MapperSQLService mapper;
	private final HashMap<String, String> queryContextes;
	
	public SQLCriteriaContext(MapperSQLService mapper, MetaDataEntity entity, IConnectionUserBean currentUser) {
		super(entity, currentUser);
		this.mapper = mapper;
		queryContextes = new HashMap<>();
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
