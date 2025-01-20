package com.arcadsoftware.metadata.sql;

import java.util.ArrayList;

public class JoinElement {

	private final ArrayList<JoinElement> children;
	private final String alias;
	private final String table;
	private final String id;
	private final String parentCol;
	private boolean inner;
	
	public JoinElement(String alias, String sql) {
		super();
		this.alias = alias;
		table = sql;
		id = null;
		parentCol = null;
		children = new ArrayList<>();
	}
	
	private JoinElement(String alias, EntityInfo entityInfo, String parentCol) {
		super();
		this.alias = alias;
		table = entityInfo.table;
		id = entityInfo.idCol;
		this.parentCol = parentCol;
		children = new ArrayList<>();
	}
	
	public void setInner() {
		inner = true;
	}
	
	public String getAlias() {
		return alias;
	}
	
	public JoinElement add(EntityInfo entityInfo, String parentCol) {
		for (JoinElement c: children) {
			if ((entityInfo.table == c.table) && c.parentCol.equals(parentCol)) {
				return c;
			}
		}
		JoinElement result = new JoinElement(alias + '_' + children.size(), entityInfo, parentCol);
		children.add(result);
		return result;
	}

	public String toString(MapperSQLService mapper) {
		StringBuilder sb = new StringBuilder(table);
		for (JoinElement c: children) {
			c.toString(mapper, sb);
		}
		return sb.toString();
	}

	private void toString(MapperSQLService mapper, StringBuilder sb) {
		if (inner) {
			sb.append(String.format(mapper.fg.join_inner, table, alias, id, parentCol));
		} else {
			sb.append(String.format(mapper.fg.join, table, alias, id, parentCol));
		}
	}
	
	
}
