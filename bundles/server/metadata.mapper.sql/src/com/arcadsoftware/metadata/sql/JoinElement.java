package com.arcadsoftware.metadata.sql;

import java.util.ArrayList;

/**
 * This internal object is use to keep track of all join made to perform the final query.
 * 
 * <p>
 * This object has a Tree like structure, each element is dependent from a parent one, and 
 * possess an unique alias even if the same table is involved ate multiple points.
 * 
 * @author ARCAD Software
 */
public class JoinElement {

	private final ArrayList<JoinElement> children;
	private final String alias;
	private final String table;
	private final String id;
	private final String parentCol;
	private final String deletedCol;
	private boolean inner;
	
	public JoinElement(String alias, String sql) {
		super();
		this.alias = alias;
		table = sql;
		id = null;
		parentCol = null;
		deletedCol = null;
		children = new ArrayList<>();
	}
	
	private JoinElement(String alias, EntityInfo entityInfo, String parentCol, boolean deleted) {
		super();
		this.alias = alias;
		table = entityInfo.table;
		id = entityInfo.idCol;
		if (deleted) {
			deletedCol = null;
		} else {
			deletedCol = entityInfo.deleteCol;
		}
		this.parentCol = parentCol;
		children = new ArrayList<>();
	}
	
	private JoinElement(String alias, LinkInfo linkInfo, String parentCol, boolean deleted) {
		super();
		this.alias = alias;
		table = linkInfo.table;
		id = linkInfo.sourceCol;
		if (deleted) {
			deletedCol = null;
		} else {
			deletedCol = linkInfo.deleteCol;
		}
		this.parentCol = parentCol;
		children = new ArrayList<>();
	}
	
	public void setInner() {
		inner = true;
	}
	
	public String getAlias() {
		return alias;
	}
	
	/**
	 * 
	 * @param entityInfo the current entity to be added to the join tree.
	 * @param parentCol must be prefixed by the parent alias in the join Tree !
	 * @param deleted True is deleted values must be used in the 
	 * @return
	 */
	public JoinElement add(EntityInfo entityInfo, String parentCol, boolean deleted) {
		for (JoinElement c: children) {
			if ((entityInfo.table == c.table) && c.parentCol.equals(parentCol)) {
				return c;
			}
		}
		JoinElement result = new JoinElement(alias + '_' + children.size(), entityInfo, parentCol, deleted);
		children.add(result);
		return result;
	}
	
	public JoinElement add(LinkInfo linkInfo, String parentCol, boolean deleted) {
		for (JoinElement c: children) {
			if ((linkInfo.table == c.table) && c.parentCol.equals(parentCol) && c.id.equals(linkInfo.sourceCol)) {
				return c;
			}
		}
		JoinElement result = new JoinElement(alias + '_' + children.size(), linkInfo, parentCol, deleted);
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
		String refcol;
		if (deletedCol == null) {
			refcol = parentCol;
		} else {
			refcol = parentCol + mapper.fg.and + alias + '.' + deletedCol + mapper.fg.equaldelfalse;
		}
		if (inner) {
			sb.append(String.format(mapper.fg.join_inner, table, alias, id, refcol));
		} else {
			sb.append(String.format(mapper.fg.join, table, alias, id, refcol));
		}
	}
	
	
}
