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

import java.util.List;

import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;

/**
 * Object used to cache multi-links pre-generated selection queries.
 * 
 * <p>
 * If code is null the this link query is not correctly built and is unusable. Please note that the generated SQL code
 * is invariant as long as any of the implicated links and inner entities are modified.
 * <p>
 * Note that this implementation take into account:
 * <ul>
 * <li>Multiple subdivised entities at any places of the link chain. Parent entity of the first link and referenced
 * entity of the last link included.
 * <li>Take into account the soft deletion on all links and on inner entities. This exclude the soft deletion on the
 * first link parent one and the last referenced entity.
 * <li>Support reverse link included in the chain.
 * <li>Only support foreign element on the last referenced entity. All the links need to be owned by the same mapper.
 * And if some inner entities are not onwer by the same mapper they are ignored for subdivision and soft deletion
 * detection.
 * </ul>
 *
 * @author ARCAD Software
 */
public class MultiLinkQuery {

	private static final String RECURCIVE_PREFIX = "r_"; //$NON-NLS-1$
	private static final String DEFAULT_LINKALIASPREFIX = "l_"; //$NON-NLS-1$
	private static final String LNKALS_INRECQUERY = "rl"; //$NON-NLS-1$

	/**
	 * Generate a unique code that represents the given link chain.
	 * 
	 * @param links
	 * @param deleted
	 * @param ignoreSubdivision
	 * @return a non null code.
	 */
	public static String getCode(List<MetaDataLink> links, boolean deleted, boolean ignoreSubdivision) {
		final StringBuilder code = new StringBuilder();
		for (final MetaDataLink l : links) {
			if (!code.isEmpty() ) {
				code.append('.');
			}
			code.append(l.getCode());
		}
		if (deleted) {
			code.append('%');
		} else {
			code.append('_');
		}
		if (ignoreSubdivision) {
			code.append('_');
		} else {
			code.append('@');
		}
		return code.toString();
	}

	/**
	 * Generate a unique code that represents the given link chain.
	 * 
	 * @param linkCode
	 * @param deleted
	 * @param ignoreSubdivision
	 * @return a non null code.
	 */
	public static String getCode(String linkCode, boolean deleted, boolean ignoreSubdivision) {
		final StringBuilder code = new StringBuilder(linkCode);
		if (deleted) {
			code.append('%');
		} else {
			code.append('_');
		}
		if (ignoreSubdivision) {
			code.append('_');
		} else {
			code.append('@');
		}
		return code.toString();
	}

	/**
	 * Generate the query part allowing to select the target data through the given link chain.
	 * 
	 * <p>
	 * A non null result ensure that:
	 * 
	 * <ul>
	 * <li> The link list is not null, non empty, and all members are not null.
	 * <li> All link given in the list are owned by the given mapper.
	 * </ul>
	 * 
	 * @param mapper
	 * @param links
	 * @param deleted
	 * @param ignoreSubdivision
	 * @return null if this link chains can not be proceeded.
	 */
	public static MultiLinkQuery generate(MapperSQLService mapper, List<MetaDataLink> links, boolean deleted,
			boolean ignoreSubdivision) {
		return generate(1, mapper, links, deleted, ignoreSubdivision);
	}
	
	/**
	 * Generate the query part allowing to select the target data through the given link chain.
	 * 
	 * <p>
	 * A non null result ensure that:
	 * 
	 * <ul>
	 * <li> The link list is not null, non empty, and all members are not null.
	 * <li> All link given in the list are owned by the given mapper.
	 * </ul>
	 * 
	 * @parma initial a positive number used to generate unique aliases (need to be >= to the previous chain "nextIncrement" value). 
	 * @param mapper a non null mapper from where all the given links belong to.  
	 * @param links a chain on MetaDataEntity links.
	 * @param deleted true if soft-deleted links and soft-deleted inner items must be also be taken into account.
	 * @param ignoreSubdivision true if subdivision (recursive query) must be ignored.
	 * @return null if this link chains can not be proceeded or is invalid.
	 */
	public static MultiLinkQuery generate(int initial, MapperSQLService mapper, List<MetaDataLink> links, boolean deleted,
			boolean ignoreSubdivision) {
		if ((links == null) || links.isEmpty()) {
			return null;
		}
		StringBuilder joins = null;
		StringBuilder where = null;
		String rec_alias = null;
		StringBuilder rec_query = new StringBuilder();
		String prev_col = null;
		String prev_alias = null;
		int alias = initial;
		for (final MetaDataLink l : links) {
			if (l == null) {
				alias = 1;
				break;
			}
			final MetaDataEntity cse = l.getParent();
			final EntityInfo csei = mapper.getEntityInfo(cse);
			if (csei == null) {
				alias = 1;
				break;
			}
			final LinkInfo cli = csei.links.get(l.getCode());
			if ((cli == null) || !cli.isComplete()) {
				// Unknown of virtual link...
				alias = 1;
				break;
			}
			final String la = DEFAULT_LINKALIASPREFIX + alias++;
			// Ignore recursive link if the current link is a non recursive link but with same target. 
			if (l.isRecursive() || !cse.getType().equals(l.getType())) {
				// Process recursive link based on the current parent entity...
				final MetaDataLink recLink = cse.getFirstRecursiveLink();
				if ((recLink != null) && !ignoreSubdivision) {
					final LinkInfo rli = csei.links.get(recLink.getCode());
					// If the recursive link is unknown or incomplete, process like a normal link.
					if ((rli != null) && rli.isComplete()) {
						rec_alias = RECURCIVE_PREFIX + alias;
						final String firstSelect;
						if (joins == null) {
							firstSelect = mapper.fg.select_const;
						} else {
							// check if current entity items are not deleted too.
							if ((!deleted) && (csei.deleteCol != null)) {
								final String talias = DEFAULT_LINKALIASPREFIX + alias++;
								joins.append(String.format(mapper.fg.join_inner, csei.table, talias, csei.idCol, prev_alias + '.' + prev_col));
								joins.append(' ');
								joins.append(mapper.fg.and);
								joins.append(talias);
								joins.append('.');
								joins.append(csei.deleteCol);
								joins.append(mapper.fg.equaldelfalse);
							}
							if ((where == null) || where.isEmpty()) {
								firstSelect = String.format(mapper.fg.selectall, prev_alias + '.' + prev_col + mapper.fg.asid, joins.toString());
							} else {
								firstSelect = String.format(mapper.fg.select, prev_alias + '.' + prev_col + mapper.fg.asid, joins.toString(),
										where.toString());
							}
						}
						final StringBuilder del = new StringBuilder();
						if (!deleted) {
							if (rli.deleteCol != null) {
								del.append(mapper.fg.and);
								del.append(LNKALS_INRECQUERY);
								del.append('.');
								del.append(rli.deleteCol);
								del.append(mapper.fg.equaldelfalse);
							}
							// Test if the sub-elements are not deleted too.
							if ((joins != null) && (csei.deleteCol != null)) {
								final String talias = LNKALS_INRECQUERY + DEFAULT_LINKALIASPREFIX + alias++;
								del.append(String.format(mapper.fg.join_inner, csei.table, talias, csei.idCol,
										LNKALS_INRECQUERY + '.' + rli.sourceCol));
								del.append(' ');
								del.append(mapper.fg.and);
								del.append(talias);
								del.append('.');
								del.append(csei.deleteCol);
								del.append(mapper.fg.equaldelfalse);
							}
						}
						if (!rec_query.isEmpty()) {
							rec_query.append(mapper.fg.rec_sub);
						}
						rec_query.append(String.format(mapper.fg.rec_alt, rec_alias, firstSelect, // 
								rli.table, rli.destCol, rli.sourceCol, del.toString()));
						// Reinitialize the joins chains from the recursive table,
						joins = new StringBuilder(rec_alias);
						where = new StringBuilder();
						if (recLink.getCode().equals(l.getCode())) {
							// The current link is the recursive one so we already have the target selection.
							prev_alias = rec_alias;
							prev_col = mapper.fg.id;
						} else {
							// And add the current link to the final selection.
							prev_alias = la;
							prev_col = cli.destCol;
							joins.append(String.format(mapper.fg.join_inner, cli.table, la, cli.sourceCol, //
									rec_alias + '.' + mapper.fg.id));
							if ((!deleted) && (cli.deleteCol != null)) {
								joins.append(mapper.fg.and);
								joins.append(la);
								joins.append('.');
								joins.append(cli.deleteCol);
								joins.append(mapper.fg.equaldelfalse);
							}
						}
						continue;
					}
				}
			}
			if (joins == null) {
				// Initialize the selection
				joins = new StringBuilder(String.format(mapper.fg.tablealias, cli.table, la));
				where = new StringBuilder(la);
				where.append('.');
				where.append(cli.sourceCol);
				where.append(mapper.fg.paramequal);
				if ((!deleted) && (cli.deleteCol != null)) {
					where.append(mapper.fg.and);
					where.append(la);
					where.append('.');
					where.append(cli.deleteCol);
					where.append(mapper.fg.equaldelfalse);
				}
				prev_alias = la;
				prev_col = cli.destCol;
			} else {
				// Test if the current link source items are not deleted too.
				if ((!deleted) && (csei.deleteCol != null)) {
					final String talias = DEFAULT_LINKALIASPREFIX + alias++;
					joins.append(String.format(mapper.fg.join_inner, csei.table, talias, csei.idCol, prev_alias + '.' + prev_col));
					joins.append(' ');
					joins.append(mapper.fg.and);
					joins.append(talias);
					joins.append('.');
					joins.append(csei.deleteCol);
					joins.append(mapper.fg.equaldelfalse);
				}
				// Add the current link to the selection
				joins.append(String.format(mapper.fg.join_inner, cli.table, la, cli.sourceCol, prev_alias + '.' + prev_col));
				if ((!deleted) && (cli.deleteCol != null)) {
					joins.append(mapper.fg.and);
					joins.append(la);
					joins.append('.');
					joins.append(cli.deleteCol);
					joins.append(mapper.fg.equaldelfalse);
				}
				prev_alias = la;
				prev_col = cli.destCol;
			}
		}
		// Link chain correctly generated (at least one link, and no unknown entities nor virtual links.
		if (alias <= initial) {
			return null;
		}
		// Process recursive link on the final target entity, if available:
		final MetaDataEntity finale = links.get(links.size() - 1).getRefEntity();
		if ((finale != null) && (!ignoreSubdivision)) {
			final MetaDataLink reclink = finale.getFirstRecursiveLink();
			if (reclink != null) {
				final EntityInfo fei = mapper.getEntityInfo(finale);
				if (fei != null) {
					final LinkInfo rfli = fei.links.get(reclink.getCode());
					// If the recursive link is unknown or incomplete, process like a normal link.
					if ((rfli != null) && rfli.isComplete()) {
						alias++;
						rec_alias = RECURCIVE_PREFIX + alias;
						final StringBuilder del = new StringBuilder();
						// check if current entity items are not deleted too.
						if ((!deleted) && (fei.deleteCol != null)) {
							// for the first selection in the recursive query
							String talias = DEFAULT_LINKALIASPREFIX + alias;
							joins.append(String.format(mapper.fg.join_inner, fei.table, talias, fei.idCol,
									prev_alias + '.' + prev_col));
							joins.append(' ');
							joins.append(mapper.fg.and);
							joins.append(talias);
							joins.append('.');
							joins.append(fei.deleteCol);
							joins.append(mapper.fg.equaldelfalse);
							// for the recursive selection itself
							talias = LNKALS_INRECQUERY + DEFAULT_LINKALIASPREFIX + alias;
							del.append(String.format(mapper.fg.join_inner, fei.table, talias, fei.idCol,
									LNKALS_INRECQUERY + '.' + rfli.sourceCol));
							del.append(' ');
							del.append(mapper.fg.and);
							del.append(talias);
							del.append('.');
							del.append(fei.deleteCol);
							del.append(mapper.fg.equaldelfalse);
						}
						final String firstSelect = String.format(mapper.fg.select,
								prev_alias + '.' + prev_col + mapper.fg.asid, joins.toString(),
								where.toString());
						if (!rec_query.isEmpty()) {
							rec_query.append(mapper.fg.rec_sub);
						}
						rec_query.append(String.format(mapper.fg.rec_alt, rec_alias, firstSelect, rfli.table,
								rfli.destCol, rfli.sourceCol, del.toString()));
						// Reinitialize the joins chains from the recursive table,
						joins = new StringBuilder(rec_alias);
						where = new StringBuilder();
						prev_alias = rec_alias;
						prev_col = mapper.fg.id;
					}
				}
			}
		}
		return new MultiLinkQuery(alias + 1, joins.toString(), where.toString(), rec_alias, rec_query.toString(), prev_alias, prev_col);
	}
	
	public final int nextIncrement;
	public final String rec_alias;
	public final String rec_query;
	public final String join;
	public final String where;
	public final String linkAlias;
	public final String linkCol;

	public MultiLinkQuery(int nextIncrement, String join, String where, String rec_alias, String rec_query, String linkAlias,
			String linkCol) {
		super();
		this.nextIncrement = nextIncrement;
		this.join = join;
		this.where = where;
		this.rec_alias = rec_alias;
		this.rec_query = rec_query;
		this.linkAlias = linkAlias;
		this.linkCol = linkCol;
	}
}
