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
package com.arcadsoftware.metadata.rights;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapPartialList;
import com.arcadsoftware.metadata.AbstractMapperService;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.OrderComparator;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.criteria.AndCriteria;
import com.arcadsoftware.metadata.criteria.CriteriaContextBasic;
import com.arcadsoftware.metadata.criteria.EqualCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.criteria.LinkCriteria;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Read only memory mapper...
 */
public class RightsMapperService extends AbstractMapperService<CriteriaContextBasic> {

	private Activator activator;
	
	public RightsMapperService(Activator activator) {
		super();
		this.activator = activator;
		addDomain("mem:rights"); //$NON-NLS-1$
	}

	@Override
	public BeanMap create(MetaDataEntity entity, List<MetaDataAttribute> attributes, List<Object> values, IConnectionUserBean currentUser) {
		return null;
	}

	@Override
	public long create(MetaDataEntity entity, List<MetaDataAttribute> attributes, Iterator<BeanMap> items, IConnectionUserBean currentUser) {
		return 0;
	}

	@Override
	public boolean delete(MetaDataEntity entity, int itemId, boolean hardDelete) {
		return false;
	}

	@Override
	public int delete(MetaDataEntity entity, ISearchCriteria criteria, IConnectionUserBean currentUser, boolean hardDelete) {
		return 0;
	}

	@Override
	public boolean delete(MetaDataEntity entity, int itemId, boolean hardDelete, IConnectionUserBean currentUser) {
		return false;
	}

	@Override
	public int undelete(MetaDataEntity entity, ISearchCriteria criteria, IConnectionUserBean currentUser) {
		return 0;
	}

	@Override
	public boolean update(MetaDataEntity entity, int itemId, List<MetaDataAttribute> attributes, List<Object> values, IConnectionUserBean currentUser) {
		return false;
	}

	@Override
	public boolean doUpdate(List<MetaDataAttribute> attributes, List<Object> values, ISearchCriteria criteria, CriteriaContextBasic context) {
		return false;
	}

	@Override
	public boolean doLinkAdd(MetaDataLink link, int sourceId, int destId) {
		return false;
	}

	@Override
	public boolean doLinkRemove(MetaDataLink link, int sourceId, int destId) {
		return false;
	}

	@Override
	public BeanMap selection(MetaDataEntity entity, int itemId, List<ReferenceLine> attributes, boolean deleted) {
		BeanMap item;
		if (Activator.RIGHT.equals(entity.getType())) {
			item = activator.getRightBean(itemId);
			if (item != null) {
				item = filterBean(item, attributes);
			}
		} else {
			item = activator.getRightCategoryBean(itemId);
		}
		return item;
	}

	@Override
	public BeanMapList doSelection(List<ReferenceLine> attributes, boolean deleted, ISearchCriteria criteria,
			boolean distinct, List<ReferenceLine> orders, int page, int limit, CriteriaContextBasic context) {
		BeanMapList result = new BeanMapList();
		if (Activator.RIGHT.equals(context.getEntity().getType())) {
			for (BeanMap right: activator.getRights()) {
				if (criteria.test(right, context.getCurrentUser())) {
					result.add(filterBean(right,attributes));
				}
			}
		} else {
			for (BeanMap cat: activator.getRightCategories()) {
				if (criteria.test(cat, context.getCurrentUser())) {
					result.add(filterBean(cat,attributes));
				}
			}
		}
		if ((orders != null) && (orders.size() > 0)) {
			Collections.sort(result, new OrderComparator(orders));
		} else {
			Collections.sort(result);
		}
		if ((limit > 0) && (limit < result.size())) {
			BeanMapPartialList list = new BeanMapPartialList();
			list.setRank(page);
			list.setTotal(result.size());
			limit = limit + page;
			if (limit > result.size()) {
				limit = result.size();
			}
			for(int i = page; i < limit;i++) {
				list.add(result.get(i));
			}
			return list;
		}
		return result;
	}

	@Override
	public int doCount(boolean deleted, ISearchCriteria criteria, boolean distinct, CriteriaContextBasic context) {
		int result = 0;
		if (Activator.RIGHT.equals(context.getEntity().getType())) {
			for (BeanMap right: activator.getRights()) {
				if (criteria.test(right, context.getCurrentUser())) {
					result++;
				}
			}
		} else {
			for (BeanMap cat: activator.getRightCategories()) {
				if (criteria.test(cat, context.getCurrentUser())) {
					result++;
				}
			}
		}
		return result;
	}

	@Override
	public BeanMap doSelectionFirst(List<ReferenceLine> attributes, boolean deleted, ISearchCriteria criteria, CriteriaContextBasic context) {
		if (Activator.RIGHT.equals(context.getEntity().getType())) {
			for (BeanMap right: activator.getRights()) {
				if (criteria.test(right, context.getCurrentUser())) {
					return filterBean(right,attributes);
				}
			}
		} else {
			for (BeanMap cat: activator.getRightCategories()) {
				if (criteria.test(cat, context.getCurrentUser())) {
					return filterBean(cat,attributes);
				}
			}
		}
		return null;
	}

	@Override
	public boolean linkTest(List<MetaDataLink> links, int sourceId, int destId, boolean deleted, boolean ignoreSubdivision) {
		if ((links == null) || (links.size() != 1)) {
			// TODO manage the completion of an external link (in a multi-link selection)...
			return false;
		}
		MetaDataLink link = links.get(0);
		MetaDataEntity e = link.getParent();
		if (e.getMapper() != this) {
			activator.error("Link Selection with RightsMapper on link: " + links.get(0) + " belong to " + e.toString());
			return e.getMapper().linkTest(links, sourceId, destId, ignoreSubdivision);
		}
		// Manage only rightCategory.rights
		if (Activator.RIGHTCATEGORY.equals(e.getType())) {
			BeanMap right = activator.getRightBean(destId);
			return (right != null) && (right.getInt(Activator.RIGHT_CATEGORY) == sourceId);
		}
		// Reverse other links..
		if (Activator.RIGHT.equals(e.getType())) {
			MetaDataEntity p = MetaDataEntity.loadEntity("profile"); //$NON-NLS-1$
			if (p == null) {
				return false;
			}
			MetaDataLink profileRights = p.getLink("rights"); //$NON-NLS-1$
			if ("profiles".equals(link.getCode())) {
				return p.getMapper().linkTest(list(profileRights), destId, sourceId, deleted, ignoreSubdivision);
			}
			if ("users".equals(link.getCode())) {
				MetaDataEntity u = MetaDataEntity.loadEntity("user"); //$NON-NLS-1$
				if (u == null) {
					return false;
				}
				MetaDataLink userProfiles = p.getLink("profiles"); //$NON-NLS-1$
				return p.getMapper().linkTest(list(userProfiles, profileRights), destId, sourceId, deleted, ignoreSubdivision);
			}
		}
		return false;
	}

	@Override
	public BeanMapList doLinkSelection(List<MetaDataLink> links, int sourceId, List<ReferenceLine> attributes, boolean deleted,
			ISearchCriteria criteria, boolean distinct, boolean ignoreSubdivision, List<ReferenceLine> orders, int page, 
			int limit, CriteriaContextBasic context) {
		if ((links == null) || links.isEmpty()) {
			return new BeanMapList();
		}
		MetaDataLink link = links.get(0);
		MetaDataEntity parent = link.getParent();
		if (parent.getMapper() != this) {
			activator.error("Link Selection with RightsMapper on link: " + links.get(0) + " belong to " + parent.toString());
			return parent.getMapper().linkSelection(links, sourceId, attributes, deleted, criteria, distinct, ignoreSubdivision, orders, context.getCurrentUser(), page, limit);
		}
		// Manage only rightCategory.rights
		if (Activator.RIGHTCATEGORY.equals(parent.getType())) {
			BeanMapList result = new BeanMapList();
			for (BeanMap right: activator.getRights()) {
				if (right.get(Activator.RIGHTCATEGORY).equals(sourceId) &&  criteria.test(right, context.getCurrentUser())) {
					result.add(filterBean(right,attributes));
				}
			}
			if ((orders != null) && (orders.size() > 0)) {
				Collections.sort(result, new OrderComparator(orders));
			} else {
				Collections.sort(result);
			}
			if (limit < result.size()) {
				BeanMapPartialList list = new BeanMapPartialList();
				list.setRank(page);
				list.setTotal(result.size());
				limit = limit + page;
				if (limit > result.size()) {
					limit = result.size();
				}
				for(int i = page; i < limit;i++) {
					list.add(result.get(i));
				}
				return list;
			}
			return result;
		}		
		if (!Activator.RIGHT.equals(context.getEntity().getType())) {
			return new BeanMapList();
		}
		MetaDataEntity p = MetaDataEntity.loadEntity("profile"); //$NON-NLS-1$
		if (p == null) {
			return new BeanMapList();
		}
		if ("profiles".equals(link.getCode())) { //$NON-NLS-1$
			return p.getMapper().selection(attributes, deleted, AndCriteria.and(new LinkCriteria(sourceId, "rights"), criteria), distinct, orders, context.getCurrentUser(), page, limit); //$NON-NLS-1$ 
		}
		if ("users".equals(link.getCode())) { //$NON-NLS-1$
			return p.getMapper().selection(attributes, deleted, AndCriteria.and(new LinkCriteria(sourceId, "profiles.rights"), criteria), distinct, orders, context.getCurrentUser(), page, limit); //$NON-NLS-1$
		}
		return new BeanMapList();
	}

	@Override
	public int doLinkCount(List<MetaDataLink> links, int id, boolean deleted, boolean ignoreSubdivision, ISearchCriteria criteria, boolean distinct,
			CriteriaContextBasic context) {
		if ((links == null) || links.isEmpty()) {
			return 0;
		}
		MetaDataLink link = links.get(0);
		MetaDataEntity parent = link.getParent();
		if (parent.getMapper() != this) {
			activator.error("Link Count with RightsMapper on link: " + links.get(0) + " belong to " + parent.toString());
			return parent.getMapper().linkCount(links, id, deleted, criteria, distinct, ignoreSubdivision, context.getCurrentUser());
		}
		// Manage only rightCategory.rights
		if (Activator.RIGHTCATEGORY.equals(parent.getType())) {
			return doCount(deleted, new AndCriteria(criteria, new EqualCriteria(Activator.RIGHT_CATEGORY, id)), distinct, context);
		}
		if (!Activator.RIGHT.equals(context.getEntity().getType())) {
			return 0;
		}
		MetaDataEntity p = MetaDataEntity.loadEntity("profile"); //$NON-NLS-1$
		if (p == null) {
			return 0;
		}
		if ("profiles".equals(link.getCode())) { //$NON-NLS-1$
			return p.getMapper().count(p, deleted, AndCriteria.and(new LinkCriteria(id, "rights"), criteria), distinct, context.getCurrentUser()); //$NON-NLS-1$
		}
		if ("users".equals(link.getCode())) { //$NON-NLS-1$
			MetaDataEntity u = MetaDataEntity.loadEntity("user"); //$NON-NLS-1$
			if (u == null) {
				return 0;
			}
			return u.getMapper().count(u, deleted, AndCriteria.and(new LinkCriteria(id, "profiles.rights"), criteria), distinct, context.getCurrentUser()); //$NON-NLS-1$ 
		}
		return 0;
	}

	@Override
	protected CriteriaContextBasic getContext(MetaDataEntity entity, IConnectionUserBean currentUser) {
		return new CriteriaContextBasic(entity, currentUser);
	}

	public void rights() {
		System.out.println("Currently defined Access Rights:"); //$NON-NLS-1$
		ArrayList<BeanMap> rights = new ArrayList<>(activator.getRights());
		Collections.sort(rights);
		for (BeanMap b: rights) {
			System.out.println(b.getId() + " - " + b.getString(Activator.RIGHT_CODE) + " [" + b.getString(Activator.RIGHT_CATEGORY) + ']'); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}
	
	public void rights(String prefix) {
		if ((prefix == null) || prefix.isBlank()) {
			rights();
			return;
		}
		System.out.println("Currently defined Access Rights starting with " + prefix + ":"); //$NON-NLS-1$
		prefix = prefix.toLowerCase();
		ArrayList<BeanMap> rights = new ArrayList<>(activator.getRights());
		Collections.sort(rights);
		for (BeanMap b: rights) {
			String code =  b.getString(Activator.RIGHT_CODE);
			if (code.toLowerCase().startsWith(prefix)) {
				System.out.println(b.getId() + " - " +code + " [" + b.getString(Activator.RIGHT_CATEGORY) + ']'); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
	}

	@Override
	public Date lastModification(MetaDataEntity entity, boolean deleted) {
		return entity.getDate();
	}
}
