/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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

import java.util.Collections;
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
import com.arcadsoftware.metadata.criteria.EqualCriteria;
import com.arcadsoftware.metadata.criteria.ICriteriaContext;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Read only memory mapper...
 */
public class RightsMapperService extends AbstractMapperService {

	private Activator activator;
	
	public RightsMapperService(Activator activator) {
		super();
		this.activator = activator;
		addDomain("mem:rights"); //$NON-NLS-1$
	}

	@Override
	public BeanMap create(MetaDataEntity entity, List<MetaDataAttribute> attributes, List<Object> values) {
		return null;
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
	public int undelete(MetaDataEntity entity, ISearchCriteria criteria, IConnectionUserBean currentUser) {
		return 0;
	}

	@Override
	public boolean update(MetaDataEntity entity, int itemId, List<MetaDataAttribute> attributes, List<Object> values) {
		return false;
	}

	@Override
	public boolean doUpdate(List<MetaDataAttribute> attributes, List<Object> values, ISearchCriteria criteria, ICriteriaContext context) {
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
			boolean distinct, List<ReferenceLine> orders, int page, int limit, ICriteriaContext context) {
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
	public int doCount(boolean deleted, ISearchCriteria criteria, boolean distinct, ICriteriaContext context) {
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
	public BeanMap doSelectionFirst(List<ReferenceLine> attributes, boolean deleted, ISearchCriteria criteria, ICriteriaContext context) {
		if (Activator.RIGHT.equals(context.getEntity().getType())) {
			for(BeanMap right: activator.getRights()) {
				if (criteria.test(right, context.getCurrentUser())) {
					return filterBean(right,attributes);
				}
			}
		} else {
			for(BeanMap cat:activator.getRightCategories()) {
				if (criteria.test(cat, context.getCurrentUser())) {
					return filterBean(cat,attributes);
				}
			}
		}
		return null;
	}

	@Override
	public boolean doLinkTest(MetaDataLink link, int sourceId, int destId) {
		BeanMap right = activator.getRightBean(destId);
		return (right != null) && (right.getInt(Activator.RIGHT_CATEGORY) == sourceId);
	}

	@Override
	public BeanMapList doLinkSelection(MetaDataLink link, int sourceId, List<ReferenceLine> attributes, boolean deleted,
			ISearchCriteria criteria, boolean distinct, List<ReferenceLine> orders, int page, int limit, ICriteriaContext context) {
		if (link.getParent().getMapper() != this) {
			activator.error("Link Selection with RightsMapper on link: " + link.toString() + " belong to " + link.getParent().toString());
			return new BeanMapList();
		}
		if (link.getRefEntity().getMapper() != this) {
			return link.getRefEntity().getMapper().linkSelection(link, sourceId, attributes, deleted, criteria, distinct, orders, context.getCurrentUser(), page, limit);
		}
		if (!Activator.RIGHT.equals(context.getEntity().getType())) {
			return new BeanMapList();
		}
		BeanMapList result = new BeanMapList();
		for(BeanMap right:activator.getRights()) {
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

	@Override
	public int doLinkCount(MetaDataLink link, int id, boolean deleted, ISearchCriteria criteria, boolean distinct,
			ICriteriaContext context) {
		if (context.getEntity().getType().equals(Activator.RIGHT)) {
			context.useReference(context.getReference(Activator.RIGHT_CATEGORY));
			return doCount(deleted,new AndCriteria(criteria, new EqualCriteria(Activator.RIGHT_CATEGORY, id)),distinct,context);
		}
		if (link.getRefEntity().getMapper() != this) {
			return link.getRefEntity().getMapper().linkCount(link, id, deleted, criteria, distinct, context.getCurrentUser());
		}
		return 0;
	}

}
