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
package com.arcadsoftware.metadata.internal;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapPartialList;
import com.arcadsoftware.metadata.IEntityRegistry;
import com.arcadsoftware.metadata.IMapperService;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.client.DataAccess;
import com.arcadsoftware.metadata.criteria.AndCriteria;
import com.arcadsoftware.metadata.criteria.EqualCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.criteria.IdEqualCriteria;
import com.arcadsoftware.metadata.xml.XmlCriteriaStream;
import com.arcadsoftware.osgi.ILoggedPlugin;
import com.arcadsoftware.rest.ServerErrorException;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * This facade allow a DataAccess to activate all the functionalities of the MetaData Entities 
 * (i.e. data loading and possibility to load  other entities from same server).
 * @author ARCAD Software
 */
public class DataAccessServices implements IMapperService, IEntityRegistry {

	private final ILoggedPlugin activator;
	private final DataAccess dao;
	private final ArrayList<String> domains;
	
	public DataAccessServices(ILoggedPlugin activator, DataAccess dao) {
		super();
		this.activator = activator;
		this.dao = dao;
		domains = new ArrayList<String>();
	}

	private void logError(Throwable e) {
		if (activator != null) {
			activator.error(e.getLocalizedMessage(), e);
		} else {
			//Do something
		}
	}

	private BeanMap getBeanMap(String type, int id, String attributes, Object[] values) {
		BeanMap result = new BeanMap(type, id);
		int i = 0;
		for(String att:attributes.split(" ")) { //$NON-NLS-1$
			if (i >= values.length) {
				break;
			}
			result.put(att, values[i++]);
		}
		return result;
	}

	private BeanMap getBeanMap(int id, List<MetaDataAttribute> attributes, List<Object> values) {
		return getBeanMap(attributes.get(0).getParent().getType(), id,attributes, values);
	}

	private BeanMap getBeanMap(String type, int id,
			List<MetaDataAttribute> attributes, List<Object> values) {
		BeanMap result = new BeanMap(type, id);
		int i = 0;
		if (attributes != null) {
			for(MetaDataAttribute att:attributes) { //$NON-NLS-1$
				if (i >= values.size()) {
					break;
				}
				result.put(att.getCode(), values.get(i++));
			}
		}
		return result;
	}
	
	@Override
	public MetaDataEntity getEntity(String type) {
		try {
			return dao.getEntity(type);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public List<MetaDataEntity> getEntities(String domain) {
		ArrayList<MetaDataEntity> result = new ArrayList<MetaDataEntity>();
		try {
			for(MetaDataEntity entity:dao.getEntities()) {
				if (domain.equalsIgnoreCase(entity.getDomain())) {
					result.add(entity);
				}
			}
		} catch (ServerErrorException e) {
			logError(e);
		}
		return null;
	}

	@Override
	public List<MetaDataEntity> getEntities() {
		try {
			return dao.getEntities();
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}
	
	@Override
	public MetaDataEntity updateEntity(MetaDataEntity entityPatch) {
		return null;
	}

	@Override
	public MetaDataEntity addEntity(MetaDataEntity entity) {
		return null;
	}

	@Override
	public MetaDataEntity removeEntity(MetaDataEntity entity) {
		return null;
	}

	@Override
	public BeanMap create(BeanMap item) {
		try {
			return dao.post(item);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMap create(String type, String attributes, List<Object> values) {
		return create(getBeanMap(type, 0, attributes, values.toArray(new Object[values.size()])));
	}

	@Override
	public BeanMap create(MetaDataEntity entity, String attributes, List<Object> values) {
		return create(getBeanMap(entity.getType(), 0, attributes, values.toArray(new Object[values.size()])));
	}

	@Override
	public BeanMap create(MetaDataEntity entity, String attributes, Object... values) {
		return create(getBeanMap(entity.getType(), 0, attributes, values));
	}

	@Override
	public BeanMap create(MetaDataAttribute attribute, Object value) {
		return create(getBeanMap(attribute.getParent().getType(), 0, attribute.getCode(), new Object[]{value}));
	}

	@Override
	public BeanMap create(List<MetaDataAttribute> attributes, List<Object> values) {
		return create(getBeanMap(0, attributes, values));
	}

	@Override
	public BeanMap create(MetaDataEntity entity, List<MetaDataAttribute> attributes, List<Object> values) {
		return create(getBeanMap(entity.getType(), 0, attributes, values));
	}

	@Override
	public boolean delete(BeanMap item, boolean hardDelete) {
		if (item == null) {
			return false;
		}
		try {
			if (hardDelete) {
				dao.deleteHard(item.getType(), item.getId());
			} else {
				dao.delete(item);
			}
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean delete(String type, int itemId, boolean hardDelete) {
		try {
			if (hardDelete) {
				dao.deleteHard(type, itemId);
			} else {
				dao.delete(type, itemId);
			}
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean delete(MetaDataEntity entity, int itemId, boolean hardDelete) {
		try {
			if (hardDelete) {
				dao.deleteHard(entity.getType(), itemId);
			} else {
				dao.delete(entity.getType(), itemId);
			}
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public int delete(MetaDataEntity entity, ISearchCriteria criteria, IConnectionUserBean currentUser, boolean hardDelete) {
		try {
			if (hardDelete) {
				dao.deleteHard(entity.getType(), criteria);
			} else {
				dao.delete(entity.getType(), criteria);
			}
			return 1;
		} catch (ServerErrorException e) {
			logError(e);
			return 0;
		}
		// TODO DataAccess does not get the number of deleted element from the web-service !
	}

	@Override
	public int undelete(MetaDataEntity entity, ISearchCriteria criteria, IConnectionUserBean currentUser) {
		// TODO DataAccess Does not implement multi undelete...
		return 0;
	}

	@Override
	public boolean undelete(BeanMap item) {
		try {
			dao.undelete(item);
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean undelete(String type, int itemId) {
		try {
			dao.undelete(type, itemId);
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean undelete(MetaDataEntity entity, int itemId) {
		try {
			dao.undelete(entity.getType(),itemId);
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean update(BeanMap item) {
		try {
			dao.put(item);
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean update(String type, int itemId, String attributes, List<Object> values) {
		try {
			dao.put(getBeanMap(type,itemId,attributes,values.toArray(new Object[values.size()])));
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean update(MetaDataEntity entity, int itemId, String attributes, List<Object> values) {
		try {
			dao.put(getBeanMap(entity.getType(),itemId,attributes,values.toArray(new Object[values.size()])));
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean update(MetaDataEntity entity, int itemId, String attributes, Object... values) {
		try {
			dao.put(getBeanMap(entity.getType(),itemId,attributes,values));
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean update(int itemId, MetaDataAttribute attribute, Object value) {
		try {
			dao.put(getBeanMap(attribute.getParent().getType(),itemId,attribute.getCode(),new Object[]{value}));
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean update(int itemId, List<MetaDataAttribute> attributes, List<Object> values) {
		try {
			dao.put(getBeanMap(itemId, attributes, values));
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean update(MetaDataEntity entity, int itemId,
			List<MetaDataAttribute> attributes, List<Object> values) {
		try {
			dao.put(getBeanMap(entity.getType(), itemId, attributes, values));
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean touch(MetaDataEntity entity, int itemId, IConnectionUserBean currentUser) {
		try {
			dao.put(getBeanMap(entity.getType(), itemId, new ArrayList<>(), new ArrayList<>()));
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean update(MetaDataEntity entity,
			List<MetaDataAttribute> attributes, List<Object> values,
			ISearchCriteria criteria, IConnectionUserBean currentUser) {
		// TODO DataAccess does not implement multi-update !!!!
		return false;
	}

	@Override
	public boolean update(String type, String attributes, List<Object> values, String criteria, IConnectionUserBean currentUser) {
		// TODO DataAccess does not implement multi-update !!!!
		return false;
	}

	@Override
	public boolean update(MetaDataEntity entity, String[] attributes, List<Object> values, String criteria, IConnectionUserBean currentUser) {
		// TODO DataAccess does not implement multi-update !!!!
		return false;
	}

	@Override
	public boolean update(List<MetaDataAttribute> attributes, List<Object> values, ISearchCriteria criteria, IConnectionUserBean currentUser) {
		// TODO DataAccess does not implement multi-update !!!!
		return false;
	}

	@Override
	public BeanMap selection(BeanMap item, boolean deleted) {
		try {
			// TODO DataAccess can not get deleted items.
			return dao.get(item);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMap selection(MetaDataEntity entity, int itemId) {
		try {
			return dao.get(entity.getType(), itemId);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMap selection(String type, int itemId, String attributes, boolean deleted) {
		try {
			// TODO DataAccess does not support the selection of attributes to return.
			// TODO DataAccess can not get deleted items.
			return dao.get(type, itemId);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMap selection(MetaDataEntity entity, int itemId, String attributes, boolean deleted) {
		try {
			// TODO DataAccess does not support the selection of attributes to return.
			// TODO DataAccess can not get deleted items.
			return dao.get(entity.getType(), itemId);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMap selection(MetaDataEntity entity, int itemId, List<ReferenceLine> attributes, boolean deleted) {
		try {
			// TODO DataAccess does not support the selection of attributes to return.
			// TODO DataAccess can not get deleted items.
			return dao.get(entity.getType(), itemId);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMapList selection(String type) {
		try {
			return dao.getList(type);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMapList selection(MetaDataEntity entity) {
		try {
			return dao.getList(entity.getType());
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMapList selection(MetaDataEntity entity, String attributes) {
		try {
			return dao.getList(entity.getType(),attributes,(String)null,null,0,-1,false);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMapList selection(String type, String attributes, boolean deleted, String attributeTest, Object value) {
		try {
			return dao.getList(type,attributes,new EqualCriteria(attributeTest, value.toString()),null,0,-1,deleted);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMapList selection(MetaDataEntity entity, String attributes, boolean deleted, String attributeTest,
			Object value) {
		try {
			return dao.getList(entity.getType(),attributes,new EqualCriteria(attributeTest, value.toString()),null,0,-1,deleted);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMapList selection(List<ReferenceLine> attributes, boolean deleted, ReferenceLine attributeTest,
			Object value) {
		try {
			return dao.getList(attributes.get(0).getOriginEntity().getType(),ReferenceLine.getCodes(attributes),new EqualCriteria(attributeTest.getCode(), value.toString()),null,0,-1,deleted);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMapList selection(String type, String attributes, boolean deleted, String criteria, boolean distinct,
			String orders, IConnectionUserBean currentUser, int page, int limit) {
		try {
			// TODO Support user substitution (if real current user possess the associated right).
			return dao.getList(type,attributes,criteria,orders,page,limit,deleted, distinct);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMapList selection(List<ReferenceLine> attributes, boolean deleted, ISearchCriteria criteria,
			boolean distinct, List<ReferenceLine> orders, IConnectionUserBean currentUser, int page, int limit) {
		try {
			// TODO Support user substitution (if real current user possess the associated right).
			return dao.getList(attributes.get(0).getOriginEntity().getType(), ReferenceLine.getCodes(attributes), criteria, ReferenceLine.getCodes(orders),page,limit,deleted, distinct);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMapList selection(MetaDataEntity entity, boolean deleted, ISearchCriteria criteria, boolean distinct,
			IConnectionUserBean currentUser, int page, int limit) {
		try {
			// TODO Support user substitution (if real current user possess the associated right).
			return dao.getList(entity.getType(),null,criteria,null,page,limit,deleted, distinct);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMapList selection(MetaDataEntity entity, String attributes, boolean deleted, ISearchCriteria criteria,
			boolean distinct, String orders, IConnectionUserBean currentUser, int page, int limit) {
		try {
			// TODO Support user substitution (if real current user possess the associated right).
			return dao.getList(entity.getType(),attributes,criteria,orders,page,limit,deleted, distinct);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMapList selection(MetaDataEntity entity, List<ReferenceLine> attributes, boolean deleted,
			ISearchCriteria criteria, boolean distinct, List<ReferenceLine> orders, IConnectionUserBean currentUser,
			int page, int limit) {
		try {
			// TODO Support user substitution (if real current user possess the associated right).
			return dao.getList(entity.getType(),ReferenceLine.getCodes(attributes),criteria,ReferenceLine.getCodes(orders),page,limit,deleted, distinct);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public int count(String type) {
		try {
			// TODO DataAccess can not count items.
			BeanMapList list = dao.getList(type,"",(String)null,null,0,1,false); //$NON-NLS-1$
			if (list instanceof BeanMapPartialList) {
				return ((BeanMapPartialList)list).getTotal();
			}
			if (list != null) {
				return list.size();
			}
			return 0;
		} catch (ServerErrorException e) {
			logError(e);
			return 0;
		}
	}

	@Override
	public int count(MetaDataEntity entity) {
		try {
			// TODO DataAccess can not count items.
			BeanMapList list = dao.getList(entity.getType(),"",(String)null,null,0,1,false); //$NON-NLS-1$
			if (list instanceof BeanMapPartialList) {
				return ((BeanMapPartialList)list).getTotal();
			}
			if (list != null) {
				return list.size();
			}
			return 0;
		} catch (ServerErrorException e) {
			logError(e);
			return 0;
		}
	}

	@Override
	public int count(String type, boolean deleted, String attributeTest, Object value) {
		try {
			// TODO DataAccess can not count items.
			BeanMapList list = dao.getList(type,"",new EqualCriteria(attributeTest, value.toString()),null,0,1,deleted); //$NON-NLS-1$
			if (list instanceof BeanMapPartialList) {
				return ((BeanMapPartialList)list).getTotal();
			}
			if (list != null) {
				return list.size();
			}
			return 0;
		} catch (ServerErrorException e) {
			logError(e);
			return 0;
		}
	}

	@Override
	public int count(MetaDataEntity entity, boolean deleted, ReferenceLine attributeTest, Object value) {
		try {
			// TODO DataAccess can not count items.
			BeanMapList list = dao.getList(entity.getType(),"",new EqualCriteria(attributeTest.getCode(), value.toString()),null,0,1,deleted); //$NON-NLS-1$
			if (list instanceof BeanMapPartialList) {
				return ((BeanMapPartialList)list).getTotal();
			}
			if (list != null) {
				return list.size();
			}
			return 0;
		} catch (ServerErrorException e) {
			logError(e);
			return 0;
		}
	}

	@Override
	public int count(String type, boolean deleted, String criteria, boolean distinct, IConnectionUserBean currentUser) {
		try {
			// TODO DataAccess can not count items.
			// TODO DataAccess can not select distinct items.
			// TODO Support user substitution (if real current user possess the associated right).
			BeanMapList list = dao.getList(type,"",criteria,null,0,1,deleted); //$NON-NLS-1$
			if (list instanceof BeanMapPartialList) {
				return ((BeanMapPartialList)list).getTotal();
			}
			if (list != null) {
				return list.size();
			}
			return 0;
		} catch (ServerErrorException e) {
			logError(e);
			return 0;
		}
	}

	@Override
	public int count(MetaDataEntity entity, boolean deleted, ISearchCriteria criteria, boolean distinct,
			IConnectionUserBean currentUser) {
		try {
			// TODO DataAccess can not count items.
			// TODO DataAccess can not select distinct items.
			// TODO Support user substitution (if real current user possess the associated right).
			BeanMapList list = dao.getList(entity.getType(),"",criteria,null,0,1,deleted); //$NON-NLS-1$
			if (list instanceof BeanMapPartialList) {
				return ((BeanMapPartialList)list).getTotal();
			}
			if (list != null) {
				return list.size();
			}
			return 0;
		} catch (ServerErrorException e) {
			logError(e);
			return 0;
		}
	}

	@Override
	public boolean test(MetaDataEntity entity, ISearchCriteria criteria, IConnectionUserBean currentUser) {
		try {
			// TODO DataAccess can not count items.
			// TODO Support user substitution (if real current user possess the associated right).
			BeanMapList list = dao.getList(entity.getType(),"",criteria,null,0,1,false); //$NON-NLS-1$
			return (list != null) && (list.size() > 0);
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean test(BeanMap item, ISearchCriteria criteria, IConnectionUserBean currentUser) {
		try {
			// TODO DataAccess can not count items.
			// TODO Support user substitution (if real current user possess the associated right).
			BeanMapList list = dao.getList(item.getType(),"",AndCriteria.and(criteria,new IdEqualCriteria(item.getId())),null,0,1,false); //$NON-NLS-1$
			return (list != null) && (list.size() > 0);
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean test(MetaDataEntity entity, int itemId, ISearchCriteria criteria, boolean deleted, IConnectionUserBean currentUser) {
		try {
			// TODO DataAccess can not count items.
			// TODO Support user substitution (if real current user possess the associated right).
			BeanMapList list = dao.getList(entity.getType(), "", AndCriteria.and(criteria, new IdEqualCriteria(itemId)), null, 0, 1, deleted); //$NON-NLS-1$
			return (list != null) && (list.size() > 0);
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean test(MetaDataEntity entity, int itemId, ISearchCriteria criteria, IConnectionUserBean currentUser) {
		return test(entity, itemId, criteria, false, currentUser);
	}

	@Override
	public BeanMap selectionFirst(String type, String attributes, boolean deleted, String attributeTest, Object value) {
		try {
			BeanMapList list = dao.getList(type,attributes,new EqualCriteria(attributeTest, value.toString()),null,0,1,deleted);
			if ((list != null) && (list.size() >= 1)) {
				return list.get(0);
			}
		} catch (ServerErrorException e) {
			logError(e);
		}
		return null;
	}

	@Override
	public BeanMap selectionFirst(MetaDataEntity entity, String attributes, boolean deleted, String attributeTest,
			Object value) {
		try {
			BeanMapList list = dao.getList(entity.getType(),attributes,new EqualCriteria(attributeTest, value.toString()),null,0,1,deleted);
			if ((list != null) && (list.size() >= 1)) {
				return list.get(0);
			}
		} catch (ServerErrorException e) {
			logError(e);
		}
		return null;
	}

	@Override
	public BeanMap selectionFirst(MetaDataEntity entity, String attributes, boolean deleted,
			ReferenceLine attributeTest, Object value) {
		try {
			BeanMapList list = dao.getList(entity.getType(),attributes,new EqualCriteria(attributeTest.getCode(), value.toString()),null,0,1,deleted);
			if ((list != null) && (list.size() >= 1)) {
				return list.get(0);
			}
		} catch (ServerErrorException e) {
			logError(e);
		}
		return null;
	}

	@Override
	public BeanMap selectionFirst(List<ReferenceLine> attributes, boolean deleted, ReferenceLine attributeTest,
			Object value) {
		try {
			BeanMapList list = dao.getList(attributes.get(0).getOriginEntity().getType(),ReferenceLine.getCodes(attributes),new EqualCriteria(attributeTest.getCode(), value.toString()),null,0,1,deleted);
			if ((list != null) && (list.size() >= 1)) {
				return list.get(0);
			}
		} catch (ServerErrorException e) {
			logError(e);
		}
		return null;
	}

	@Override
	public BeanMap selectionFirst(String type, String attributes, boolean deleted, String criteria,
			IConnectionUserBean currentUser) {
		try {
			// TODO Support user substitution (if real current user possess the associated right).
			BeanMapList list = dao.getList(type,attributes,criteria,null,0,1,deleted);
			if ((list != null) && (list.size() >= 1)) {
				return list.get(0);
			}
		} catch (ServerErrorException e) {
			logError(e);
		}
		return null;
	}

	@Override
	public BeanMap selectionFirst(List<ReferenceLine> attributes, boolean deleted, ISearchCriteria criteria,
			IConnectionUserBean currentUser) {
		try {
			// TODO Support user substitution (if real current user possess the associated right).
			BeanMapList list = dao.getList(attributes.get(0).getOriginEntity().getType(),ReferenceLine.getCodes(attributes),criteria,null,0,1,deleted);
			if ((list != null) && (list.size() >= 1)) {
				return list.get(0);
			}
		} catch (ServerErrorException e) {
			logError(e);
		}
		return null;
	}

	@Override
	public BeanMap selectionFirst(MetaDataEntity entity,
			List<ReferenceLine> attributes, boolean deleted,
			ISearchCriteria criteria, IConnectionUserBean currentUser) {
		try {
			// TODO Support user substitution (if real current user possess the associated right).
			BeanMapList list = dao.getList(entity.getType(),ReferenceLine.getCodes(attributes),criteria,null,0,1,deleted);
			if ((list != null) && (list.size() >= 1)) {
				return list.get(0);
			}
		} catch (ServerErrorException e) {
			logError(e);
		}
		return null;
	}

	@Override
	public boolean linkAdd(BeanMap source, String linkCode, int destId) {
		try {
			dao.addLink(source.getType(), source.getId(), linkCode, destId);
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean linkAdd(BeanMap source, String linkCode, BeanMap dest) {
		try {
			dao.addLink(source.getType(), source.getId(), linkCode, dest.getId());
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean linkAdd(String sourceType, String linkCode, int sourceId, int destId) {
		try {
			dao.addLink(sourceType, sourceId, linkCode, destId);
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean linkAdd(MetaDataLink link, int sourceId, int destId) {
		try {
			dao.addLink(link.getParent().getType(), sourceId, link.getCode(), destId);
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean linkTest(String sourceType, String linkCode, int sourceId, int destId) {
		return dao.testLink(sourceType, sourceId, linkCode, destId, false);
	}

	@Override
	public boolean linkTest(MetaDataLink link, int sourceId, int destId) {
		return dao.testLink(link.getParent().getType(), sourceId, link.getCode(), destId, false);
	}

	@Override
	public boolean linkTest(MetaDataLink link, int sourceId, int destId, boolean ignoreSubdivision) {
		return dao.testLink(link.getParent().getType(), sourceId, link.getCode(), destId, ignoreSubdivision);
	}

	@Override
	public boolean linkTest(List<MetaDataLink> links, int sourceId, int destId, boolean ignoreSubdivision) {
		if (links == null) {
			return false;
		}
		StringBuilder linkCode = new StringBuilder();
		String type = null;
		for (MetaDataLink l : links) {
			if (type == null) {
				type = l.getParent().getType();
			} else {
				linkCode.append('+');
			}
			linkCode.append(l.getCode());
		}
		if ((type == null) || linkCode.isEmpty()) {
			return false;
		}
		return dao.testLink(type, sourceId, linkCode.toString(), destId, ignoreSubdivision);
	}
	
	@Override
	public boolean linkRemove(String sourceType, String linkCode, int sourceId, int destId) {
		try {
			dao.removeLink(sourceType, sourceId, linkCode, destId);
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public boolean linkRemove(MetaDataLink link, int sourceId, int destId) {
		try {
			dao.removeLink(link.getParent().getType(), sourceId, link.getCode(), destId);
			return true;
		} catch (ServerErrorException e) {
			logError(e);
			return false;
		}
	}

	@Override
	public BeanMapList linkSelection(String sourceType, String linkCode, int sourceId) {
		// DataAccess ne peut pas récupérer une liste d'association sans en connaitre le type !!!
		MetaDataEntity entity = null;
		try {
			entity = dao.getEntity(sourceType);
		} catch (ServerErrorException e) {
			logError(e);
		}
		if (entity == null) {
			return null;
		}
		MetaDataLink link = entity.getLink(linkCode);
		return linkSelection(link, sourceId);
	}

	@Override
	public BeanMapList linkSelection(MetaDataLink link, int sourceId) {
		try {
			return dao.getLinks(link.getParent().getType(), sourceId, link.getCode(), link.getType());
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMapList linkSelection(String sourceType, String linkCode, int sourceId, String attributes,
			boolean deleted, String attributeTest, Object value) {
		// DataAccess ne peut pas récupérer une liste d'association sans en connaitre le type !!!
		MetaDataEntity entity = null;
		try {
			entity = dao.getEntity(sourceType);
		} catch (ServerErrorException e) {
			logError(e);
		}
		if (entity == null) {
			return null;
		}
		MetaDataLink link = entity.getLink(linkCode);
		return linkSelection(link, sourceId, attributes, deleted, attributeTest, value);
	}

	@Override
	public BeanMapList linkSelection(MetaDataLink link, int sourceId, String attributes, boolean deleted,
			String attributeTest, Object value) {
		try {
			return dao.getLinks(new BeanMap(link.getParent().getType(), sourceId), 
					link.getCode(), 
					link.getType(), 
					attributes,
					new XmlCriteriaStream().toXML(new EqualCriteria(attributeTest, value.toString())), null, 0, -1, deleted, false);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMapList linkSelection(MetaDataLink link, int sourceId, List<ReferenceLine> attributes, boolean deleted,
			ReferenceLine attributeTest, Object value) {
		try {
			return dao.getLinks(new BeanMap(link.getParent().getType(), sourceId), 
					link.getCode(), 
					link.getType(), 
					ReferenceLine.getCodes(attributes),
					new XmlCriteriaStream().toXML(new EqualCriteria(attributeTest.getCode(), value.toString())), null, 0, -1, deleted, false);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMapList linkSelection(String sourceType, String linkCode, int sourceId, String attributes,
			boolean deleted, String criteria, boolean distinct, String orders, IConnectionUserBean currentUser,
			int page, int limit) {
		// TODO Support user substitution (if real current user possess the associated right).
		// DataAccess ne peut pas récupérer une liste d'association sans en connaitre le type !!!
		MetaDataEntity entity = null;
		try {
			entity = dao.getEntity(sourceType);
		} catch (ServerErrorException e) {
			logError(e);
		}
		if (entity == null) {
			return null;
		}
		MetaDataLink link = entity.getLink(linkCode);
		try {
			return dao.getLinks(new BeanMap(link.getParent().getType(), sourceId),
					link.getCode(), link.getType(), attributes, criteria, orders, page, limit, deleted, false);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMapList linkSelection(MetaDataLink link, int sourceId, List<ReferenceLine> attributes, boolean deleted,
			ISearchCriteria criteria, boolean distinct, List<ReferenceLine> orders, IConnectionUserBean currentUser,
			int page, int limit) {
		try {
			// TODO Support user substitution (if real current user possess the associated right).
			return dao.getLinks(new BeanMap(link.getParent().getType(), sourceId), 
					link.getCode(), 
					link.getType(), 
					ReferenceLine.getCodes(attributes),
					new XmlCriteriaStream().toXML(criteria),
					ReferenceLine.getCodes(orders), page, limit, deleted, false);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public int linkCount(MetaDataLink link, int sourceId, boolean deleted, ISearchCriteria criteria, boolean distinct,
			IConnectionUserBean currentUser) {
		try {
			// TODO DataAccess can not count items.
			// TODO DataAccess can not select distinct items.
			// TODO Support user substitution (if real current user possess the associated right).
			BeanMapList list = dao.getLinks(new BeanMap(link.getParent().getType(), sourceId), 
					link.getCode(), 
					link.getType(), 
					null,
					new XmlCriteriaStream().toXML(criteria),
					null, 0, 1, deleted, false);
			if (list instanceof BeanMapPartialList) {
				return ((BeanMapPartialList)list).getTotal();
			}
			if (list != null) {
				return list.size();
			}
		} catch (ServerErrorException e) {
			logError(e);
		}
		return 0;
	}

	@Override
	public void addDomain(String domain) {
		domains.add(domain);
	}

	@Override
	public boolean sameDomain(MetaDataEntity entity) {
		for (String d: domains) {
			if (d.equalsIgnoreCase(entity.getDomain())) {
				return true;
			}
		}
		return false;
	}

	@Override
	public boolean sameDomain(String domain) {
		for (String d: domains) {
			if (d.equalsIgnoreCase(domain)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public BeanMapList linkSelection(List<MetaDataLink> links, int sourceId, List<ReferenceLine> attributes, boolean deleted,
			boolean ignoreSubdivision, ReferenceLine attributeTest, Object value) {
		if ((links == null) || links.isEmpty()) {
			return new BeanMapList();
		}
		StringBuilder codes = new StringBuilder();
		for (MetaDataLink l: links) {
			if (!codes.isEmpty()) {
				codes.append('+');
			}
			codes.append(l.getCode());
		}
		try {
			return dao.getLinks(new BeanMap(links.get(0).getParent().getType(), sourceId), 
					codes.toString(), 
					links.get(links.size() - 1).getType(), 
					ReferenceLine.getCodes(attributes),
					new XmlCriteriaStream().toXML(new EqualCriteria(attributeTest.getCode(), value.toString())), null, 0, -1, deleted, ignoreSubdivision);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public BeanMapList linkSelection(List<MetaDataLink> links, int sourceId, List<ReferenceLine> attributes, boolean deleted,
			ISearchCriteria criteria, boolean distinct, boolean ignoreSubdivision, List<ReferenceLine> orders,
			IConnectionUserBean currentUser, int page, int limit) {
		if ((links == null) || links.isEmpty()) {
			return new BeanMapList();
		}
		StringBuilder codes = new StringBuilder();
		for (MetaDataLink l: links) {
			if (!codes.isEmpty()) {
				codes.append('+');
			}
			codes.append(l.getCode());
		}
		try {
			// TODO Support user substitution (if real current user possess the associated right).
			return dao.getLinks(new BeanMap(links.get(0).getParent().getType(), sourceId), 
					codes.toString(), 
					links.get(links.size() - 1).getType(), 
					ReferenceLine.getCodes(attributes),
					new XmlCriteriaStream().toXML(criteria),
					ReferenceLine.getCodes(orders), page, limit, deleted, ignoreSubdivision);
		} catch (ServerErrorException e) {
			logError(e);
			return null;
		}
	}

	@Override
	public int linkCount(List<MetaDataLink> links, int sourceId, boolean deleted, ISearchCriteria criteria, boolean distinct,
			boolean ignoreSubdivision, IConnectionUserBean currentUser) {
		if ((links == null) || links.isEmpty()) {
			return 0;
		}
		StringBuilder codes = new StringBuilder();
		for (MetaDataLink l: links) {
			if (!codes.isEmpty()) {
				codes.append('+');
			}
			codes.append(l.getCode());
		}
		try {
			// TODO DataAccess can not count items.
			// TODO DataAccess can not select distinct items.
			// TODO Support user substitution (if real current user possess the associated right).
			BeanMapList list = dao.getLinks(new BeanMap(links.get(0).getParent().getType(), sourceId), 
					codes.toString(), 
					links.get(links.size() - 1).getType(), 
					null,
					new XmlCriteriaStream().toXML(criteria),
					null, 0, 1, deleted, ignoreSubdivision);
			if (list instanceof BeanMapPartialList) {
				return ((BeanMapPartialList) list).getTotal();
			}
			if (list != null) {
				return list.size();
			}
		} catch (ServerErrorException e) {
			logError(e);
		}
		return 0;
	}

	@Override
	public BeanMap create(BeanMap item, IConnectionUserBean currentUser) {
		return create(item);
	}

	@Override
	public BeanMap create(String type, String attributes, List<Object> values, IConnectionUserBean currentUser) {
		return create(type, attributes, values);
	}

	@Override
	public BeanMap create(MetaDataEntity entity, String attributes, List<Object> values, IConnectionUserBean currentUser) {
		return create(entity, attributes, values);
	}

	@Override
	public BeanMap create(MetaDataEntity entity, IConnectionUserBean currentUser, String attributes, Object... values) {
		return create(entity, attributes, values);
	}

	@Override
	public BeanMap create(MetaDataAttribute attribute, Object value, IConnectionUserBean currentUser) {
		return create(attribute, value);
	}

	@Override
	public BeanMap create(List<MetaDataAttribute> attributes, List<Object> values, IConnectionUserBean currentUser) {
		return create(attributes, values);
	}

	@Override
	public BeanMap create(MetaDataEntity entity, List<MetaDataAttribute> attributes, List<Object> values, IConnectionUserBean currentUser) {
		return create(entity, attributes, values);
	}

	@Override
	public boolean delete(BeanMap item, boolean hardDelete, IConnectionUserBean currentUser) {
		return delete(item, hardDelete);
	}

	@Override
	public boolean delete(String type, int itemId, boolean hardDelete, IConnectionUserBean currentUser) {
		return delete(type, itemId, hardDelete);
	}

	@Override
	public boolean delete(MetaDataEntity entity, int itemId, boolean hardDelete, IConnectionUserBean currentUser) {
		return delete(entity, itemId, hardDelete);
	}

	@Override
	public boolean undelete(BeanMap item, IConnectionUserBean currentUser) {
		return undelete(item);
	}

	@Override
	public boolean undelete(String type, int itemId, IConnectionUserBean currentUser) {
		return undelete(type, itemId);
	}

	@Override
	public boolean undelete(MetaDataEntity entity, int itemId, IConnectionUserBean currentUser) {
		return undelete(entity, itemId);
	}

	@Override
	public boolean update(BeanMap item, IConnectionUserBean currentUser) {
		return update(item);
	}

	@Override
	public boolean update(String type, int itemId, String attributes, List<Object> values, IConnectionUserBean currentUser) {
		return update(type, itemId, attributes, values);
	}

	@Override
	public boolean update(MetaDataEntity entity, int itemId, String attributes, List<Object> values, IConnectionUserBean currentUser) {
		return update(entity, itemId, attributes, values);
	}

	@Override
	public boolean update(MetaDataEntity entity, IConnectionUserBean currentUser, int itemId, String attributes, Object... values) {
		return update(entity, itemId, attributes, values);
	}

	@Override
	public boolean update(int itemId, MetaDataAttribute attribute, Object value, IConnectionUserBean currentUser) {
		return update(itemId, attribute, value);
	}

	@Override
	public boolean update(int itemId, List<MetaDataAttribute> attributes, List<Object> values, IConnectionUserBean currentUser) {
		return update(itemId, attributes, values);
	}

	@Override
	public boolean update(MetaDataEntity entity, int itemId, List<MetaDataAttribute> attributes, List<Object> values, IConnectionUserBean currentUser) {
		return update(entity, itemId, attributes, values);
	}

	@Override
	public boolean linkAdd(BeanMap source, String linkCode, int destId, IConnectionUserBean currentUser) {
		return linkAdd(source, linkCode, destId);
	}

	@Override
	public boolean linkAdd(BeanMap source, String linkCode, BeanMap dest, IConnectionUserBean currentUser) {
		return linkAdd(source, linkCode, dest);
	}

	@Override
	public boolean linkAdd(String sourceType, String linkCode, int sourceId, int destId, IConnectionUserBean currentUser) {
		return linkAdd(sourceType, linkCode, sourceId, destId);
	}

	@Override
	public boolean linkAdd(MetaDataLink link, int sourceId, int destId, IConnectionUserBean currentUser) {
		return linkAdd(link, sourceId, destId);
	}

	@Override
	public boolean linkRemove(String sourceType, String linkCode, int sourceId, int destId, IConnectionUserBean currentUser) {
		return linkRemove(sourceType, linkCode, sourceId, destId);
	}

	@Override
	public boolean linkRemove(MetaDataLink link, int sourceId, int destId, IConnectionUserBean currentUser) {
		return linkRemove(link, sourceId, destId);
	}

	@Override
	public Date lastModification(MetaDataEntity entity, boolean deleted) {
		try {
			return dao.getWebServicesAccess().head("/data/" + entity.getType());
		} catch (ServerErrorException e) {
			logError(e);
			return entity.getDate();
		}
	}

}
