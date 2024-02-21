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
package com.arcadsoftware.metadata.client;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.IDatedBean;
import com.arcadsoftware.beanmap.IIdentifiedBean;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.metadata.client.cache.CachedDatedBean;
import com.arcadsoftware.metadata.client.cache.ICacheableObject;

/**
 * Local, in memory, cache used to store loaded objects.
 * 
 * Creation Date: 1 mars 2011
 */
public class LocalCache {

	/**
	 * duration (in milliseconds) that is it used to limit repeated call to server.
	 */
	public static final int NOTEST_DURATION_DEFAULT = 50;
	
	private int notest;
	private boolean lazzyStage;
	private final Map<String, ICacheableObject> cache = new ConcurrentHashMap<String, ICacheableObject>();
	//private final Map<String, ICacheableObject> cache = Collections.synchronizedMap(new HashMap<String, ICacheableObject>());
	private final Map<String, BeanMapList> lists = new ConcurrentHashMap<String, BeanMapList>();
	private List<MetaDataEntity> entities = null;

	public LocalCache() {
		this(true);
	}
	
	public LocalCache(boolean lazzyStage) {
		this(lazzyStage,NOTEST_DURATION_DEFAULT);
	}

	public LocalCache(boolean lazzyStage, int noTestDuration) {
		super();
		this.lazzyStage = lazzyStage;
		notest = noTestDuration;
	}

	public int getNoTestDuration() {
		return notest;
	}

	public boolean isLazzyStage() {
		return lazzyStage;
	}

	public void setNoTestDuration(int notest) {
		this.notest = notest;
	}

	public void setLazzyStage(boolean lazzyStage) {
		this.lazzyStage = lazzyStage;
	}

	/**
	 * Empty the cache.
	 */
	public void purgeCache() {
		cache.clear();
		lists.clear();
		setEntities(null);
	}

	/**
	 * Return true if we do not need to reload this object.
	 * 
	 * @param key
	 * @param object
	 * @return true if we do not need to reload this object.
	 */
	public boolean testLocal(String key, ICacheableObject object) {
		if (object == null) {
			return false;
		}
		// Lazzy object are always positives
		if (lazzyStage && object.isLazzy()) {
			return true;
		}
		// if last test was recent.
		Date limit = object.getLastTest();
		if ((limit != null) && (limit.after(new Date(System.currentTimeMillis() - notest)))) {
			if (Activator.getInstance() != null) {
				Activator.getInstance().debug(Messages.LocalCache_SkipReload + key);
			}
			return true;
		}
		object.setLastTest(new Date());
		return false;
	}

	/**
	 * Ensure that the cached object (if exist) will fail to the next local test.
	 * 
	 * @param key
	 */
	public void outOfDate(String key) {
		ICacheableObject o = get(key);
		if ((o != null) && !o.isLazzy()) {
			o.setLastTest(null);
			put(key, o);
		}
	}

	/**
	 * Return the typed object from the cache.
	 * 
	 * @return the typed object from the cache.
	 */
	public ICacheableObject get(String key) {
		return cache.get(key);
	}

	/**
	 * Add or update a list object, list object automatically add their identifiable content to the cache.
	 * 
	 * @param key the list key.
	 * @param object
	 */
	@SuppressWarnings("rawtypes")
	public void putList(String key, ICacheableObject object) {
		put(key, object);
		if (object.isCachableList() && (object.getContent() instanceof List)) {
			for (Object o : (List) object.getContent()) {
				if ((o instanceof IIdentifiedBean) && (o instanceof IDatedBean)) {
					put(key + '/' + ((IIdentifiedBean) o).getId(),
							new CachedDatedBean((IDatedBean) o, object.isLazzy()));
				}
			}
		}
	}

	/**
	 * Add or update the cached object.
	 * 
	 * @param key
	 * @param object
	 */
	public void put(String key, ICacheableObject object) {
		cache.put(key, object);
	}

	/**
	 * Remove the corresponding element from the cache.
	 * 
	 * <p>
	 * If this element is going to get put on the cache again, then removing it is useless.
	 */
	public void purge(String key) {
		cache.remove(key);
	}

	public BeanMap getListElement(String type, int id) {
		BeanMap result = null;
		if (lists.containsKey(type)) {
			BeanMapList list = lists.get(type);
			if (list != null) {
				for (BeanMap beanMap : list) {
					if (beanMap.getId() == id) {
						result = beanMap;
						break;
					}
				}
			}
		}
		return result;
	}

	public BeanMapList getList(String type) {
		return lists.get(type);
	}

	public void putList(String type, BeanMapList list) {
		lists.put(type, list);
	}

	public void deleteInLists(BeanMap item) {
		BeanMapList list = lists.get(item.getType());
		list.remove(list.find(item.getId()));
	}

	public void addInLists(BeanMap item) {
		BeanMapList list = lists.get(item.getType());
		list.add(item);
	}

	public void updateInLists(BeanMap item) {
		BeanMapList list = lists.get(item.getType());
		int index = list.findIndex(item.getId());
		list.set(index, item);
	}

	/**
	 * @param entities the entities to set
	 */
	public void setEntities(List<MetaDataEntity> entities) {
		this.entities = entities;
	}

	/**
	 * @return the entities
	 */
	public List<MetaDataEntity> getEntities() {
		return entities;
	}

}
