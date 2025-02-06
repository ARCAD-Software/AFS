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
package com.arcadsoftware.afs.client.core.model;

import com.arcadsoftware.aev.core.collections.ArcadCollection;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

public class BeanMapListCollection extends ArcadCollection {

	public void add(BeanMapCollectionItem item) {
		super.add(item);
	}

	public void populate(BeanMapList list) {
		for (final BeanMap bean : list) {
			add(new BeanMapCollectionItem(bean));
		}
	}

	public static BeanMapListCollection toBeanMapListCollection(BeanMapList list) {
		final BeanMapListCollection collection = new BeanMapListCollection();
		for (final BeanMap bean : list) {
			collection.add(new BeanMapCollectionItem(bean));
		}
		return collection;
	}

	public BeanMapList toBeanMapList() {
		final BeanMapList list = new BeanMapList();
		for (int i = 0; i < count(); i++) {
			final BeanMapCollectionItem item = (BeanMapCollectionItem) items(i);
			list.add(item.getBeanMap());
		}
		return list;
	}

	public BeanMapList toBeanMapList(String type) {
		final BeanMapList list = new BeanMapList();
		for (int i = 0; i < count(); i++) {
			final BeanMapCollectionItem item = (BeanMapCollectionItem) items(i);
			if (item.getBeanMap().getType().equalsIgnoreCase(type)) {
				list.add(item.getBeanMap());
			}
		}
		return list;
	}

	public BeanMapCollectionItem findBeanMap(BeanMap b) {
		for (int i = 0; i < count(); i++) {
			final BeanMapCollectionItem item = (BeanMapCollectionItem) items(i);
			if (item.getBeanMap() == b) {
				return item;
			}
		}
		return null;
	}

	public BeanMapCollectionItem findBeanMap(int id) {
		for (int i = 0; i < count(); i++) {
			final BeanMapCollectionItem item = (BeanMapCollectionItem) items(i);
			if (item.getBeanMap().getId() == id) {
				return item;
			}
		}
		return null;
	}

}
