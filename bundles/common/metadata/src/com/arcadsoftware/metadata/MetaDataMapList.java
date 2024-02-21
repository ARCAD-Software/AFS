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
package com.arcadsoftware.metadata;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.restlet.data.Language;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.IBeanMap;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public class MetaDataMapList implements List<MetaDataMap> {

	private ArrayList<MetaDataMap> values;
	private MetaDataMap parent;
	private MetaDataLink link;
	
	public MetaDataMapList() {
		super();
	}
	
	public MetaDataMapList(MetaDataMap parent,MetaDataLink link) {
		this();
		this.parent = parent;
		this.link = link;
	}

	private synchronized boolean list() {
		if ((values != null) || (parent == null) || (link == null)) {
			return false;
		}
		BeanMapList list = parent.getEntity().getMapper().linkSelection(link, parent.getId(), new ArrayList<ReferenceLine>(), false, link.getRightList(true),false,null,parent.getCurrentUser(), -1, -1);
		if (list == null) {
			values = new ArrayList<MetaDataMap>(0);
			return true;
		}
		values = new ArrayList<MetaDataMap>(list.size());
		MetaDataEntity entity = link.getRefEntity();
		Language language = parent.getLanguage();
		IConnectionUserBean currentUser = parent.getCurrentUser();
		for(BeanMap bean:list) {
			values.add(new MetaDataMap(entity, bean.getId(), currentUser, language));
		}
		return true;
	}

	private boolean linkTo(IBeanMap o) {
		if ((parent == null) || (link == null)) {
			return false;
		}
		BeanMap linked = new BeanMap(o.getType(),o.getId());
		if (parent.getEntity().getMapper().test(linked, link.getRightCreate(true), parent.getCurrentUser()) &&
				parent.getEntity().getMapper().linkAdd(link, parent.getId(), o.getId())) {
			Activator.getInstance().fireLinkEvent(parent.getEntity(), link, new BeanMap(parent.getType(),parent.getId()), linked, parent.getCurrentUser());
			return true;
		}
		return false;
	}

	private boolean unlinkTo(IBeanMap o) {
		if ((parent == null) || (link == null)) {
			return false;
		}
		BeanMap linked = new BeanMap(o.getType(),o.getId());
		if (parent.getEntity().getMapper().test(linked, link.getRightCreate(true), parent.getCurrentUser()) &&
				parent.getEntity().getMapper().linkRemove(link, parent.getId(), o.getId())) {
			Activator.getInstance().fireUnlinkEvent(parent.getEntity(), link, new BeanMap(parent.getType(),parent.getId()), linked, parent.getCurrentUser());
			return true;
		}
		return false;
	}

	public int size() {
		if (values == null) {
			list();
		}
		return values.size();
	}

	public boolean isEmpty() {
		if (values == null) {
			list();
		}
		return values.isEmpty();
	}

	public boolean contains(Object o) {
		if (values != null) {
			return values.contains(o);
		}
		if (o instanceof IBeanMap) {
			return link.getType().equals((((IBeanMap)o).getType())) && parent.getEntity().getMapper().linkTest(link, parent.getId(), ((IBeanMap)o).getId());
		}
		return false;
	}

	public Iterator<MetaDataMap> iterator() {
		if (values == null) {
			list();
		}
		return values.iterator();
	}

	public Object[] toArray() {
		if (values == null) {
			list();
		}
		return values.toArray();
	}

	public <T> T[] toArray(T[] a) {
		if (values == null) {
			list();
		}
		return values.toArray(a);
	}

	public boolean add(MetaDataMap e) {
		if (linkTo(e)) {
			if (values != null) {
				values.add(e);
			}
			return true;
		}
		return false;
	}

	public boolean remove(Object o) {
		if ((o instanceof IBeanMap) && unlinkTo((IBeanMap)o)) {
			if (values != null) {
				values = null;
			}
			return true;
		}
		return false;
	}

	public boolean containsAll(Collection<?> c) {
		if (values != null) {
			return values.containsAll(c);
		}
		for (Object o:c) {
			if (o instanceof IBeanMap) {
				if (!link.getType().equals((((IBeanMap)o).getType())) && parent.getEntity().getMapper().linkTest(link, parent.getId(), ((IBeanMap)o).getId())) {
					return false;
				}
			} else {
				return false;
			}
		}
		return true;
	}

	public boolean addAll(Collection<? extends MetaDataMap> c) {
		boolean result = false;
		for(MetaDataMap o:c) {
			if (add(o) && !result) {
				result = true;
			}
		}
		return result;
	}

	public boolean addAll(int index, Collection<? extends MetaDataMap> c) {
		for(MetaDataMap o:c) {
			add(index++,o);
		}
		return c.size() > 0;
	}

	public boolean removeAll(Collection<?> c) {
		boolean result = false;
		for(Object o:c) {
			if (remove(o) && !result) {
				result = true;
			}
		}
		return result;
	}

	public boolean retainAll(Collection<?> c) {
		if (values == null) {
			list();
		}
		boolean result = false;
		ArrayList<MetaDataMap> val = values;
		for(MetaDataMap bean:val) {
			if (!c.contains(bean)) {
				if (remove(bean) && !result) {
					result = true;
				}
			}
		}
		return result;
	}

	public void clear() {
		if (values == null) {
			list();
		}
		ArrayList<MetaDataMap> val = values;
		for(MetaDataMap bean:val) {
			remove(bean);
		}
	}

	public MetaDataMap get(int index) {
		if (values == null) {
			list();
		}
		return values.get(index);
	}

	public MetaDataMap set(int index, MetaDataMap element) {
		MetaDataMap result = get(index);
		if (result != null) {
			remove(result);
			add(index,element);
		}
		return result;
	}

	public void add(int index, MetaDataMap element) {
		if (linkTo(element) && (values != null)) {
			values.add(index, element);
		}
	}

	public MetaDataMap remove(int index) {
		if (values == null) {
			list();
		}
		MetaDataMap result = values.get(index);
		if (result != null) {
			remove(result);
		}
		return result;
	}

	public int indexOf(Object o) {
		if (values == null) {
			list();
		}
		return values.indexOf(o);
	}

	public int lastIndexOf(Object o) {
		if (values == null) {
			list();
		}
		return values.lastIndexOf(o);
	}

	public ListIterator<MetaDataMap> listIterator() {
		if (values == null) {
			list();
		}
		return values.listIterator();
	}

	public ListIterator<MetaDataMap> listIterator(int index) {
		if (values == null) {
			list();
		}
		return values.listIterator(index);
	}

	public List<MetaDataMap> subList(int fromIndex, int toIndex) {
		if (values == null) {
			list();
		}
		return values.subList(fromIndex, toIndex);
	}

}
