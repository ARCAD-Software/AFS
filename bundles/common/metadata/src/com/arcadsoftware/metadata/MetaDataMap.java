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
package com.arcadsoftware.metadata;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.restlet.data.Language;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.IBeanMap;
import com.arcadsoftware.beanmap.IDatedBean;
import com.arcadsoftware.beanmap.IDeletableBean;
import com.arcadsoftware.beanmap.IIdentifiedBean;
import com.arcadsoftware.beanmap.ITypedBean;
import com.arcadsoftware.metadata.criteria.AndCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.criteria.IdEqualCriteria;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.osgi.ISODateFormater;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.thoughtworks.xstream.converters.ConversionException;

/**
 * This class implement a BeanMap-like object that is connected to a Data storage through its MetaDataEntity.
 * 
 * <p>
 * Using this class is <b>not</b> an efficient way to access Datas with MetaData API. Any way, if a connected
 * bean is require to process data into a BeanMap compatible API, this class may be useful.
 * 
 * <p>
 * This class test Access Right if "currentUser" property is defined.
 * 
 * Creation Date: 15 marsh 2012
 */
public class MetaDataMap implements Map<String, Object>, IBeanMap, IIdentifiedBean, ITypedBean, IDatedBean, IDeletableBean,
Cloneable, Comparable<IBeanMap> {

	private MetaDataEntity entity;
	private IConnectionUserBean currentUser;
	private int id;
	private BeanMap value;
	private long load;
	private Language language;
	
	public MetaDataMap(MetaDataEntity entity, int id, IConnectionUserBean currentUser, Language language) {
		super();
		this.entity = entity;
		this.currentUser = currentUser;
		this.id = id;
		if (language == null) {
			this.language = Language.ENGLISH_US;
		} else {
			this.language = language;
		}
	}
	
	private boolean load() {
		if ((entity == null) || (id <= 0)) {
			return false;
		}
		long t = System.currentTimeMillis();
		if ((value == null) || ((t - load) > 5000)) {
			// Test Read Right...
			value = new BeanMap(entity.getType(), id);
			List<ReferenceLine> attributes = entity.getAllAttributes();
			// Test Read Right...
			Iterator<ReferenceLine> itt = attributes.iterator();
			while (itt.hasNext()) {
				MetaDataAttribute a = itt.next().getLastAttribute();
				if ((currentUser != null) && (a.getRightRead(false) != null)) {
					if (!entity.getMapper().test(entity, a.getRightRead(false), currentUser)) {
						itt.remove();
						continue;
					}
				}
				if (a.isTranslatable()) {
					a.translate(value, language);
				}
			}				
			ISearchCriteria criteria = entity.getRightRead();
			if (criteria instanceof AndCriteria) {
				((AndCriteria)criteria).add(new IdEqualCriteria(id));
			} else {
				criteria = new AndCriteria(criteria, new IdEqualCriteria(id));
			}
			BeanMapList result = entity.getMapper().selection(entity,attributes, true, criteria , false, null, currentUser, 0, 1);
			if ((result == null) || (result.size() == 0)) {
				id = 0;
				return false;
			}
			Activator.getInstance().test(MetaDataTest.EVENTCODE_READ, entity, result, currentUser, language);
			if ((result == null) || (result.size() == 0)) {
				id = 0;
				return false;
			}
			if ((entity.getMetadata().getBoolean(MetaDataEntity.METADATA_EVENTONSELECTION)) && (result.size() > 1)) {
				Activator.getInstance().fireSelectionEvent(getEntity(),result,currentUser);
			}
			value.addAll(result.get(0));
		}
		return true;
	}

	private void update(ArrayList<MetaDataAttribute> atts, ArrayList<Object> values) {
		if ((entity == null) || (id <= 0) || entity.isReadOnly()) {
			return;
		}
		// Test Update Right...
		if ((currentUser != null) && !entity.getMapper().test(entity, id, entity.getRightUpdate(), currentUser)) {
			return;
		}
		// attlist ne doit pas être modifié...
		BeanMap original = entity.dataSelection(id, null, true);
		if (original == null) {
			return;
		}
		BeanMap modified = new BeanMap(entity.getType(), id);
		Iterator<MetaDataAttribute> itt = atts.iterator();
		Iterator<Object> itv = values.iterator();
		while (itt.hasNext()) {
			MetaDataAttribute att = itt.next();
			if ((att.getRightUpdate(false) == null) || //
					(currentUser == null) ||
					entity.getMapper().test(entity, id, att.getRightUpdate(false), currentUser)) {
				if (itv.hasNext()) {
					modified.put(att.getCode(), itv.next());
				}
			} else {
				itt.remove();
				if (itv.hasNext()) {
					itv.next();
					itv.remove();
				}
			}
		}
		List<IMetaDataModifyListener> listeners = Activator.getInstance().getModifyListener(getType());
		try {
			for(IMetaDataModifyListener listener:listeners) {
				if (!listener.testModification(entity, original, modified, atts, currentUser, language)) {
					return;
				}
			}
		} catch (ResourceException e) {
			return;
		}
		if (!Activator.getInstance().test(MetaDataTest.EVENTCODE_BEFOREUPDATE, entity, original, modified, atts, currentUser, language)) {
			return;
		}
		entity.getMapper().update(modified);
		if (modified.getId() == 0) {
			return;
		}
		load = 0;
		try {
			for(IMetaDataModifyListener listener:listeners) {
				listener.postModification(entity, original, modified, atts, currentUser, language);
			}
		} catch (ResourceException e) {
			return;
		}
		Activator.getInstance().test(MetaDataTest.EVENTCODE_AFTERUPDATE, entity, original, modified, atts, currentUser, language);
		Activator.getInstance().fireUpdateEvent(getEntity(), original, modified, currentUser);
	}

	public void undelete() {
		if ((entity == null) || (id <= 0) || entity.isReadOnly()) {
			return;
		}
		// Test Update Right...
		if ((currentUser != null) && !entity.getMapper().test(entity, id, entity.getRightUpdate(), currentUser)) {
			return;
		}
		entity.dataUndelete(id);
		if (value != null) {
			value.setDeleted(false);
		} else {
			value = new BeanMap(entity.getType(), id);
		}
		Activator.getInstance().fireUndeleteEvent(getEntity(), value, currentUser);
	}

	public void clear() {
		// Delete Data...
		if ((entity == null) || (id <= 0) || entity.isReadOnly()) {
			return;
		}
		// Test Delete Right...
		BeanMap item = new BeanMap(entity.getType(), id);
		try {
			if ((currentUser != null) && 
					(!entity.getMapper().test(entity, id, entity.getRightDelete(), currentUser) ||
							!Activator.getInstance().test(MetaDataTest.EVENTCODE_BEFOREDELETE, entity, item, currentUser, language))) {
				return;
			}
			entity.dataDelete(id, false);
			// Post Process...
			Activator.getInstance().test(MetaDataTest.EVENTCODE_AFTERDELETE, entity, item, currentUser, language);
			Activator.getInstance().fireDeleteEvent(entity, item, currentUser);
			id = 0;
		} catch (ResourceException e) {
			Activator.getInstance().debug(e);
		}
	}
	
	public int compareTo(IBeanMap bean) {
		if ((entity == null) || (entity.getType() == null)) {
			return -1;
		}
		int i = entity.getType().compareTo(bean.getType());
		if (i != 0) {
			return i;
		}
		return id - bean.getId();
	}

	public String getType() {
		if (entity == null) {
			return null;
		}
		return entity.getType();
	}

	public boolean equalsType(ITypedBean bm) {
		if ((entity == null) || (entity.getType() == null) || (bm == null)) {
			return false;
		}
		return entity.getType().equals(bm.getType());
	}

	public int getId() {
		return id;
	}

	/**
	 * @return The currently connected User.
	 */
	public IConnectionUserBean getCurrentUser() {
		return currentUser;
	}

	public MetaDataEntity getEntity() {
		return entity;
	}
	
	public <T> T get(String key, Class<T> clazz) {
		Object result = get(key);
		if (result == null) {
			return null;
		}
		if (clazz.isInstance(result)) {
			return clazz.cast(result);
		}
		if (clazz.equals(Integer.class)) {
			// Return the BeanMap Id.
			if (result instanceof BeanMap) {
				result = ((BeanMap) result).getId();
			} else {
				try {
					result = Integer.decode(result.toString());
				} catch (NumberFormatException e) {
					return null;
				}
			}
			if (clazz.isInstance(result)) {
				return clazz.cast(result);
			}
		} else if (clazz.equals(String.class)) {
			result = result.toString();
			if (clazz.isInstance(result)) {
				return clazz.cast(result);
			}
		} else if (clazz.equals(Date.class)) {
			String d = result.toString();
			if (ISODateFormater.mayIsoDate(d)) {
				try {
					result = ISODateFormater.toDate(d);
				} catch (ParseException e) {}
			} else {
				try {
					result = BeanMap.DATECONVERTER.fromString(d);
				} catch (ConversionException e) {
					return null;
				}
			}
			if (clazz.isInstance(result)) {
				return clazz.cast(result);
			}
		} else if (clazz.equals(Float.class)) {
			try {
				result = Float.valueOf(result.toString());
			} catch (NumberFormatException e) {
				return null;
			}
			if (clazz.isInstance(result)) {
				return clazz.cast(result);
			}
		}
		return null;
	}

	public int getInt(String key) {
		Object o = get(key);
		if (o == null) {
			return 0;
		}
		if (o instanceof Integer) {
			return (Integer) o;
		}
		if (o instanceof BigInteger) {
			return ((BigInteger) o).intValue();
		}
		if (o instanceof BigDecimal) {
			return ((BigDecimal) o).intValue();
		}
		if (o instanceof String) {
			try {
				return Integer.parseInt((String) o);
			} catch (NumberFormatException e) {
				// Do nothing
			}
		}
		return -1;
	}
	
	public float getFloat(String key) {
		Object o = get(key);
		if (o == null) {
			return 0F;
		}
		if (o instanceof Float) {
			return (Float) o;
		}
		try {
			return Float.parseFloat(o.toString());
		} catch (NumberFormatException e) {
			return -1F;
		}
	}
	
	public String getString(String key) {
		Object o = get(key);
		if (o == null) {
			return null;
		}
		if (o instanceof String) {
			return (String) o;
		}
		return o.toString();
	}

	@SuppressWarnings("unchecked")
	public Set<String> keys() {
		if (entity == null) {
			return Collections.EMPTY_SET;
		}
		return entity.getCodes();
	}

	public IBeanMap addAll(IBeanMap bean) {
		if (entity != null) {
			for(Entry<String, Object> e:bean.entrySet()) {
				put(e.getKey(),e.getValue());
			}
		}
		return this;
	}

	public int size() {
		if (entity == null) {
			return 0;
		}
		return entity.getCodes().size();
	}

	public boolean containsKey(Object key) {
		if (entity == null) {
			return false;
		}
		String k = key.toString();
		return (entity.getAttribute(k) != null) || (entity.getLink(k) != null);
	}

	public Object get(Object key) {
		if (key == null) {
			return null;
		}
		return get(key.toString());
	}

	public boolean isDeleted() {
		if ((id <= 0) || (entity == null)) {
			return true;
		}
		return (!load()) || value.isDeleted();
	}

	public Set<String> keySet() {
		return keys();
	}

	public Date getDate() {
		if (!load()) {
			return null;
		}
		return value.getDate();
	}

	public void setDate(Date date) {
		update(new ArrayList<MetaDataAttribute>(), new ArrayList<Object>());
	}

	public boolean moreRecent(IDatedBean bm) {
		if (!load()) {
			return false;
		}
		return (value.getDate() != null) && value.getDate().after(bm.getDate());
	}

	public Object get(String key) {
		if (entity.getAttribute(key) != null) {
			if (!load()) {
				return null;
			}
			return value.get(key);
		}
		MetaDataLink link = entity.getLink(key);
		if (link != null) {
			return new MetaDataMapList(this, link);
		}
		return null;
	}

	public boolean isEmpty() {
		return (entity == null) || (id <= 0) || ((entity.getAttributes().size() + entity.getLinks().size()) == 0);
	}

	public boolean containsValue(Object value) {
		if ((value == null) || !load()) {
			return false;
		}
		if (value instanceof IBeanMap) {
			for (MetaDataAttribute a: entity.getAttributesFromType(((IBeanMap)value).getType())) {
				if (((IBeanMap)value).getId() == this.value.getInt(a.getCode())) {
					return true;
				}
			}
			return false;
		}
		return this.value.containsValue(value);
	}

	public Object put(String key, Object value) {
		if (entity == null) {
			return null;
		}
		MetaDataAttribute att = entity.getAttribute(key);
		if (att != null) {
			Object oldVal = null;
			if (load()) {
				oldVal = this.value.get(key);
			}
			ArrayList<MetaDataAttribute> atts = new ArrayList<MetaDataAttribute>();
			atts.add(att);
			ArrayList<Object> values = new ArrayList<Object>();
			values.add(value);
			update(atts, values);
			return oldVal;
		}
		MetaDataLink link = entity.getLink(key);
		if ((link != null) && (value instanceof IBeanMap)) {
			BeanMap linked = new BeanMap(((IBeanMap)value).getType(),((IBeanMap)value).getId());
			if (entity.getMapper().test(linked, link.getRightCreate(true), currentUser) &&
				getEntity().getMapper().linkAdd(link, id, ((IBeanMap)value).getId())) {
				Activator.getInstance().fireLinkEvent(getEntity(), link, new BeanMap(entity.getType(),id), linked, currentUser);
			}
		}
		return null;
	}

	public Object remove(Object key) {
		if (key == null) {
			return null;
		}
		return put(key.toString(), null);
	}

	public void putAll(Map<? extends String, ? extends Object> m) {
		if ((entity == null) || (id <= 0)) {
			return;
		}
		ArrayList<MetaDataAttribute> atts = new ArrayList<MetaDataAttribute>();
		ArrayList<Object> values = new ArrayList<Object>();
		for(java.util.Map.Entry<? extends String, ? extends Object> e:m.entrySet()) {
			MetaDataAttribute att = entity.getAttribute(e.getKey());
			if (att != null) {
				atts.add(att);
				values.add(value);
			}
		}
		update(atts, values);
	}

	public Collection<Object> values() {
		if (!load()) {
			return new ArrayList<Object>();
		}
		ArrayList<Object> values = new ArrayList<Object>();
		for(String code:entity.getAttributes().keySet()) {
			values.add(value.get(code));
		}
		return values;
	}

	@SuppressWarnings("unchecked")
	public Set<Entry<String, Object>> entrySet() {
		if (!load()) {
			return Collections.EMPTY_SET;
		}
		return value.entrySet();
	}

	public Language getLanguage() {
		return language;
	}

}
