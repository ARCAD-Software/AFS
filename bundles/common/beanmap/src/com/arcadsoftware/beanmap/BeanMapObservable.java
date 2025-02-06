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
package com.arcadsoftware.beanmap;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.beans.VetoableChangeSupport;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * Implement a BeanMap warper with Observable Pattern facilities.
 * 
 */
public class BeanMapObservable implements Map<String,Object>, IBeanMap, IIdentifiedBean, ITypedBean, IDatedBean, Cloneable {

	private final BeanMap bm;
	private final PropertyChangeSupport pcs = new PropertyChangeSupport(this);
	private final VetoableChangeSupport vcs = new VetoableChangeSupport(this);

	public BeanMapObservable(BeanMap warped) {
		super();
		bm = warped;
	}
	
	/**
     * Add a VetoableChangeListener for a specific attribute.  The listener
     * will be invoked only when a call on fireVetoableChange names that
     * specific attribute.
     * The same listener object may be added more than once.  For each
     * attribute, the listener will be invoked the number of times it was added
     * for that attribute.
     * If <code>attributeName</code> or <code>listener</code> is null, no
     * exception is thrown and no action is taken.
     *
     * @param attributeName  The name of the attribute to listen on.
     * @param listener  The VetoableChangeListener to be added
	 */
	public void addVetoableChangeListener(String attributeName, VetoableChangeListener listener) {
		vcs.addVetoableChangeListener(attributeName, listener);
	}
	
	/**
     * Add a VetoableChangeListener for a specific list of attributes.  The listener
     * will be invoked only when a call on fireVetoableChange names theses
     * specifics attributes.
     * The same listener object may be added more than once.  For each
     * attribute, the listener will be invoked the number of times it was added
     * for a specific attribute.
     * If <code>attributesNames</code> is empty or <code>listener</code> is null, no
     * exception is thrown and no action is taken.
     *
	 * @param attributesNames a names' list of attributes to listen on.
	 * @param listener
	 */
	public void addVetoableChangeListener(String[] attributesNames, VetoableChangeListener listener) {
		if (attributesNames != null) {
			for(String name:attributesNames) {
				vcs.addVetoableChangeListener(name, listener);
			}
		}
	}
	
    /**
     * Add a VetoableListener to the listener list.
     * The listener is registered for all attributes.
     * The same listener object may be added more than once, and will be called
     * as many times as it is added.
     * If <code>listener</code> is null, no exception is thrown and no action
     * is taken.
     *
     * @param listener  The VetoableChangeListener to be added
     */
	public void addVetoableChangeListener(VetoableChangeListener listener) {
		vcs.addVetoableChangeListener(listener);
	}
	
    /**
     * Remove a VetoableChangeListener from the listener list.
     * This removes a VetoableChangeListener that was registered
     * for all attributes.
     * If <code>listener</code> was added more than once to the same event
     * source, it will be notified one less time after being removed.
     * If <code>listener</code> is null, or was never added, no exception is
     * thrown and no action is taken.
     *
     * @param listener  The VetoableChangeListener to be removed
     */
	public void removeVetoableChangeListener(VetoableChangeListener listener) {
		vcs.removeVetoableChangeListener(listener);
	}
	
    /**
     * Remove a VetoableChangeListener for a specific attribute.
     * If <code>listener</code> was added more than once to the same event
     * source for the specified attribute, it will be notified one less time
     * after being removed.
     * If <code>attributeName</code> is null, no exception is thrown and no
     * action is taken.
     * If <code>listener</code> is null, or was never added for the specified
     * property, no exception is thrown and no action is taken.
     *
     * @param attributeName  The name of the attribute that was listened on.
     * @param listener  The VetoableChangeListener to be removed
     */
	public void removeVetoableChangeListener(String attributeName, VetoableChangeListener listener) {
		vcs.removeVetoableChangeListener(attributeName,listener);
	}
	
    /**
     * Remove a VetoableChangeListener for a specific list of attributes.
     * If <code>listener</code> was added more than once to the same event
     * source for one of the specified attributes, it will be notified one less time
     * after being removed.
     * If <code>attributesNames</code> is null, or empty, no exception is thrown and no
     * action is taken.
     * If <code>listener</code> is null, or was never added for the specified
     * property, no exception is thrown and no action is taken.
     *
     * @param attributesNames  The names' list of the attributes that was listened on.
     * @param listener  The VetoableChangeListener to be removed
     */
	public void removeVetoableChangeListener(String[] attributesNames, VetoableChangeListener listener) {
		if (attributesNames != null) {
			for(String name:attributesNames) {
				vcs.removeVetoableChangeListener(name,listener);
			}
		}
	}
	
	/**
	 * Add a PropertyChangeListener to a specific attribute.
	 * 
	 * @param attributeName
	 * @param listener
	 */
	public void addAttributeChangeListener(String attributeName, PropertyChangeListener listener) {
		pcs.addPropertyChangeListener(attributeName, listener);
	}

	/**
	 * Add a PropertyChangeListener to a specific set of attributes.
	 * 
	 * @param attributesNames
	 * @param listener
	 */
	public void addAttributeChangeListener(String[] attributesNames, PropertyChangeListener listener) {
		if (attributesNames != null) {
			for(String name:attributesNames) {
				pcs.addPropertyChangeListener(name, listener);
			}
		}
	}

	/**
	 * Add a PropertyChangeListener registered to all attributes.
	 * @param listener
	 */
	public void addAttributeChangeListener(PropertyChangeListener listener) {
		pcs.addPropertyChangeListener(listener);
	}

	/**
	 * Remove a PropertyChangeListener registered to all attributes.
	 * @param listener
	 */
	public void removeAttributeChangeListener(PropertyChangeListener listener) {
		pcs.removePropertyChangeListener(listener);
	}
	
	/**
	 * Remove a PropertyChangeListener resgistered to a specific attribute.
	 * 
	 * @param attributeName
	 * @param listener
	 */
	public void removeAttributeChangeListener(String attributeName, PropertyChangeListener listener) {
		pcs.removePropertyChangeListener(attributeName,listener);
	}
	
	/**
	 * Remove a PropertyChangeListener registered to a specific set of attributes.
	 * @param attributesNames
	 * @param listener
	 */
	public void removeAttributeChangeListener(String[] attributesNames, PropertyChangeListener listener) {
		if (attributesNames != null) {
			for(String name:attributesNames) {
				pcs.removePropertyChangeListener(name, listener);
			}
		}
	}

	public Set<Entry<String, Object>> entrySet() {
		return bm.entrySet();
	}

	public Object get(String key) {
		return bm.get(key);
	}

	public <T> T get(String key, Class<T> clazz) {
		return bm.get(key, clazz);
	}

	public int getInt(String key) {
		return bm.getInt(key);
	}

	public float getFloat(String key) {
		return bm.getFloat(key);
	}

	public String getString(String key) {
		return bm.getString(key);
	}

	public boolean isEmpty() {
		return bm.isEmpty();
	}

	public Set<String> keys() {
		return bm.keys();
	}

	public Object put(String key, Object value) {
		Object oldValue = bm.get(key);
		try {
			vcs.fireVetoableChange(key, oldValue, value);
		} catch (PropertyVetoException e) {
			return oldValue;
		}
		oldValue = bm.put(key, value);
		pcs.firePropertyChange(key, oldValue, value);
		return oldValue;
	}

	/**
	 * Insert into this bean all the values contained into another one except its Id and type.
	 * @param bean
	 */
	public IBeanMap addAll(IBeanMap bean) {
		if (bean  != null) {
			for (Entry<String, Object> entry: bean.entrySet()) {
				// old value.
				Object ov = get(entry.getKey());
				try {
					vcs.fireVetoableChange(entry.getKey(), ov, entry.getValue());
					if ((ov instanceof IBeanMap) && (entry.getValue() instanceof IBeanMap)) {
						// try to clone the old BeanMap...
						Object cv = ov;
						if (ov instanceof BeanMap) {
							cv = ((BeanMap)ov).clone();
						}
						((IBeanMap)ov).addAll(((IBeanMap)entry.getValue()));
						ov = cv;
					} else {
						put(entry.getKey(), entry.getValue());
					}
					pcs.firePropertyChange(entry.getKey(), ov, entry.getValue());
				} catch (PropertyVetoException e) {} //Nothing to do here.
			}
		}
		return this;
	}

	public int size() {
		return bm.size();
	}

	public int getId() {
		return bm.getId();
	}

	public boolean equalsType(ITypedBean bm) {
		return this.bm.equalsType(bm);
	}

	public String getType() {
		return bm.getType();
	}

	public Date getDate() {
		return bm.getDate();
	}

	public boolean moreRecent(IDatedBean bm) {
		return this.bm.moreRecent(bm);
	}

	public void setDate(Date date) {
		bm.setDate(date);
	}

	@Override
	protected Object clone() throws CloneNotSupportedException {
		return new BeanMapObservable((BeanMap)bm.clone());
	}

	@Override
	public boolean equals(Object obj) {
		return bm.equals(obj);
	}

	@Override
	public String toString() {
		return bm.toString();
	}

	public boolean containsKey(Object key) {
		return bm.containsKey(key);
	}

	public boolean containsValue(Object value) {
		return bm.containsValue(value);
	}

	public Object get(Object key) {
		return bm.get(key);
	}

	public Object remove(Object key) {
		Object oldValue = bm.get(key);
		try {
			vcs.fireVetoableChange(key.toString(), oldValue, (Object)null);
		} catch (PropertyVetoException e) {
			return oldValue;
		}
		oldValue = bm.remove(key);
		pcs.firePropertyChange(key.toString(), oldValue, (Object)null);
		return oldValue;
	}

	public void putAll(Map<? extends String, ? extends Object> m) {
		for(Entry<? extends String, ? extends Object> e:m.entrySet()) {
			Object oldValue = bm.get(e.getKey());
			try {
				vcs.fireVetoableChange(e.getKey(), oldValue, e.getValue());
			} catch (PropertyVetoException ee) {
				continue;
			}
			oldValue = bm.put(e.getKey(), e.getValue());
			pcs.firePropertyChange(e.getKey(), oldValue, e.getValue());
		}
	}

	public void clear() {
		Iterator<Entry<String, Object>> i = bm.entrySet().iterator();
		while(i.hasNext()) {
			Entry<String, Object> e = i.next();
			try {
				vcs.fireVetoableChange(e.getKey(), e.getValue(), (Object)null);
			} catch (PropertyVetoException ee) {
				continue;
			}
			i.remove();
			pcs.firePropertyChange(e.getKey(), e.getValue(), (Object)null);
		}
	}

	public Set<String> keySet() {
		return bm.keySet();
	}

	public Collection<Object> values() {
		return bm.values();
	}

	@Override
	public int hashCode() {
		//Linked to the override of the Equals method
		return super.hashCode();
	}
}
