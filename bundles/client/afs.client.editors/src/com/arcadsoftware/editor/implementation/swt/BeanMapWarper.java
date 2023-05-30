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
package com.arcadsoftware.editor.implementation.swt;

import java.util.ArrayList;
import java.util.Date;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.IBeanMap;
import com.arcadsoftware.beanmap.IDatedBean;
import com.arcadsoftware.beanmap.IIdentifiedBean;
import com.arcadsoftware.beanmap.ITypedBean;
import com.arcadsoftware.editor.implementation.swt.renderer.SWTRenderer;
import com.arcadsoftware.metadata.MetaDataAttribute;

/**
 * This warper maintain 3 version of asame BeanMap :
 * 
 * <ul>
 * <li>The original one as given to the <code>warp</code> method.
 * <li>The current one (the warper can be used like a BeanMap).
 * <li>The diff between the current and the original one.
 * </ul>
 * 
 */
public class BeanMapWarper implements ISelection, IBeanMap, IIdentifiedBean, ITypedBean, IDatedBean {

	private BeanMap source;
	private BeanMap current;
	private BeanMap changed;
	private SWTRenderer renderer;
	private boolean dirty;

	private ListenerList selListeners = new ListenerList();

	public BeanMapWarper(SWTRenderer renderer) {
		super();
		this.renderer = renderer;
		purge();
	}

	private void purge() {
		dirty = false;
		if (source != null) {
			changed = new BeanMap(source.getType(), source.getId());
			current = (BeanMap) source.clone();
		} else if (renderer.getStructure() != null) {
			changed = new BeanMap(renderer.getStructure().getType());
			current = new BeanMap(renderer.getStructure().getType());
		} else {
			changed = new BeanMap();
			current = new BeanMap();
		}
	}

	private void fireChangedSelectionEvent() {
		final SelectionChangedEvent event = new SelectionChangedEvent(renderer, this);
		Object[] listeners = selListeners.getListeners();
		for (int i = 0; i < listeners.length; ++i) {
			final ISelectionChangedListener l = (ISelectionChangedListener) listeners[i];
			SafeRunnable.run(new SafeRunnable() {
				public void run() {
					l.selectionChanged(event);
				}
			});
		}
	}

	public void warp(IBeanMap beanMap) {
		source = new BeanMap(beanMap);
		purge();
		fireChangedSelectionEvent();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.viewers.ISelection#isEmpty()
	 */
	public boolean isEmpty() {
		return source == null;
	}

	/**
	 * @param listener
	 */
	public void removeSelectionChangedListener(ISelectionChangedListener listener) {
		selListeners.remove(listener);
	}

	/**
	 * @param listener
	 */
	public void addSelectionChangedListener(ISelectionChangedListener listener) {
		selListeners.add(listener);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.utils.IBeanMap#entrySet()
	 */
	public Set<Entry<String, Object>> entrySet() {
		return current.entrySet();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.utils.IBeanMap#get(java.lang.String)
	 */
	public Object get(String key) {
		return current.get(key);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.utils.IBeanMap#get(java.lang.String, java.lang.Class)
	 */
	public <T> T get(String key, Class<T> clazz) {
		return current.get(key, clazz);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.utils.IBeanMap#getInt(java.lang.String)
	 */
	public int getInt(String key) {
		return current.getInt(key);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.utils.IBeanMap#getString(java.lang.String)
	 */
	public String getString(String key) {
		return current.getString(key);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.utils.IBeanMap#keys()
	 */
	public Set<String> keys() {
		return current.keys();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.utils.IBeanMap#put(java.lang.String, java.lang.Object)
	 */
	public Object put(String key, Object value) {
		// Just ignore non declared attributes.
		MetaDataAttribute att = renderer.getStructure().getAttribute(key);
		if (att == null) {
			return null;
		}
		Object oldValue = current.get(key);
		// No change...
		if (value == null) {
			if (oldValue == null) {
				return null;
			}
		} else if (value.equals(oldValue)) {
			return value;
		}
		if (!att.isReadonly()) {
			// If we are changing an already changed value, and get back to the original value.
			if (source != null) {
				Object sourceValue = source.get(key);
				if (value == null) {
					changed.put(key, null);
					// We can not force "null" value !
				} else if (value.equals(sourceValue)) {
					// Cancel change.
					//Ceci est une grosse erreur !!!!!!
					//->changed.put(key, null);
					changed.remove(key);
					
				} else {
					changed.put(key, value);
				}
			} else {
				// We are creating a new item.
				changed.put(key, value);
			}
		}
		current.put(key, value);
		fireChangedSelectionEvent();
		return oldValue;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.utils.IBeanMap#size()
	 */
	public int size() {
		return current.size();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.utils.IIdentifiedBean#getId()
	 */
	public int getId() {
		return current.getId();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.utils.ITypedBean#equalsType(com.arcadsoftware.utils.ITypedBean)
	 */
	public boolean equalsType(ITypedBean bm) {
		return current.equalsType(bm);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.utils.ITypedBean#getType()
	 */
	public String getType() {
		if (renderer.getStructure() != null) {
			return renderer.getStructure().getType();
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.utils.IDatedBean#getDate()
	 */
	public Date getDate() {
		if (source != null) {
			return source.getDate();
		}
		return current.getDate();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.utils.IDatedBean#moreRecent(com.arcadsoftware.utils.IDatedBean)
	 */
	public boolean moreRecent(IDatedBean bm) {
		if (source != null) {
			return source.moreRecent(bm);
		}
		return current.moreRecent(bm);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.arcadsoftware.utils.IDatedBean#setDate(java.util.Date)
	 */
	public void setDate(Date date) {
		current.setDate(date);
	}

	/**
	 * @return the modifications Attributes between the source and the current version of this BeanMap.
	 */
	public BeanMap getModifications() {
		BeanMap clone = (BeanMap) changed.clone();
		// Add the mandatory attributes
		for (MetaDataAttribute a : renderer.getStructure().getAttributes().values()) {
			if (a.isMandatory() && (clone.get(a.getCode()) == null)) {
				clone.put(a.getCode(), current.get(a.getCode()));
			}
		}
		// Clean up the clone...
		for (Entry<String, Object> e : clone.entrySet()) {
			if (e.getValue() instanceof BeanMap) {
				int id = ((BeanMap) e.getValue()).getId();
				if (id <= 0) {
					clone.put(e.getKey(), null);
				}
			}
		}
		return clone;
	}

	public boolean isDirty() {
		return dirty || !changed.isEmpty();
	}

	public BeanMap cloneCurrent() {
		return (BeanMap) current.clone();
	}

	public void updateCurrent(BeanMap item) {
		if (source == null) {
			source = (BeanMap) item.clone();
			for (Entry<String, Object> entry : item.entrySet()) {
				current.put(entry.getKey(), entry.getValue());
				if (entry.getValue().equals(changed.get(entry.getKey()))) {
					changed.put(entry.getKey(), null);
				}
			}
			return;
		}
		for (Entry<String, Object> entry : item.entrySet()) {
			if (entry.getValue() == null) {
				source.put(entry.getKey(), null);
				current.put(entry.getKey(), null);
				if (changed.get(entry.getKey()) == null) {
					changed.put(entry.getKey(), null);
				}
			} else if (!entry.getValue().equals(source.get(entry.getKey()))) {
				source.put(entry.getKey(), entry.getValue());
				current.put(entry.getKey(), entry.getValue());
				if (entry.getValue().equals(changed.get(entry.getKey()))) {
					changed.put(entry.getKey(), null);
				}
			}
		}
		ArrayList<String> keysToRemove = null;
		for (Entry<String, Object> srcEntry : source.entrySet()) {
			if (srcEntry.getValue() != null) {
				if (item.get(srcEntry.getKey()) == null) {
					if (keysToRemove == null)
						keysToRemove = new ArrayList<String>();
					keysToRemove.add(srcEntry.getKey());
				}
			}
		}
		if (keysToRemove != null)
			// La suppression est faite a posteriori pour �viter les acc�s
			// concurrentiels
			for (String key : keysToRemove) {
				source.keys().remove(key);
				current.keys().remove(key);
				// if (changed.get(srcEntry.getKey()) == null) {
				// changed.put(entry.getKey(), null);
				// }
			}
	}


	public float getFloat(String key) {
		return current.getFloat(key);
	}


	public IBeanMap addAll(IBeanMap bean) {
		for(Entry<String, Object> e:bean.entrySet()) {
			  put(e.getKey(),e.getValue());
			 }
		return this;
	}

	public void forceDirty() {
		dirty = true;
	}
}
