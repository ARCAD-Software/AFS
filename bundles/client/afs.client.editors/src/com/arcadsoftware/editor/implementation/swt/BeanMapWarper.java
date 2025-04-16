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
 * <ul>
 * <li>The original one as given to the <code>warp</code> method.
 * <li>The current one (the warper can be used like a BeanMap).
 * <li>The diff between the current and the original one.
 * </ul>
 */
public class BeanMapWarper implements ISelection, IBeanMap, IIdentifiedBean, ITypedBean, IDatedBean {

	private BeanMap source;
	private BeanMap current;
	private BeanMap changed;
	private final SWTRenderer renderer;
	private boolean dirty;

	private final ListenerList<ISelectionChangedListener> selListeners = new ListenerList<>();

	public BeanMapWarper(SWTRenderer renderer) {
		super();
		this.renderer = renderer;
		purge();
	}

	private void purge() {
		dirty = false;
		if (source != null) {
			changed = new BeanMap(source.getType(), source.getId());
			current = source.clone();
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
		for (final Object listener : selListeners.getListeners()) {
			final ISelectionChangedListener l = (ISelectionChangedListener) listener;
			SafeRunnable.run(new SafeRunnable() {
				@Override
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

	@Override
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

	@Override
	public Set<Entry<String, Object>> entrySet() {
		return current.entrySet();
	}

	@Override
	public Object get(String key) {
		return current.get(key);
	}

	@Override
	public <T> T get(String key, Class<T> clazz) {
		return current.get(key, clazz);
	}

	@Override
	public int getInt(String key) {
		return current.getInt(key);
	}

	@Override
	public String getString(String key) {
		return current.getString(key);
	}

	@Override
	public Set<String> keys() {
		return current.keys();
	}

	@Override
	public Object put(String key, Object value) {
		// Just ignore non declared attributes.
		final MetaDataAttribute att = renderer.getStructure().getAttribute(key);
		if (att == null) {
			return null;
		}
		final Object oldValue = current.get(key);
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
				final Object sourceValue = source.get(key);
				if (value == null) {
					changed.put(key, null);
					// We can not force "null" value !
				} else if (value.equals(sourceValue)) {
					// Cancel change.
					// Ceci est une grosse erreur !!!!!!
					// ->changed.put(key, null);
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

	@Override
	public int size() {
		return current.size();
	}

	@Override
	public int getId() {
		return current.getId();
	}

	@Override
	public boolean equalsType(ITypedBean bm) {
		return current.equalsType(bm);
	}

	@Override
	public String getType() {
		if (renderer.getStructure() != null) {
			return renderer.getStructure().getType();
		}
		return null;
	}

	@Override
	public Date getDate() {
		if (source != null) {
			return source.getDate();
		}
		return current.getDate();
	}

	@Override
	public boolean moreRecent(IDatedBean bm) {
		if (source != null) {
			return source.moreRecent(bm);
		}
		return current.moreRecent(bm);
	}

	@Override
	public void setDate(Date date) {
		current.setDate(date);
	}

	/**
	 * @return the modifications Attributes between the source and the current version of this BeanMap.
	 */
	public BeanMap getModifications() {
		final BeanMap clone = changed.clone();
		// Add the mandatory attributes
		for (final MetaDataAttribute a : renderer.getStructure().getAttributes().values()) {
			if (a.isMandatory() && (clone.get(a.getCode()) == null)) {
				clone.put(a.getCode(), current.get(a.getCode()));
			}
		}
		// Clean up the clone...
		for (final Entry<String, Object> e : clone.entrySet()) {
			if (e.getValue() instanceof BeanMap) {
				final int id = ((BeanMap) e.getValue()).getId();
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
		return current.clone();
	}

	public void updateCurrent(BeanMap item) {
		if (source == null) {
			source = item.clone();
			for (final Entry<String, Object> entry : item.entrySet()) {
				current.put(entry.getKey(), entry.getValue());
				if (entry.getValue().equals(changed.get(entry.getKey()))) {
					changed.put(entry.getKey(), null);
				}
			}
			return;
		}
		for (final Entry<String, Object> entry : item.entrySet()) {
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
		for (final Entry<String, Object> srcEntry : source.entrySet()) {
			if (srcEntry.getValue() != null) {
				if (item.get(srcEntry.getKey()) == null) {
					if (keysToRemove == null) {
						keysToRemove = new ArrayList<>();
					}
					keysToRemove.add(srcEntry.getKey());
				}
			}
		}
		if (keysToRemove != null) {
			// La suppression est faite a posteriori pour �viter les acc�s
			// concurrentiels
			for (final String key : keysToRemove) {
				source.keys().remove(key);
				current.keys().remove(key);
				// if (changed.get(srcEntry.getKey()) == null) {
				// changed.put(entry.getKey(), null);
				// }
			}
		}
	}

	@Override
	public float getFloat(String key) {
		return current.getFloat(key);
	}

	@Override
	public IBeanMap addAll(IBeanMap bean) {
		for (final Entry<String, Object> e : bean.entrySet()) {
			put(e.getKey(), e.getValue());
		}
		return this;
	}

	public void forceDirty() {
		dirty = true;
	}

	@Override
	public int getMUID() {
		return current.getMUID();
	}

	@Override
	public void setMUID(int id) {
		current.setMUID(id);
	}

	@Override
	public void setModification(int uid, Date date) {
		current.setModification(uid, date);
	}
}
