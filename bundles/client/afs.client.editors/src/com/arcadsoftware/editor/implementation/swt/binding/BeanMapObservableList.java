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
package com.arcadsoftware.editor.implementation.swt.binding;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.eclipse.core.databinding.observable.list.ObservableList;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.editor.implementation.swt.IListContainer;
import com.arcadsoftware.editor.implementation.swt.renderer.SWTRenderer;

/**
 * Binding read-only !!!
 */
public class BeanMapObservableList extends ObservableList implements IListContainer {

	private final String type;
	private boolean populated;
	private boolean autoPopulate;
	private final SWTRenderer renderer;

	/**
	 *
	 */
	@SuppressWarnings("unchecked")
	public BeanMapObservableList(SWTRenderer renderer, String type, boolean autoPopulate) {
		super(new ArrayList(), BeanMap.class);
		this.type = type;
		this.autoPopulate = autoPopulate;
		this.renderer = renderer;
	}

	public String getType() {
		return type;
	}

	public boolean isPopulated() {
		return populated;
	}

	public boolean isAutoPopulate() {
		return autoPopulate;
	}

	public void setAutoPopulate(boolean autoPopulate) {
		this.autoPopulate = autoPopulate;
	}

	public void populate() {
		populated = true;
		// Run a delayed populate...
		renderer.getDataLoader().loadList(type, new BindingListLoadRunnable(getRealm(), this));
	}

	public void reset() {
		if (populated) {
			populate();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.editor.internal.swt.IListContainer#load(com.arcadsoftware.utils.BeanMapList)
	 */
	@Override
	public void load(BeanMapList list) {
		updateWrappedList(list);
		if ((list != null) && (list.size() > 0)) {
			renderer.loadListCompleted(list.get(0).getType());
		}
	}

	private void autoPop() {
		if ((!populated) && autoPopulate) {
			populate();
		}
	}

	@Override
	public boolean contains(Object o) {
		autoPop();
		return super.contains(o);
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean containsAll(Collection c) {
		autoPop();
		return super.containsAll(c);
	}

	@Override
	public Object get(int index) {
		autoPop();
		return super.get(index);
	}

	@Override
	public int indexOf(Object o) {
		autoPop();
		return super.indexOf(o);
	}

	@Override
	public boolean isEmpty() {
		autoPop();
		return super.isEmpty();
	}

	@SuppressWarnings("unchecked")
	@Override
	public Iterator iterator() {
		autoPop();
		return super.iterator();
	}

	@Override
	public int lastIndexOf(Object o) {
		autoPop();
		return super.lastIndexOf(o);
	}

	@SuppressWarnings("unchecked")
	@Override
	public ListIterator listIterator() {
		autoPop();
		return super.listIterator();
	}

	@SuppressWarnings("unchecked")
	@Override
	public ListIterator listIterator(int index) {
		autoPop();
		return super.listIterator(index);
	}

	@Override
	public int size() {
		autoPop();
		return super.size();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List subList(int fromIndex, int toIndex) {
		autoPop();
		return super.subList(fromIndex, toIndex);
	}

	@Override
	public Object[] toArray() {
		autoPop();
		return super.toArray();
	}

	@Override
	public Object[] toArray(Object[] a) {
		autoPop();
		return super.toArray(a);
	}

	@Override
	public String toString() {
		autoPop();
		return super.toString();
	}

	/**
	 * @param item
	 */
	public void updateItem(BeanMap item) {
		for (int i = wrappedList.size() - 1; i >= 0; i--) {
			if (item.getId() == ((BeanMap) wrappedList.get(i)).getId()) {
				set(i, item);
				return;
			}
		}
	}

}
