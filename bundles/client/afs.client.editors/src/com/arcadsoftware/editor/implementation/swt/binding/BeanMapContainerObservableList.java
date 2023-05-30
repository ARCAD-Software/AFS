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
package com.arcadsoftware.editor.implementation.swt.binding;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.databinding.BindingException;
import org.eclipse.core.databinding.observable.Diffs;
import org.eclipse.core.databinding.observable.ObservableTracker;
import org.eclipse.core.databinding.observable.list.AbstractObservableList;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.editor.swt.IBeanMapContainerList;

/**
 * BeanMap list Container observable (used for binding).
 */
public class BeanMapContainerObservableList extends AbstractObservableList<BeanMap> {

	private IBeanMapContainerList widget;

	public BeanMapContainerObservableList(IBeanMapContainerList widget) {
		super();
		this.widget = widget;
	}

	private void currentGetterCalled() {
		ObservableTracker.getterCalled(this);
	}

	@Override
	protected int doGetSize() {
		if (widget.getBeanMapList() == null) {
			return 0;
		}
		return widget.getBeanMapList().size();
	}

	@Override
	public BeanMap get(int index) {
		currentGetterCalled();
		return getItem(index);
	}

	public Object getElementType() {
		return BeanMap.class;
	}

	@Override
	public void add(int index, BeanMap element) {
		widget.addBeanMapToList(index, (BeanMap) element);
		fireListChange(Diffs.createListDiff(Diffs.createListDiffEntry(index, true, element)));
	}

	@Override
	public BeanMap remove(int index) {
		currentGetterCalled();
		int size = doGetSize();
		if (index < 0 || index > size - 1) {
			throw new BindingException("Request to remove an element out of the collection bounds"); //$NON-NLS-1$
		}
		BeanMap[] newItems = new BeanMap[size - 1];
		BeanMap oldElement = getItem(index);
		if (newItems.length > 0) {
			System.arraycopy(getItems(), 0, newItems, 0, index);
			if (size - 1 > index) {
				System.arraycopy(getItems(), index + 1, newItems, index, size - index - 1);
			}
		}
		setItems(newItems);
		fireListChange(Diffs.createListDiff(Diffs.createListDiffEntry(index, false, oldElement)));
		return oldElement;
	}

	@Override
	public BeanMap set(int index, BeanMap element) {
		BeanMap oldElement = getItem(index);
		setItem(index, (BeanMap) element);
				
		if (element instanceof BeanMap){
			BeanMap elmt = (BeanMap) element;					
			fireListChange(Diffs.createListDiff(Diffs.createListDiffEntry(index, false, oldElement), Diffs
					.createListDiffEntry(index, true, elmt)));
		}
		return oldElement;
	}

	@Override
	public BeanMap move(int oldIndex, int newIndex) {
		checkRealm();
		if (oldIndex == newIndex)
			return get(oldIndex);
		int size = doGetSize();
		if (oldIndex < 0 || oldIndex >= size) {
			throw new IndexOutOfBoundsException("oldIndex: " + oldIndex + ", size:" + size); //$NON-NLS-1$ //$NON-NLS-2$
		}
		if (newIndex < 0 || newIndex >= size) {
			throw new IndexOutOfBoundsException("newIndex: " + newIndex + ", size:" + size); //$NON-NLS-1$ //$NON-NLS-2$
		}

		BeanMap[] items = getItems();
		BeanMap[] newItems = new BeanMap[size];
		BeanMap element = items[oldIndex];
		if (newItems.length > 0) {
			System.arraycopy(items, 0, newItems, 0, size);
			if (oldIndex < newIndex) {
				System.arraycopy(items, oldIndex + 1, newItems, oldIndex, newIndex - oldIndex);
			} else {
				System.arraycopy(items, newIndex, newItems, newIndex + 1, oldIndex - newIndex);
			}
			newItems[newIndex] = element;
		}
		setItems(newItems);
		fireListChange(Diffs.createListDiff(Diffs.createListDiffEntry(oldIndex, false, element), Diffs
				.createListDiffEntry(newIndex, true, element)));
		return element;
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		checkRealm();
		List<BeanMap> oldItems = Arrays.asList(getItems());
		List<BeanMap> newItems = new ArrayList<BeanMap>(oldItems);
		boolean removedAll = newItems.removeAll(c);
		if (removedAll) {
			setItems((BeanMap[]) newItems.toArray(new BeanMap[newItems.size()]));
			fireListChange(Diffs.computeListDiff(oldItems, newItems));
		}
		return removedAll;
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		checkRealm();
		List<BeanMap> oldItems = Arrays.asList(getItems());
		List<BeanMap> newItems = new ArrayList<BeanMap>(oldItems);
		boolean retainedAll = newItems.retainAll(c);
		if (retainedAll) {
			setItems((BeanMap[]) newItems.toArray(new BeanMap[newItems.size()]));
			fireListChange(Diffs.computeListDiff(oldItems, newItems));
		}
		return retainedAll;
	}

	public IBeanMapContainerList getWidget() {
		return widget;
	}

	private void setItems(BeanMap[] items) {
		widget.setBeanMapList(new BeanMapList(items));
	}

	private void setItem(int index, BeanMap element) {
		BeanMapList list = widget.getBeanMapList();
		if (list == null) {
			list = new BeanMapList(element);
		} else {
			try {
				list.set(index, element);
			} catch (IndexOutOfBoundsException e) {
				list.add(element);
			}
		}
		widget.setBeanMapList(list);
	}

	private BeanMap[] getItems() {
		BeanMapList list = widget.getBeanMapList();
		if (list == null) {
			return new BeanMap[] {};
		}
		return list.toArray();
	}

	private BeanMap getItem(int index) {
		BeanMapList list = widget.getBeanMapList();
		if (list == null) {
			return null;
		}
		return list.get(index);
	}
}
