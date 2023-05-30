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

import org.eclipse.core.databinding.observable.Diffs;
import org.eclipse.core.databinding.observable.list.ObservableList;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.observable.value.IValueChangeListener;
import org.eclipse.core.databinding.observable.value.ValueChangeEvent;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.editor.implementation.swt.IListContainer;
import com.arcadsoftware.editor.implementation.swt.renderer.SWTRenderer;
import com.arcadsoftware.metadata.MetaDataAttribute;

/**
 *
 */
public class BeanMapObservableLinkedList extends ObservableList implements IListContainer, IObservableValue {

	SWTRenderer renderer;
	MetaDataAttribute sourceAttribute;
	MetaDataAttribute attribute;
	String linkCode;
	BeanMap currentRef = null;
	FakeValueChangeEvent fake;

	// Needed to gain access to the protected method "getElementType".
	private final class FakeValueChangeEvent extends ValueChangeEvent {
		private static final long serialVersionUID = 1L;

		public FakeValueChangeEvent(IObservableValue o) {
			super(o, null);
		}

		public Object naze() {
			return getElementType();
		}
	}

	/**
	 * @param renderer
	 * @param attribute
	 * @param linkCode
	 * @param autoPopulate
	 */
	@SuppressWarnings("unchecked")
	public BeanMapObservableLinkedList(SWTRenderer renderer, MetaDataAttribute attribute,
			MetaDataAttribute sourceAttribute, String linkCode) {
		super(new ArrayList(), BeanMap.class);
		fake = new FakeValueChangeEvent(this);
		this.renderer = renderer;
		this.sourceAttribute = sourceAttribute;
		this.attribute = attribute;
		this.linkCode = linkCode;
	}

	public String getType() {
		return attribute.getType();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @seeorg.eclipse.core.databinding.observable.value.IObservableValue#
	 * addValueChangeListener
	 * (org.eclipse.core.databinding.observable.value.IValueChangeListener)
	 */
	public void addValueChangeListener(IValueChangeListener listener) {
		addListener(fake.naze(), listener);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.core.databinding.observable.value.IObservableValue#getValue()
	 */
	public Object getValue() {
		getterCalled();
		return currentRef;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.core.databinding.observable.value.IObservableValue#getValueType
	 * ()
	 */
	public Object getValueType() {
		return BeanMap.class;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @seeorg.eclipse.core.databinding.observable.value.IObservableValue#
	 * removeValueChangeListener
	 * (org.eclipse.core.databinding.observable.value.IValueChangeListener)
	 */
	public void removeValueChangeListener(IValueChangeListener listener) {
		removeListener(fake.naze(), listener);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.core.databinding.observable.value.IObservableValue#setValue
	 * (java.lang.Object)
	 */
	public void setValue(Object value) {
		checkRealm();
		if (value == null) {
			updateWrappedList(new BeanMapList());
			fireEvent(new ValueChangeEvent(this, Diffs.createValueDiff(currentRef, null)));
			currentRef = null;
			return;
		}
		BeanMap beanMap = (BeanMap) value;
		int id = beanMap.getId();
		// Blindage !
		if (id <= 0) {
			updateWrappedList(new BeanMapList());
			fireEvent(new ValueChangeEvent(this, Diffs.createValueDiff(currentRef, null)));
			currentRef = null;
			return;
		}
		// Run a delayed populate...
		int sourceId = renderer.getCurrentBean().getInt(sourceAttribute.getCode());
		if (sourceId > 0)
			renderer.getDataLoader().loadSubList(sourceAttribute.getType(), sourceId, linkCode, attribute.getType(),
					new BindingListLoadRunnable(getRealm(), this));
		// Fire a value change event !
		fireEvent(new ValueChangeEvent(this, Diffs.createValueDiff(currentRef, beanMap)));
		currentRef = beanMap;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.arcadsoftware.editor.internal.swt.IListContainer#load(com.arcadsoftware
	 * .utils.BeanMapList)
	 */
	public void load(BeanMapList list) {
		// Delayed list update !
		updateWrappedList(list);
	}

	/**
	 * @param item
	 */
	@SuppressWarnings("unchecked")
	public void updateItem(BeanMap item) {
		for (int i = wrappedList.size() - 1; i >= 0; i--) {
			if (item.getId() == ((BeanMap) wrappedList.get(i)).getId()) {
				checkRealm();
				Object oldElement = wrappedList.set(i, item);
				if (oldElement instanceof BeanMap){
					BeanMap oldBm = (BeanMap) oldElement;					
					fireListChange(Diffs.createListDiff(Diffs.createListDiffEntry(i, false, oldBm), 
							   Diffs.createListDiffEntry(i, true, item)));
				}
				return;
			}
		}
	}

}
