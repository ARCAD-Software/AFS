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

import org.eclipse.core.databinding.observable.Diffs;
import org.eclipse.core.databinding.observable.value.AbstractObservableValue;
import org.eclipse.jface.databinding.swt.DisplayRealm;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.swt.IBeanMapContainerValue;

public class ContainerObservableValue extends AbstractObservableValue<BeanMap> {

	private IBeanMapContainerValue widget;
	boolean updating = false;
	BeanMap currentValue;

	/**
	 * @param widget
	 */
	public ContainerObservableValue(IBeanMapContainerValue widget) {
		super(DisplayRealm.getRealm(widget.getWidget().getDisplay()));
		this.widget = widget;
		this.widget.addSelectionListener(new SelectionAdapter() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (!updating) {
					BeanMap newSelection = (BeanMap) ContainerObservableValue.this.doGetValue();
					BeanMap oldSelection = currentValue;
					currentValue = newSelection;
					fireValueChange(Diffs.createValueDiff(oldSelection, newSelection));
				}
			}
		});
		widget.getWidget().addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent e) {
				ContainerObservableValue.this.dispose();
			}
		});

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.databinding.observable.value.AbstractObservableValue#doGetValue()
	 */
	@Override
	protected BeanMap doGetValue() {
		return widget.getBeanMapValue();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.databinding.observable.value.AbstractObservableValue#doSetValue(java.lang.Object)
	 */
	@Override
	public void doSetValue(final BeanMap value) {
		BeanMap oldValue;
		BeanMap newValue;
		try {
			updating = true;
			newValue = (BeanMap) value;
			oldValue = widget.getBeanMapValue();
			widget.setBeanMapValue(newValue);
			currentValue = newValue;
			fireValueChange(Diffs.createValueDiff(oldValue, newValue));
		} finally {
			updating = false;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.databinding.swt.ISWTObservable#getWidget()
	 */
	public Widget getWidget() {
		return widget.getWidget();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.databinding.observable.value.IObservableValue#getValueType()
	 */
	public Object getValueType() {
		return BeanMap.class;
	}

}
