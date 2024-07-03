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
package com.arcadsoftware.editor.implementation.swt.binding;

import org.eclipse.core.databinding.observable.Diffs;
import org.eclipse.core.databinding.observable.value.AbstractObservableValue;
import org.eclipse.jface.databinding.swt.DisplayRealm;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.editor.swt.IWidgetValue;

/**
 * Creates a widget observable class for binding.
 */
public class WidgetObservableValue extends AbstractObservableValue<Object> {

	private final IWidgetValue widgetValue;
	private final Widget widget;
	Object currentObject;
	boolean updating = false;

	/**
	 * Standard constructor for an SWT ObservableValue. Makes sure that the observable gets disposed when the SWT widget
	 * is disposed.
	 *
	 * @param widget
	 */
	public WidgetObservableValue(IWidgetValue widgetValue) {
		super(DisplayRealm.getRealm(widgetValue.getWidget().getDisplay()));
		this.widgetValue = widgetValue;
		widget = widgetValue.getWidget();

		widget.addDisposeListener(new DisposeListener() {
			@Override
			public void widgetDisposed(DisposeEvent e) {
				WidgetObservableValue.this.dispose();
			}
		});
		currentObject = doGetValue();
		widgetValue.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (!updating) {
					final Object newObject = WidgetObservableValue.this.doGetValue();
					fireValueChange(currentObject, newObject);
					currentObject = newObject;
				}
			}
		});

	}

	protected void fireValueChange(Object oldValue, Object newValue) {
		fireValueChange(Diffs.createValueDiff(oldValue, newValue));
	}

	@Override
	protected Object doGetValue() {
		return widgetValue.getValue();
	}

	@Override
	public void doSetValue(final Object value) {
		Object oldValue;
		Object newValue;
		try {
			updating = true;
			newValue = value;
			oldValue = doGetValue();
			widgetValue.setValue(newValue);
			currentObject = newValue;
			fireValueChange(Diffs.createValueDiff(oldValue, newValue));
		} finally {
			updating = false;
		}
	}

	public Widget getWidget() {
		return widget;
	}

	@Override
	public Object getValueType() {
		return widgetValue.getValueType();
	}

}
