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

import java.util.Calendar;
import java.util.Date;

import org.eclipse.core.databinding.observable.Diffs;
import org.eclipse.core.databinding.observable.value.AbstractObservableValue;
import org.eclipse.jface.databinding.swt.DisplayRealm;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Widget;

/**
 * Observable inspired from :
 * 
 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=169876
 */
public class DateTimeObservableValue extends AbstractObservableValue<Date> {

	private final DateTime dateTime;
	private Calendar calendar;
	boolean updating = false;
	Date currentSelection;

	/**
	 * Standard constructor for an SWT ObservableValue. Makes sure that the observable gets disposed when the SWT widget
	 * is disposed.
	 * 
	 * @param widget
	 */
	public DateTimeObservableValue(DateTime widget) {
		super(DisplayRealm.getRealm(widget.getDisplay()));
		dateTime = widget;
		widget.addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent e) {
				DateTimeObservableValue.this.dispose();
			}
		});
		calendar = Calendar.getInstance();
		currentSelection = getSelection();
		dateTime.addSelectionListener(new SelectionAdapter() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (!updating) {
					Date newSelection = getSelection();
					fireValueChange(Diffs.createValueDiff(currentSelection, newSelection));
					currentSelection = newSelection;
				}
			}
		});
	}

	@Override
	public void doSetValue(final Date value) {
		Date oldValue;
		Date newValue;
		try {
			updating = true;
			newValue = (Date) value;
			oldValue = getSelection();
			setSelection(newValue);
			currentSelection = newValue;
			fireValueChange(Diffs.createValueDiff(oldValue, newValue));
		} finally {
			updating = false;
		}
	}

	@Override
	public Date doGetValue() {
		return getSelection();
	}

	public Object getValueType() {
		return Date.class;
	}

	Date getSelection() {
		// calendar.clear();
		// Control 1 (If conflict, Control 1 overrides Control 2)
		int style = dateTime.getStyle();
		if ((style & SWT.CALENDAR) != 0 || (style & SWT.DATE) != 0) {
			// Calendar or Date
			calendar.set(dateTime.getYear(), dateTime.getMonth(), dateTime.getDay());
		} else {
			// Time
			calendar.set(Calendar.HOUR_OF_DAY, dateTime.getHours());
			calendar.set(Calendar.MINUTE, dateTime.getMinutes());
			calendar.set(Calendar.SECOND, dateTime.getSeconds());
		}
		return calendar.getTime();
	}

	private void setSelection(Date value) {
		calendar.setTime(value);
		// Control 1
		int style = dateTime.getStyle();
		if ((style & SWT.CALENDAR) != 0 || (style & SWT.DATE) != 0) {
			// Calendar or Date
			dateTime.setYear(calendar.get(Calendar.YEAR));
			dateTime.setMonth(calendar.get(Calendar.MONTH));
			dateTime.setDay(calendar.get(Calendar.DAY_OF_MONTH));
		} else {
			// Time
			dateTime.setHours(calendar.get(Calendar.HOUR_OF_DAY));
			dateTime.setMinutes(calendar.get(Calendar.MINUTE));
			dateTime.setSeconds(calendar.get(Calendar.SECOND));
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.databinding.swt.ISWTObservable#getWidget()
	 */
	public Widget getWidget() {
		return dateTime;
	}

}
