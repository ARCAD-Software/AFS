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
package com.arcadsoftware.client.editors.swtwidgets.widgets;

import java.util.Calendar;
import java.util.Date;

import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.editor.swt.IWidgetValue;

/**
 * Defines a custom dateTime with its binding.
 */
public class CustomDateTime implements IWidgetValue {

	DateTime dateTime;

	Calendar calendar = Calendar.getInstance();

	/**
	 * Constructs a new instance of DateTime given its parent and a style value describing its behavior and appearance.
	 * <p>
	 * The style value is either one of the style constants defined in class <code>SWT</code> which is applicable to
	 * instances of this class, or must be built by <em>bitwise OR</em>'ing together (that is, using the
	 * <code>int</code> "|" operator) two or more of those <code>SWT</code> style constants. The class description lists
	 * the style constants that are applicable to the class. Style bits are also inherited from superclasses.
	 * </p>
	 * 
	 * @param parent
	 *            a composite control which will be the parent of the new instance (cannot be null)
	 * @param style
	 *            the style of control to construct
	 * 
	 * @exception IllegalArgumentException
	 *                <ul>
	 *                <li>ERROR_NULL_ARGUMENT - if the parent is null</li>
	 *                </ul>
	 * @exception SWTException
	 *                <ul>
	 *                <li>ERROR_THREAD_INVALID_ACCESS - if not called from the thread that created the parent</li>
	 *                <li>ERROR_INVALID_SUBCLASS - if this class is not an allowed subclass</li>
	 *                </ul>
	 * 
	 * @see SWT#DATE
	 * @see SWT#TIME
	 * @see SWT#CALENDAR
	 * @see Widget#checkSubclass
	 * @see Widget#getStyle
	 */
	public CustomDateTime(Composite parent, int style) {
		dateTime = new DateTime(parent, style);
	}

	public void addSelectionListener(SelectionListener selectionListener) {
		dateTime.addSelectionListener(selectionListener);
	}

	public Object getValue() {
		int style = dateTime.getStyle();
		if ((style & SWT.CALENDAR) != 0 || (style & SWT.DATE) != 0) {
			// Calendar or Date
			calendar.set(dateTime.getYear(), dateTime.getMonth(), dateTime.getDay(), 0, 0, 0);
		} else {
			// Time
			calendar.set(Calendar.HOUR_OF_DAY, dateTime.getHours());
			calendar.set(Calendar.MINUTE, dateTime.getMinutes());
			calendar.set(Calendar.SECOND, dateTime.getSeconds());
		}
		return calendar.getTime();
	}

	public Object getValueType() {
		return Date.class;
	}

	public Control getWidget() {
		return dateTime;
	}

	public void setValue(Object newValue) {
		if (newValue != null) {
			calendar.setTime((Date) newValue);
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
			dateTime.notifyListeners(SWT.Selection, new Event());
		}
	}

	/**
	 * @return the dateTime widget
	 */
	public DateTime getDateTime() {
		return dateTime;
	}

}
