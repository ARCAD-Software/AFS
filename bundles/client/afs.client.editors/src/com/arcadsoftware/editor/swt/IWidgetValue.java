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
package com.arcadsoftware.editor.swt;

import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Control;

/**
 * Used to create a custom binding on a widget
 */
public interface IWidgetValue {

	/**
	 * @return the widget.
	 */
	public Control getWidget();

	/**
	 * @return the widget value.
	 */
	public Object getValue();

	/**
	 * Used to add a selectionListener on the widget.
	 * 
	 * @param SelectionListener
	 *            the selection listener.
	 */
	public void addSelectionListener(SelectionListener selectionListener);

	/**
	 * Sets the widget value.
	 * 
	 * @param newValue
	 *            the new widget value.
	 */
	public void setValue(Object newValue);

	/**
	 * @return the widget value type.
	 */
	public Object getValueType();

}
