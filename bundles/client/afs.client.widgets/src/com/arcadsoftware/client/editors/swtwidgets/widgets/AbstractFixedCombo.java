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
package com.arcadsoftware.client.editors.swtwidgets.widgets;

import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.IWidgetValue;
import com.arcadsoftware.metadata.Element;

public abstract class AbstractFixedCombo implements IWidgetValue {
	private final Combo combo;

	public AbstractFixedCombo(Composite parent, int style, ILayoutParameters parameters, ISWTRenderer renderer,
			Element element, int horizontalSpan) {
		combo = new Combo(parent, style);
		if (parent.getLayout() instanceof GridLayout) {
			combo.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, horizontalSpan, 1));
		}
		fill(combo);
	}

	@Override
	public Control getWidget() {
		return combo;
	}

	@Override
	public Object getValue() {
		return indexToValue(combo.getSelectionIndex());
	}

	@Override
	public void addSelectionListener(SelectionListener SelectionListener) {
		combo.addSelectionListener(SelectionListener);
	}

	@Override
	public void setValue(Object newValue) {
		final int index = valueToIndex(newValue);
		combo.select(index);
	}

	@Override
	public Object getValueType() {
		return getType();
	}

	public abstract void fill(Combo combo);

	public abstract int valueToIndex(Object value);

	public abstract Object indexToValue(int index);

	public abstract Object getType();

}
