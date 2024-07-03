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
package com.arcadsoftware.client.editors.swtwidgets.inputs;

import java.util.List;

import org.eclipse.swt.widgets.Combo;

import com.arcadsoftware.editor.ElementParameter;

/**
 * This class implement a Combo SWT Widget provider for the dynamic editors.
 */
public class FixedValueComboSWTProvider extends AbstractFixedValueComboSWTProvider {

	protected static final String ITEM = "item";
	protected static final String VALUE = "value";
	protected static final String INDEX = "index";
	protected static final String DEFAULT = "default";
	protected static final String TRANSLATE = "translate";

	protected List<ElementParameter> values;

	protected String[] items;
	protected int[] indexes;

	@Override
	public void fill(Combo combo) {
		final boolean translate = parameters.getParameterBoolean(TRANSLATE);
		values = parameters.getListElementParameter(ITEM);
		items = new String[values.size()];
		indexes = new int[values.size()];
		int i = 0;
		int defaultIndex = -1;
		for (final ElementParameter element : values) {
			String value = parameters.getElementParameter(element, VALUE);
			final int index = parameters.getElementParameterInteger(element, INDEX, -1);
			if (parameters.getElementParameterBoolean(element, DEFAULT)) {
				defaultIndex = i;
			}
			if (translate) {
				value = renderer.getLocalizedMessage(value);
			}
			items[i] = value;
			indexes[i] = index;
			i++;
			combo.add(value);
		}
		if (defaultIndex != -1) {
			combo.select(defaultIndex);
		}
	}

	@Override
	public int valueToIndex(Object value) {
		if (value instanceof Integer) {
			final int val = ((Integer) value).intValue();
			for (int i = 0; i < indexes.length; i++) {
				final int index = indexes[i];
				if (index == val) {
					return i;
				}
			}
		}
		return -1;
	}

	@Override
	public Object indexToValue(int index) {
		if ((index > -1) && (index < items.length)) {
			return indexes[index];
		} else {
			return null;
		}
	}

	@Override
	public Object getResultType() {
		return Integer.class;
	}

}
