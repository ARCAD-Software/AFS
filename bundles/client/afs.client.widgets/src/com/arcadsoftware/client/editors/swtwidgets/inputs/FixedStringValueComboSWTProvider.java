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
package com.arcadsoftware.client.editors.swtwidgets.inputs;

import org.eclipse.swt.widgets.Combo;

import com.arcadsoftware.editor.ElementParameter;

/**
 * This class implement a Combo SWT Widget provider for the dynamic editors.
 */
public class FixedStringValueComboSWTProvider extends FixedValueComboSWTProvider {

	protected String[] stringIndexes;

	@Override
	public void fill(Combo combo) {
		final boolean translate = parameters.getParameterBoolean(TRANSLATE);
		values = parameters.getListElementParameter(ITEM);
		items = new String[values.size()];
		stringIndexes = new String[values.size()];
		int i = 0;
		int defaultIndex = -1;
		for (final ElementParameter element : values) {
			String value = parameters.getElementParameter(element, VALUE);
			final String index = parameters.getElementParameter(element, INDEX);
			if (parameters.getElementParameterBoolean(element, DEFAULT)) {
				defaultIndex = i;
			}
			if (translate) {
				value = renderer.getLocalizedMessage(value);
			}
			items[i] = value;
			stringIndexes[i] = index;
			i++;
			combo.add(value);
		}

		if (defaultIndex != -1) {
			combo.select(defaultIndex);
		}
	}

	@Override
	public int valueToIndex(Object value) {
		if (value instanceof String) {
			final String val = (String) value;
			for (int i = 0; i < stringIndexes.length; i++) {
				if (stringIndexes[i].equalsIgnoreCase(val)) {
					return i;
				}
			}
		}
		return -1;
	}

	@Override
	public Object indexToValue(int index) {
		if ((index > -1) && (index < stringIndexes.length)) {
			return stringIndexes[index];
		} else {
			return null;
		}
	}

	@Override
	public Object getResultType() {
		return String.class;
	}

}
