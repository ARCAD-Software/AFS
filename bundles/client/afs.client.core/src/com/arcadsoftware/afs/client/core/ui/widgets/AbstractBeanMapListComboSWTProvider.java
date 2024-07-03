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
package com.arcadsoftware.afs.client.core.ui.widgets;

import org.eclipse.swt.widgets.Combo;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

public abstract class AbstractBeanMapListComboSWTProvider extends AbstractConnectedFixedValueCombo {

	private BeanMapList values;

	@Override
	public void fill(Combo combo) {
		values = getValues();
		int index = -1;
		for (int i = 0; i < values.size(); i++) {
			final BeanMap r = values.get(i);
			final String displayValue = getDisplayValue(r);
			combo.add(displayValue);
			if (isDefaultValue(r)) {
				index = i;
			}
		}
		combo.select(index);
	}

	@Override
	public int valueToIndex(Object value) {
		if (value instanceof String) {
			final String val = (String) value;
			for (int i = 0; i < values.size(); i++) {
				final BeanMap r = values.get(i);
				final String beanValue = getValue(r);
				if (val.equalsIgnoreCase(beanValue)) {
					return i;
				}
			}
		}
		return -1;
	}

	@Override
	public Object indexToValue(int index) {
		if ((index > -1) && (index < values.size())) {
			final BeanMap r = values.get(index);
			return getValue(r);
		} else {
			return null;
		}
	}

	@Override
	public Object getResultType() {
		return String.class;
	}

	protected boolean isDefaultValue(BeanMap b) {
		return false;
	}

	public abstract String getDisplayValue(BeanMap b);

	public abstract String getValue(BeanMap b);

	public abstract BeanMapList getValues();

}
