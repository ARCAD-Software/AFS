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
package com.arcadsoftware.client.editors.swtwidgets;

import org.eclipse.core.databinding.observable.Realm;
import org.eclipse.core.databinding.observable.value.AbstractObservableValue;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.client.editors.swtwidgets.decorators.VirtualLabelSWTProvider;

public class VirtualObservableValue extends AbstractObservableValue<Object> {

	private Object value;
	
	public VirtualObservableValue(VirtualLabelSWTProvider provider) {
		this(Realm.getDefault(), provider);
	}

	public VirtualObservableValue(Realm realm, VirtualLabelSWTProvider provider) {
		super(realm);
	}

	@Override
	protected Object doGetValue() {
		return value;
	}

	@Override
	public Object getValueType() {
		return BeanMap.class;
	}

	@Override
	protected void doSetValue(Object newValue) {
		value = newValue;
	}

}
