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
package com.arcadsoftware.afs.framework.ui.beanmap.composites;

import java.util.Hashtable;
import java.util.Set;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import com.arcadsoftware.beanmap.BeanMap;

public class BeanMapControler {

	private final Composite composite;
	private final Hashtable<Control, String> attributeBinder;
	private final Hashtable<String, Control> controlBinder;
	private BeanMap updated;

	public BeanMapControler(Composite composite) {
		attributeBinder = new Hashtable<>();
		controlBinder = new Hashtable<>();
		this.composite = composite;
	}

	public Composite getComposite() {
		return composite;
	}

	public void setBeanMap(BeanMap original) {
		beanMapToControl(original);
		// We make the assignment after to clear the Updated BeanMap
		// which will probably be updated during beanMapToControl execution
		updated = original.duplicate();
	}

	private void beanMapToControl(BeanMap original) {
		final Set<String> keys = original.keys();
		for (final String attribute : keys) {
			final Control control = controlBinder.get(attribute);
			if (control != null) {
				BeanMapConverter.setValue(control, original.get(attribute));
			}
		}
	}

	public void controlToBeanMap(BeanMap updated) {
		final Set<Control> keys = attributeBinder.keySet();
		for (final Control control : keys) {
			final String attribute = attributeBinder.get(control);
			if (attribute != null) {
				updated.put(attribute, BeanMapConverter.getValue(control));
			}
		}
	}

	public void bindControl(String attribute, Control c) {
		attributeBinder.put(c, attribute);
		controlBinder.put(attribute, c);
	}

	public BeanMap getUpdated() {
		if (updated != null) {
			controlToBeanMap(updated);
		}
		return updated;
	}

}
