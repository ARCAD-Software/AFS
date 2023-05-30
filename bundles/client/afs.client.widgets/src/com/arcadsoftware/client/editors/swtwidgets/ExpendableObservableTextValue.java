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
package com.arcadsoftware.client.editors.swtwidgets;

import org.eclipse.core.databinding.observable.Diffs;
import org.eclipse.core.databinding.observable.Realm;
import org.eclipse.core.databinding.observable.value.AbstractObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.forms.widgets.ExpandableComposite;

public class ExpendableObservableTextValue extends AbstractObservableValue<String> {

	private ExpandableComposite composite;
	
	public ExpendableObservableTextValue(ExpandableComposite composite) {
		this(SWTObservables.getRealm(composite.getDisplay()), composite);
	}

	public ExpendableObservableTextValue(Realm realm,ExpandableComposite composite) {
		super(realm);
		this.composite = composite;
		composite.addDisposeListener(disposeListener);
	}
	
	private DisposeListener disposeListener = new DisposeListener() {
		public void widgetDisposed(DisposeEvent e) {
			ExpendableObservableTextValue.this.dispose();
		}
	};

	@Override
	protected void doSetValue(String value) {
		String oldValue = composite.getText();
		String newValue = value == null ? "" : value.toString(); //$NON-NLS-1$
		composite.setText(newValue);
		if (!newValue.equals(oldValue)) {
			fireValueChange(Diffs.createValueDiff(oldValue, newValue));
		}
	}

	@Override
	protected String doGetValue() {
		return composite.getText();
	}

	public Widget getWidget() {
		return composite;
	}

	public Object getValueType() {
		return String.class;
	}

}
