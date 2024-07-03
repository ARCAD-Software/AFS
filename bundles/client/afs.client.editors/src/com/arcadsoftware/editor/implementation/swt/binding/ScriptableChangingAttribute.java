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
package com.arcadsoftware.editor.implementation.swt.binding;

import org.eclipse.core.databinding.observable.value.IValueChangingListener;
import org.eclipse.core.databinding.observable.value.ValueChangingEvent;

import com.arcadsoftware.metadata.MetaDataAttribute;

public class ScriptableChangingAttribute implements IValueChangingListener {

	private boolean updating = false;

	/**
	 * @param attribute
	 */
	public ScriptableChangingAttribute(final MetaDataAttribute attribute) {
		super();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.core.databinding.observable.value.IValueChangingListener#handleValueChanging(org.eclipse.core.
	 * databinding .observable.value.ValueChangingEvent)
	 */
	@Override
	public void handleValueChanging(ValueChangingEvent event) {
		// If the change has already be cancelled !
		if (event.veto || updating) {
			return;
		}
		updating = true;
		// TODO RAP
		// try {
		// IScriptEngine engine = Activator.getInstance().openScriptEngine();
		// if (engine != null) {
		// try {
		// engine.bind(ACCEPT, new Boolean(true));
		// engine.bind(OLDVALUE, event.diff.getOldValue());
		// engine.bind(VALUE, event.diff.getNewValue());
		// engine.bind(attribute.getCode(), event.diff.getNewValue());
		// try {
		// engine.eval(attribute.getTest());
		// Object o = engine.getValue(ACCEPT);
		// if (o instanceof Boolean) {
		// event.veto = !((Boolean) o).booleanValue();
		// }
		// } catch (ScriptExecutionException e) {
		// Activator.getInstance().debug(
		// "Error during Script Execution (Validating Attribute Changing Event).", e); //$NON-NLS-1$
		// }
		// } finally {
		// Activator.getInstance().closeStriptEngine(engine);
		// }
		// }
		// } finally {
		// updating = false;
		// }
	}

}
