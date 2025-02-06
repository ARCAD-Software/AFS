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
package com.arcadsoftware.editor;

import java.util.HashMap;

import com.arcadsoftware.editor.implementation.Activator;

public class ElementParameter {

	private static final String TRUE = "true"; //$NON-NLS-1$
	private static final String YES = "yes"; //$NON-NLS-1$

	private final String name;
	private final HashMap<String, String> params = new HashMap<>();

	public ElementParameter(String name) {
		super();
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public HashMap<String, String> getParams() {
		return params;
	}

	public String getParameter(String parameterName) {
		return params.get(parameterName);
	}

	public boolean getParameterBoolean(String key) {
		final String value = getParameter(key);
		return ((value != null) && (TRUE.equalsIgnoreCase(value) || YES.equalsIgnoreCase(value)));
	}

	public int getParameterInteger(String key, int defaultValue) {
		final String val = params.get(key);
		if (val == null) {
			return defaultValue;
		}
		try {
			return Integer.parseInt(val);
		} catch (final NumberFormatException e) {
			Activator.getInstance().log(e);
			return defaultValue;
		}
	}
}
