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
package com.arcadsoftware.editor.implementation;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.arcadsoftware.editor.ElementParameter;
import com.arcadsoftware.editor.IActionElement;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.metadata.Element;

public class LayoutElement implements ILayoutParameters {

	private static final String TRUE = "true"; //$NON-NLS-1$
	private static final String YES = "yes"; //$NON-NLS-1$

	private final String name;
	private final Object provider;
	private final HashMap<String, String> params = new HashMap<>();
	private final List<ElementParameter> elementParams = new ArrayList<>();
	private final Element element;
	private final ArrayList<LayoutElement> container = new ArrayList<>();
	private final ArrayList<IActionElement> actions = new ArrayList<>();

	/**
	 * @param name
	 * @param provider
	 * @param attribute
	 */
	public LayoutElement(String name, Object provider, Element element) {
		super();
		this.name = name;
		this.provider = provider;
		this.element = element;
	}

	public String getName() {
		return name;
	}

	public ArrayList<LayoutElement> getContaint() {
		return container;
	}

	public Object getProvider() {
		return provider;
	}

	public void addParameter(String parameterName, String value) {
		params.put(parameterName, value);
	}

	@Override
	public ElementParameter addElementParameter(String elementName) {
		final ElementParameter elementParameter = new ElementParameter(elementName);
		elementParams.add(elementParameter);
		return elementParameter;
	}

	public void addParameterToElementParameter(ElementParameter elementParameter, String key, String value) {
		elementParameter.getParams().put(key, value);
	}

	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.editor.IParameters#getParameter(java.lang.String)
	 */
	@Override
	public String getParameter(String parameterName) {
		return params.get(parameterName);
	}

	@Override
	public String getElementParameter(ElementParameter elementParameter, String key) {
		return elementParameter.getParams().get(key);
	}

	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.editor.IParameters#getParameterInteger(java.lang.String ,int)
	 */
	@Override
	public int getParameterInteger(String parameterName, int defaultValue) {
		final String val = params.get(parameterName);
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

	@Override
	public int getElementParameterInteger(ElementParameter elementParameter, String key, int defaultValue) {
		final String val = getElementParameter(elementParameter, key);
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

	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.editor.IParameters#getParameterBoolean(java.lang.String )
	 */
	@Override
	public boolean getParameterBoolean(String key) {
		final String value = getParameter(key);
		return (value != null) && (TRUE.equalsIgnoreCase(value) || YES.equalsIgnoreCase(value));
	}

	@Override
	public boolean getParameterBoolean(String key, boolean defaultValue) {
		final String value = getParameter(key);
		if ((value == null) || (value.trim().length() == 0)) {
			return defaultValue;
		}
		return TRUE.equalsIgnoreCase(value) || YES.equalsIgnoreCase(value);
	}

	@Override
	public boolean getElementParameterBoolean(ElementParameter elementParameter, String key) {
		final String value = getElementParameter(elementParameter, key);
		return (value != null) && (TRUE.equalsIgnoreCase(value) || YES.equalsIgnoreCase(value));
	}

	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.editor.IParameters#getElement()
	 */
	public Element getElement() {
		return element;
	}

	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.editor.ILayoutParameters#isContainer()
	 */
	public boolean isContainer() {
		return !container.isEmpty();
	}

	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.editor.ILayoutParameters#getActions()
	 */
	@Override
	public List<IActionElement> getActions() {
		return actions;
	}

	/*
	 * (non-Javadoc)
	 * @see com.arcadsoftware.editor.ILayoutParameters#getParameter(java.lang.String, java.lang.String)
	 */
	@Override
	public String getParameter(String parameterName, String defaultValue) {
		final String result = params.get(parameterName);
		if (result == null) {
			return defaultValue;
		}
		return result;
	}

	@Override
	public String getElementParameter(ElementParameter elementParameter, String key, String defaultValue) {
		final String result = getElementParameter(elementParameter, key);
		if (result == null) {
			return defaultValue;
		}
		return result;
	}

	@Override
	public List<ElementParameter> getListElementParameter(String elementName) {
		if (elementParams == null) {
			return null;
		}
		final List<ElementParameter> result = new ArrayList<>(elementParams.size());
		for (final ElementParameter elementParameter : elementParams) {
			if (elementParameter.getName().equals(elementName)) {
				result.add(elementParameter);
			}
		}
		return result;
	}

}
