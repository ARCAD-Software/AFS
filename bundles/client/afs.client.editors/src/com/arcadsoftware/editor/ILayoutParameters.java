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
package com.arcadsoftware.editor;

import java.util.List;

/**
 * The parameters are transmited to the Widget Provider from the XML layout document.
 * <p>
 * Theses elements correspond to the declared parameter into the extension point that define the Widget Provider.
 */
public interface ILayoutParameters {

	/**
	 * Return a parameter value as defined into the extension point.
	 *
	 * @param name
	 *            the value name.
	 * @return the value as a string.
	 */
	public String getParameter(String name);

	/**
	 * Return a parameter value as defined into the extension point.
	 *
	 * @param name
	 *            the value name.
	 * @param defaultValue
	 *            the default value
	 * @return the value as a string.
	 */
	public String getParameter(String name, String defaultValue);

	/**
	 * Return a parameter value as defined into the extension point.
	 *
	 * @param name
	 *            the value name.
	 * @param defaultValue
	 *            the default value
	 * @return the parameter value as an integer.
	 */
	public int getParameterInteger(String name, int defaultValue);

	/**
	 * Return a parameter value as defined into the extension point.
	 *
	 * @param name
	 *            the value name.
	 * @return the parameter value as a boolean; false if parameter doesn't exist.
	 */
	public boolean getParameterBoolean(String name);

	/**
	 * Return a parameter value as defined into the extension point.
	 *
	 * @param name
	 *            the value name.
	 * @param defaultValue
	 *            the default value.
	 * @return the parameter value as a boolean; false if parameter doesn't exist.
	 */
	public boolean getParameterBoolean(String name, boolean defaultValue);

	/**
	 * The associated action are accessible only to Containers. Input and Decorator providers can not have dependent
	 * Actions. Theses action can be ignored or transmit to the renderer that will add them to the global action list.
	 *
	 * @return a possibly empty list of actions.
	 */
	public List<IActionElement> getActions();

	// public void addParameterToElementParameter(ElementParameter elementParameter, String key, String value); Nothing
	// to do into the interface

	/**
	 * @param elementParameter
	 * @param key
	 * @return
	 */
	public String getElementParameter(ElementParameter elementParameter, String key);

	/**
	 * @param elementParameter
	 * @param key
	 * @param defaultValue
	 * @return
	 */
	public int getElementParameterInteger(ElementParameter elementParameter, String key, int defaultValue);

	/**
	 * @param elementParameter
	 * @param key
	 * @return
	 */
	public boolean getElementParameterBoolean(ElementParameter elementParameter, String key);

	/**
	 * @param elementParameter
	 * @param key
	 * @param defaultValue
	 * @return
	 */
	public String getElementParameter(ElementParameter elementParameter, String key, String defaultValue);

	/**
	 * @param elementName
	 * @return
	 */
	public List<ElementParameter> getListElementParameter(String elementName);

	/**
	 * @param elementName
	 * @return
	 */
	public ElementParameter addElementParameter(String elementName);
}
