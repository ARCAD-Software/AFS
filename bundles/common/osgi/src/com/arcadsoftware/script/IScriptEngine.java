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
package com.arcadsoftware.script;

/**
 * Define a basic interface for a language specific Script engine.
 */
public interface IScriptEngine {
	
	/**
	 * Open the engine.
	 */
	public void open();
	
	/**
	 * Close the engine.
	 * 
	 * <p>
	 * Do not dispose persistent and thread safe resources.
	 */
	public void close();
	
	/**
	 * Bind a Java Object to the script context.
	 * 
	 * <p> This binding can use a simplified access to BeanMap attributes if possible. If not it must bind
	 * the BeanMap as a simple object.
	 * 
	 * @param name the object name into the script context.
	 * @param value
	 */
	public void bind(String name, Object value);
	
	/**
	 * Unbind a previously binded object.
	 * 
	 * @param name the object name into the script context.
	 */
	public void unBind(String name);

	/**
	 * Evaluate a script.
	 * 
	 * @param script
	 * @return
	 * @throws ScriptExecutionException
	 */
	public Object eval(String script) throws ScriptExecutionException;
	
	/**
	 * Return the java value of a variable into the script context.
	 * 
	 * Warning: this may not be implemented by all script engines.
	 * 
	 * @param name
	 * @return
	 */
	public Object getValue(String name);
}
