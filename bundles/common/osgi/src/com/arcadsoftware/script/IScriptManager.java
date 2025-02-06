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
package com.arcadsoftware.script;

/**
 * The ScriptManager is responsible of global operation on scripting engine.   
 */
public interface IScriptManager {
	
	/**
	 * Create or given an engine to the client. This engine can be binded to any 
	 * object, it is reserved to the client usage. When the engine will be closed
	 * reminding binding will be freed.
	 * 
	 * <p>
	 * Usage of this engine must be release in the same thread. This service can not
	 * ensure thread safety of embedded engines.
	 *   
	 * @param languageName one of the registered engine provider.
	 * @return null if no engine is registered with this language, or the first engine. 
	 */
	public IScriptEngine open(String languageName);
	
	/**
	 * Release the engine.
	 * 
	 * <p>
	 * Must be called in the same thread of the corresponding <cod>open</code> call.
	 * 
	 * @param engine
	 */
	public void close(IScriptEngine engine);
	
	/**
	 * Bind a java variable to any available scripting engine. This binding will be maintaned until the
	 * ScriptManager will be shoot down.
	 *  
	 * @param name
	 * @param value
	 */
	public void globalBind(String name, Object value);

	/**
	 * unbind a globally binded variable.
	 * 
	 * @param name
	 */
	public void globalUnbind(String name);	
}