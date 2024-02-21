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
package com.arcadsoftware.script;

/**
 * Engine provider are thread safe constructor of Scripting engine.
 */
public interface IEngineProvider {

	//helper static...
	public static final String clazz = IEngineProvider.class.getName();
	
	// Engine provider identifier.
	public static final String PROP_LANGUAGE = "script.lang"; //$NON-NLS-1$

	// firsts scripting engines envisaged :
	public static final String LANGUAGE_GROOVY = "groovy"; //$NON-NLS-1$
	public static final String LANGUAGE_JAVASCRIPT = "javascript"; //$NON-NLS-1$
	public static final String LANGUAGE_RUBY = "ruby"; //$NON-NLS-1$
	public static final String LANGUAGE_PYTHON = "python"; //$NON-NLS-1$
	public static final String LANGUAGE_LISP = "lisp"; //$NON-NLS-1$
	
	/**
	 * Create new Engine. this Engine is context free and need to be initialized first.
	 *  
	 * @param parent the classloader from the script manager.
	 * @return a new engine instance.
	 */
	public IScriptEngine create(ClassLoader parent);
	
	/**
	 * Release (destroy) an Engine.
	 * 
	 * @param engine
	 */
	public void release(IScriptEngine engine);
}
