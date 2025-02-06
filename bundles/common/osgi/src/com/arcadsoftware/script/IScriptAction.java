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
 * Define a basic calling interface allowing to implement script calls to java code from
 * other bundles.
 * 
 * <p>
 * This interface is a very simple one and can change in the future version of this service.
 */
public interface IScriptAction {

	/*
	 * These are the very common parameters used with such actions.
	 * Nature and number of parameters are dependent from the caller and action contract.
	 */
	public static final String PARAM_RESULT = "result"; //$NON-NLS-1$
	public static final String PARAM_ITEM = "item"; //$NON-NLS-1$
	public static final String PARAM_PARENT = "parent"; //$NON-NLS-1$
	public static final String PARAM_ENTITY = "entity"; //$NON-NLS-1$
	
	/**
	 * Change the value of this parameter.
	 * 
	 * @param paramName
	 * @param paramValue
	 */
	public void set(String paramName,Object paramValue);
	
	/**
	 * Return the current value of the parameter output parameter must be set during the execution. 
	 * @param paramName
	 * @return
	 */
	public Object get(String paramName);
	
	/**
	 * Run the action.
	 * 
	 * <p>
	 * Side effect must be held into parameters values.
	 * 
	 * @return True if the execution succeed.
	 */
	public boolean run() throws ScriptExecutionException;
}
