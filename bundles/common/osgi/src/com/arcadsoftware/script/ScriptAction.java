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

import java.util.HashMap;

/**
 * Create an abstract Scriptable action with an HasMap to store the parameters.
 */
public abstract class ScriptAction implements IScriptAction {

	private HashMap<String, Object> params = new HashMap<String, Object>(); 
	
	/**
	 * Construct the Action.
	 */
	public ScriptAction() {
		super();
	}

	@Override
	public Object get(String paramName) {
		return params.get(paramName);
	}

	@Override
	public abstract boolean run() throws ScriptExecutionException;

	@Override
	public void set(String paramName, Object paramValue) {
		params.put(paramName, paramValue);
	}

}
