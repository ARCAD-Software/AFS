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

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.util.tracker.ServiceTracker;

/**
 * Helper class to build tracker about IScriptManager implementation.
 * <p>
 * This implementation does not support multiple ScriptManager services.
 */
public class ScriptEngineTracker extends ServiceTracker<IScriptManager, IScriptManager> implements IScriptManager {

	/**
	 * Create the tracker, don't forget to open it with <code>open()</code>.
	 * 
	 * @param context the current bundle context.
	 */
	public ScriptEngineTracker(BundleContext context) {
		super(context, IScriptManager.class.getName(), null);
	}

	@Override
	public void close(IScriptEngine engine) {
		for(ServiceReference<IScriptManager> reference:getServiceReferences()) {
			IScriptManager manager = getService(reference);
			if (manager != null) {
				manager.close(engine);
			}
		}
	}

	@Override
	public void globalBind(String name, Object value) {
		for(ServiceReference<IScriptManager> reference:getServiceReferences()) {
			IScriptManager manager = getService(reference);
			if (manager != null) {
				manager.globalBind(name, value);
			}
		}
	}

	@Override
	public void globalUnbind(String name) {
		for(ServiceReference<IScriptManager> reference:getServiceReferences()) {
			IScriptManager manager = getService(reference);
			if (manager != null) {
				manager.globalUnbind(name);
			}
		}
	}

	@Override
	public IScriptEngine open(String languageName) {
		for(ServiceReference<IScriptManager> reference:getServiceReferences()) {
			IScriptManager manager = getService(reference);
			if (manager != null) {
				IScriptEngine result = manager.open(languageName);
				if (result != null) {
					return result;
				}
			}
		}
		return null;
	}

	/**
	 * Try to execute the given script with the given script language.
	 * Give no information if an error occurs during evaluation.
	 * 
	 * @param languageName the script language name (for instance "groovy").
	 * @param script the script itself.
	 * @return the execution result or null if no engine are available or if an Error occurs.
	 */
	public Object evaluate(String languageName, String script) {
		IScriptEngine engine = open(languageName);
		if (engine != null) {
			try {
				return engine.eval(script);
			} catch (ScriptExecutionException e) {
			} finally {
				close(engine);
			}
		}
		return null;
	}
	
}
