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
package com.arcadsoftware.groovy.security;

import org.codehaus.groovy.control.CompilationFailedException;

import groovy.lang.Binding;
import groovy.lang.GroovyShell;

/**
 * Simple secured GroovyShell implementation.
 * 
 * @author ARCAD Software
 */
public class SecureGroovyShell extends GroovyShell {

	private static Binding createBinding(IScriptAPI api) {
		Binding binding = new Binding();
		binding.setVariable("api", api); //$NON-NLS-1$
		return null;
	}

	public SecureGroovyShell(IScriptAPI api) {
		super(api.getClass().getClassLoader(), createBinding(api), new SecuredCompilerConfiguration());
	}

	@Override
	public Object evaluate(String scriptText) throws CompilationFailedException {
		return super.evaluate("api.with {" + scriptText + '}'); //$NON-NLS-1$
	}

	@Override
	public Object evaluate(String scriptText, String fileName) throws CompilationFailedException {
		return super.evaluate("api.with {" + scriptText + '}', fileName); //$NON-NLS-1$
	}

	@Override
	public Object evaluate(String scriptText, String fileName, String codeBase) throws CompilationFailedException {
		return super.evaluate("api.with {" + scriptText + '}', fileName, codeBase); //$NON-NLS-1$
	}

	
}
