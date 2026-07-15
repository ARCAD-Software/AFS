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
package com.arcadsoftware.groovy.security;

import java.io.StringWriter;

import org.codehaus.groovy.control.CompilationFailedException;

import groovy.lang.Binding;
import groovy.lang.GroovyCodeSource;
import groovy.lang.GroovyShell;
import groovy.lang.Script;

/**
 * Simple secured GroovyShell implementation.
 * 
 * <p>
 * Along with more secured execution context this class provide the following behavior:
 * 
 * <ul>
 * <li> given "IScriptAPI" methods and fields (getter and setter) are directly accessible 
 * in the script without the need to name any variable name. 
 * <li> The "print" and "println" command of Groovy are redirected to a string accessible, 
 * after execution through the <code>getConsoleMessage()</code> method. 
 * </ul>
 * 
 * @author ARCAD Software
 * @see IScriptAPI
 */
public class SecureGroovyShell extends GroovyShell {

	private static Binding createBinding(IScriptAPI api) {
		Binding binding = new Binding();
		binding.setVariable("api", api); //$NON-NLS-1$
		return binding;
	}

	private final StringWriter out;
	
	public SecureGroovyShell(IScriptAPI api) {
		super(api.getClass().getClassLoader(), createBinding(api), new SecuredCompilerConfiguration());
		out = new StringWriter();
	}

	@Override
	public Object evaluate(String scriptText, String fileName, String codeBase) throws CompilationFailedException {
		return super.evaluate("api.with {" + scriptText + '}', fileName, codeBase); //$NON-NLS-1$
	}

	@Override
    public Object evaluate(GroovyCodeSource codeSource) throws CompilationFailedException {
		getContext().setVariable("out", out); //$NON-NLS-1$
		return super.evaluate(codeSource);
    }

	@Override
    public Script parse(String scriptText) throws CompilationFailedException {
        return super.parse("api.with {" + scriptText + '}'); //$NON-NLS-1$
    }

	public String getConsoleMessages() {
		return out.toString();
	}
}
