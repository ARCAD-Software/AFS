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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.Timer;

import org.codehaus.groovy.ast.expr.Expression;
import org.codehaus.groovy.ast.expr.MethodPointerExpression;
import org.codehaus.groovy.ast.expr.StaticMethodCallExpression;
import org.codehaus.groovy.ast.stmt.Statement;
import org.codehaus.groovy.ast.stmt.ThrowStatement;
import org.codehaus.groovy.control.CompilerConfiguration;
import org.codehaus.groovy.control.ConfigurationException;
import org.codehaus.groovy.control.customizers.SecureASTCustomizer;

import groovy.lang.Binding;
import groovy.lang.GroovyClassLoader;
import groovy.lang.GroovyObjectSupport;
import groovy.lang.GroovyShell;
import groovy.lang.Script;
import groovy.util.Eval;

/**
 * Pre-configured secured Configuration for Groovy script evaluation.
 * 
 * <p>
 * This Configuration add a high level of restriction in the interaction with the JVM classes. All the following dependencies are forbidden:
 * 
 * <nl>
 * <li>Package definition.
 * <li>Any explicit imports or static imports.
 * <li>Direct usage of System, Runtime, Object, Class, Thread, File and Void classes.
 * <li>Direct usage of Groovy classes: Eval, Script and GroovyShell.
 * <li>Throw statement.
 * <li>Any static method call and pointer method calls.
 * </nl>
 * 
 * <p>
 * The recommended usage is to provide an "API" object, bind it as variable, wrap the script with an "api.with" and set the SecuredCompilerConfiguration:
 * 
 * <pre>
 * Binding b = new Binding();
 * API api = new API();
 * b.setVariable("api", api); //$NON-NLS-1$
 * GroovyShell shell = new GroovyShell(API.class.getClassLoader(), b, new SecuredCompilerConfiguration());
 * shell.evaluate("api.with {" + script + "}"); //$NON-NLS-1$ //$NON-NLS-2$
 * </pre>
 * 
 * @author ARCAD Software
 * @see SecureASTCustomizer
 */
public class SecuredCompilerConfiguration extends CompilerConfiguration {

	private static final List<String> NONE = new ArrayList<String>();
	private static final List<String> DISALLOWEDRECEIVERS = Arrays.asList(System.class.getName(), Class.class.getName(),
			Thread.class.getName(), ThreadDeath.class.getName(), ThreadGroup.class.getName(), ClassLoader.class.getName(),
			ThreadLocal.class.getName(), Runtime.class.getName(), InternalError.class.getName(), Timer.class.getName(),
			InterruptedException.class.getName(), IllegalThreadStateException.class.getName(),
			InheritableThreadLocal.class.getName(), IllegalAccessError.class.getName(), IllegalAccessException.class.getName(),
			Void.class.getName(), Script.class.getName(), GroovyShell.class.getName(), Eval.class.getName(),
			InputStream.class.getName(), FileInputStream.class.getName(), OutputStream.class.getName(), FileOutputStream.class.getName(),
			File.class.getName(), GroovyClassLoader.class.getName(), Binding.class.getName(), GroovyObjectSupport.class.getName());
	//  Object.class.getName(), -> necessary for all basic manipulation of objects.
	private static final List<Class<? extends Expression>> DISALLOWEDEXPRESSIONS = Arrays.asList(StaticMethodCallExpression.class,
			MethodPointerExpression.class);
	private static final List<Class<? extends Statement>> DISALLOWEDSTATEMENTS = Arrays.asList(ThrowStatement.class);
			
	private final SecureASTCustomizer secureASTCustomizer = new SecureASTCustomizer();
	
	public SecuredCompilerConfiguration() {
		super();
		securityInitialization();
	}

	public SecuredCompilerConfiguration(CompilerConfiguration configuration) {
		super(configuration);
		securityInitialization();
	}

	public SecuredCompilerConfiguration(Properties configuration) throws ConfigurationException {
		super(configuration);
		securityInitialization();
	}

	@SuppressWarnings("deprecation")
	protected void securityInitialization() {
		secureASTCustomizer.setPackageAllowed(false);
		secureASTCustomizer.setAllowedImports(NONE);
		secureASTCustomizer.setAllowedStarImports(NONE);
		secureASTCustomizer.setAllowedStaticImports(NONE);
		secureASTCustomizer.setDisallowedReceivers(DISALLOWEDRECEIVERS);
		secureASTCustomizer.setDisallowedExpressions(DISALLOWEDEXPRESSIONS);
		secureASTCustomizer.setDisallowedStatements(DISALLOWEDSTATEMENTS);
		addCompilationCustomizers(secureASTCustomizer);
		super.setOutput(null);
	}

	@Deprecated
	@Override
	public void setOutput(PrintWriter output) {}

	@Override
	public boolean isGroovydocEnabled() {
		return false;
	}

	@Override
	public boolean isRuntimeGroovydocEnabled() {
		return false;
	}

	/**
	 * Get the default AST Customizer used to secure the Groovy configuration.
	 * 
	 * @return
	 */
	public SecureASTCustomizer getSecureASTCustomizer() {
		return secureASTCustomizer;
	}
}
