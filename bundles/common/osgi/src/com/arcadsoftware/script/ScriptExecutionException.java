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
 * Exception throw during script execution.
 */
public class ScriptExecutionException extends Exception {

	private static final long serialVersionUID = 4346286077879067039L;

	private String script;
	private String language = "Unkown"; //$NON-NLS-1$

	/**
	 * 
	 */
	public ScriptExecutionException() {
		super();
	}

	/**
	 * @param language
	 * @param script
	 */
	public ScriptExecutionException(String language, String script) {
		super();
		this.language = language;
		this.script = script;
	}

	/**
	 * @param message
	 */
	public ScriptExecutionException(String message) {
		super(message);
	}

	/**
	 * @param message
	 */
	public ScriptExecutionException(String language, String script, String message) {
		super(message);
		this.language = language;
		this.script = script;
	}

	/**
	 * @param cause
	 */
	public ScriptExecutionException(Throwable cause) {
		super(cause);
	}

	/**
	 * @param cause
	 */
	public ScriptExecutionException(String language, String script, Throwable cause) {
		super(cause);
		this.language = language;
		this.script = script;
	}

	/**
	 * @param message
	 * @param cause
	 */
	public ScriptExecutionException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * @param message
	 * @param cause
	 */
	public ScriptExecutionException(String language, String script, String message, Throwable cause) {
		super(message, cause);
		this.language = language;
		this.script = script;
	}

	/**
	 * @return the script
	 */
	public String getScript() {
		return script;
	}

	/**
	 * @return the language
	 */
	public String getLanguage() {
		return language;
	}

}
