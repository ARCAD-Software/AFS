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
package com.arcadsoftware.mail;

/**
 * Exception thrown during Incoming Email processing.
 * 
 */
public class EmailProcessException extends Exception {

	private static final long serialVersionUID = 1348568082862497656L;

	private boolean stop = false;
	
	public EmailProcessException() {
		super();
	}

	public EmailProcessException(String message,boolean stop) {
		super(message);
		this.stop = stop;
	}

	public EmailProcessException(String message) {
		super(message);
	}

	/**
	 * @param cause
	 */
	public EmailProcessException(Throwable cause) {
		super(cause);
	}

	/**
	 * @param message
	 * @param cause
	 */
	public EmailProcessException(String message, Throwable cause) {
		super(message, cause);
	}

	public boolean shouldStop() {
		return stop;
	}
}

