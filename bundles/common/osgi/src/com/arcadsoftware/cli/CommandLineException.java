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
package com.arcadsoftware.cli;

public class CommandLineException extends Exception {

	private static final long serialVersionUID = -2080557529210472175L;

	public CommandLineException() {
		super();
	}

	public CommandLineException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

	public CommandLineException(String message, Throwable cause) {
		super(message, cause);
	}

	public CommandLineException(String message) {
		super(message);
	}

	public CommandLineException(Throwable cause) {
		super(cause);
	}

}
