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
package com.arcadsoftware.crypt;

public class ConfiguredSSLContextException extends Exception {

	private static final long serialVersionUID = 5389889526706766502L;

	public ConfiguredSSLContextException() {
		super();
	}

	public ConfiguredSSLContextException(String message) {
		super(message);
	}

	public ConfiguredSSLContextException(Throwable cause) {
		super(cause.getLocalizedMessage(), cause);
	}

	public ConfiguredSSLContextException(String message, Throwable cause) {
		super(message, cause);
	}

	public ConfiguredSSLContextException(String message, Throwable cause, boolean enableSuppression,
			boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

}
