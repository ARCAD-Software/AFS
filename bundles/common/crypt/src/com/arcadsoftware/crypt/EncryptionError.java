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

/**
 * This Exception is a simple wraper around all security exception that may occurs during encryption operations.
 * 
 * @author ARCAD Software
 */
public class EncryptionError extends RuntimeException {

	private static final long serialVersionUID = -804600849257732396L;

	public EncryptionError() {
		super();
	}

	public EncryptionError(String message) {
		super(message);
	}

	public EncryptionError(Throwable cause) {
		super(cause);
	}

	public EncryptionError(String message, Throwable cause) {
		super(message, cause);
	}

	public EncryptionError(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

}
