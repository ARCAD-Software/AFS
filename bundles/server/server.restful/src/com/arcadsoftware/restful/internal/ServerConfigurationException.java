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
package com.arcadsoftware.restful.internal;

public class ServerConfigurationException extends Exception {

	private static final long serialVersionUID = 647182112030301419L;

	public ServerConfigurationException() {
		super();
	}

	public ServerConfigurationException(String message) {
		super(message);
	}

	public ServerConfigurationException(Throwable cause) {
		super(cause);
	}

	public ServerConfigurationException(String message, Throwable cause) {
		super(message, cause);
	}

	public ServerConfigurationException(String message, Throwable cause, boolean enableSuppression,
			boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

}
