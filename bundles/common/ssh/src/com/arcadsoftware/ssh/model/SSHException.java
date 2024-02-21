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
package com.arcadsoftware.ssh.model;

public class SSHException extends Exception {
	
	private static final long serialVersionUID = -3386197559157866233L;

	public SSHException(final String message) {
		super(message);
	}

	public SSHException(final String message, final Throwable cause) {
		super(message, cause);
	}

	public SSHException(final Throwable cause) {
		super(cause);
	}
}
