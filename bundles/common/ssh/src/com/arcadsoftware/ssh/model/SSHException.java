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
package com.arcadsoftware.ssh.model;

/**
 * Exception class used to indicate errors related to SSH operations.
 * <p>
 * This exception is typically thrown when an SSH key generation, import,
 * or communication process fails.
 * </p>
 * 
 * @author ARCAD Software
 */
public class SSHException extends Exception {

    private static final long serialVersionUID = -3386197559157866233L;

    /**
     * Constructs a new {@code SSHException} with the specified detail message.
     *
     * @param message the detail message describing the exception
     */
    public SSHException(final String message) {
        super(message);
    }

    /**
     * Constructs a new {@code SSHException} with the specified detail message and cause.
     *
     * @param message the detail message describing the exception
     * @param cause the cause of the exception (which is saved for later retrieval by the {@link #getCause()} method)
     */
    public SSHException(final String message, final Throwable cause) {
        super(message, cause);
    }

    /**
     * Constructs a new {@code SSHException} with the specified cause.
     *
     * @param cause the cause of the exception (which is saved for later retrieval by the {@link #getCause()} method)
     */
    public SSHException(final Throwable cause) {
        super(cause);
    }
}

