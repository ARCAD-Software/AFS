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
package com.arcadsoftware.osgi.internal;

import org.osgi.framework.ServiceReference;

/**
 * Temp log entry, waiting to be logged.
 */
public class WaitingLogEntry {

	private ServiceReference<?> reference;
	private int level;
	private String message;
	private Throwable exception;

	/**
	 * @param exception
	 * @param level
	 * @param message
	 * @param reference
	 */
	public WaitingLogEntry(ServiceReference<?> reference, int level, String message, Throwable exception) {
		super();
		this.exception = exception;
		this.level = level;
		this.message = message;
		this.reference = reference;
	}

	public ServiceReference<?> getReference() {
		return reference;
	}

	public void setReference(ServiceReference<?> reference) {
		this.reference = reference;
	}

	public int getLevel() {
		return level;
	}

	public void setLevel(int level) {
		this.level = level;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public Throwable getException() {
		return exception;
	}

	public void setException(Throwable exception) {
		this.exception = exception;
	}
	
}
