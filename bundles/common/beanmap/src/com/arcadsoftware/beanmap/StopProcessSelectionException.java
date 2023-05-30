/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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
package com.arcadsoftware.beanmap;

/**
 * This Exception can be thrown if the process selection should be stopped.
 * 
 * <p>
 * This can be useful if the entity should to be associated with default association mechanisms.
 * 
 * @see IProcessSelector
 */
public class StopProcessSelectionException extends Exception {

	private static final long serialVersionUID = -4590207369773549998L;

	public StopProcessSelectionException() {
		super();
	}

	public StopProcessSelectionException(String message, Throwable cause) {
		super(message, cause);
	}

	public StopProcessSelectionException(String message) {
		super(message);
	}

	public StopProcessSelectionException(Throwable cause) {
		super(cause);
	}

}
