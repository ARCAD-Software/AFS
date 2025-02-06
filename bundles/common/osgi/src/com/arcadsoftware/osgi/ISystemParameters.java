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
package com.arcadsoftware.osgi;

/**
 * System parameters generation service.
 * 
 * @since 1.2.0
 */
public interface ISystemParameters {

	/**
	 * return the "system parameters" string. this is an hashed string given from the concat of undocumented resources information.
	 * 
	 * <p>
	 * This string is 10 characters long, formed by the following code :
	 * 
	 * <p>
	 * <b>XXX-XX-XXX</b>
	 * 
	 * <p>
	 * where X characters are Hexadecimal character (0-9, A-F).
	 * 
	 * @return a non null string.
	 */
	public String getSystemParameters();

}
