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
package com.arcadsoftware.metadata.criteria;

public interface IAttributeCriteria {

	/**
	 * Define the attribute code associated to the condition.
	 * 
	 * <p>
	 * This support reference line (aka. attributes code separated with code that represent a list of references).
	 *   
	 * @param code The attribute code.
	 */
	public void setAttribute(String code);
	
	/**
	 * @return The attribute code.
	 */
	public String getAttribute();
}
