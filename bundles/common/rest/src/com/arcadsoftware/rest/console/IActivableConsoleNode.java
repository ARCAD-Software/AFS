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
package com.arcadsoftware.rest.console;

/**
 * This Console Section can be activated (or deactivated) according to some conditions.
 *  
 * <p>Deactivated section are not displayed into the list of Sections.
 *  
 * @see IRestConsoleSection
 * @author ARCAD Software
 */
public interface IActivableConsoleNode {

	/**
	 * Indicate if the Section is currently activated or not.
	 * 
	 * @return
	 */
	public boolean isActivated();
	
}
