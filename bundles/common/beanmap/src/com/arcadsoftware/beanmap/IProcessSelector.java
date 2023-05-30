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

import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * A process selector is called each time an data is created or changed. 
 * It must return the currently associated process name.
 * 
 * <p>
 * This process can change depending of the data attributes or external constraints.
 * 
 * <p>
 * If the returned process is already instantiated and associated to the given data
 * then the current instance is used. If not then a new instance is created.
 * 
 */
public interface IProcessSelector {

	public static final String clazz = IProcessSelector.class.getName();
	
	/**
	 * Return a process name that should associated to the given data. 
	 * This process will be instantiated or used as the current entity work flow.
	 * 
	 * <p>
	 * If this selector return null, then other selector may take this association
	 * into account. If none of the selector services return a process name then 
	 * the default selection will be used. The default selection look for a process 
	 * that contain a variable "arcad_type" initialized with the current BeanMap
	 * type.
	 * 
	 * <p>
	 * If this selector should take this association into account but can not return
	 * the current process associated to the data, then the selector need to throw an Exception
	 * to stop the selection mechanism. 
	 * 
	 * @param bean The data that should be associated to the returned work flow.
	 * @param user The current user if any.
	 * @return A Process name.
	 * @throws StopProcessSelectionException thrown if default association mechanism should be delayed.
	 */
	public String selectProcess(BeanMap bean, IConnectionUserBean user) throws StopProcessSelectionException;
}
