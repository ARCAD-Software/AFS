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
package com.arcadsoftware.metadata.template;

import com.arcadsoftware.metadata.criteria.ISearchCriteria;

public interface IConditionalTemplate extends ITemplate {

	/**
	 * Condition on the first type.
	 * @return
	 */
	public ISearchCriteria getPrimaryCondition();
	
	/**
	 * Condition on the auxiliary type.
	 * @return
	 */
	public ISearchCriteria getSecondaryCondition();
	
	/**
	 * Groovy script to be perform each time the event is thrown.
	 * @return
	 */
	public String getScript();

	/**
	 * define the list of event name that trigger this template.
	 * @return
	 */
	public String getEvent();
}
