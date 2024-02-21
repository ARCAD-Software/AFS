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

/**
 * This kind of criteria compare two attributes values.
 * 
 * If the references imply more that one domain of entities then the base reference (the part of the reference that bolong to
 * the domain of the selected entity) must be the same for the two attributes.
 * 
 * Creation Date: 4 oct. 2011
 */
public interface IAttributesCriteria extends IAttributeCriteria {

	/**
	 * Define the second attribute code associated to the condition.
	 * 
	 * <p>
	 * This support reference line (aka. attributes code separated with code that represent a list of references).
	 *   
	 * @param code The second attribute code to set.
	 */
	public void setSecondAttribute(String code);
	
	/**
	 * @return The second attribute code.
	 */
	public String getSecondAttribute();

}
