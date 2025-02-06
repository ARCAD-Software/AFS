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
package com.arcadsoftware.metadata.criteria;

/**
 * This criteria is relative to some MetaDatEntity links.
 * 
 * @author ARCAD Software
 */
public interface ILinkCriteria extends ISearchCriteria {

	/**
	 * Get the Link code.
	 * 
	 * <p>
	 * This code may reference a chain of links, in that case the link codes are presented in the correct order, separated with dots (.).
	 * 
	 * @return
	 */
	public String getLinkCode();
	
	/**
	 * Get the split list of links code to use in this test.
	 *  
	 * @return a non null array of string, may be empty if the linkCode is not provided.
	 */
	public String[] getLinkCodes();
	
	/**
	 * Define the linkCode value.
	 * 
	 * <p>
	 * This code may reference a chain of links, in that case the link codes are presented in the correct order, separated with dots (.).
	 * 
	 * @param linkCode
	 */
	public void setLinkCode(String linkCode);

	/**
	 * If true all the Subdivision implied in the link chain, including the first and the last referenced entities, will be ignored, only direct link will be taken into account.
	 *  
	 * @return
	 */
	public boolean isIgnoreSubdivision();

	/**
	 * Define if the subdivisions implied in the link chain, including the first and the last referenced entities, must be ignored.
	 * 
	 * @param ignoreSubdivision
	 */
	public void setIgnoreSubdivision(boolean ignoreSubdivision);

	/**
	 * True if the deleted links implied in the link chain, and inner deleted entities items, are taken into account.
	 *  
	 * @return
	 */
	public boolean isDeletedLinks();

	/**
	 * Define if the deleted links implied in the link chain, and inner deleted entities items, should be taken into account or not.
	 * 
	 * @param deleted
	 */
	public void setDeletedLinks(boolean deleted);
}
