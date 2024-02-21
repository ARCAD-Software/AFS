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

import java.util.Collection;

import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * This interface describe the Criteria Generation processor. The context
 * object is used during reduction processing to detect 
 *  
 * @see CriteriaContextBasic
 */
public interface ICriteriaContext {
	
	/**
	 * @return The reference entity type.
	 */
	public MetaDataEntity getEntity();

	/**
	 * Get an entity structure information.
	 * 
	 * @param type an entity type.
	 * @return the entity structure or null if does not exists.
	 */
	public MetaDataEntity getEntity(String type);

	/**
	 * 
	 * @return can return null.
	 */
	public IConnectionUserBean getCurrentUser();

	/**
	 * This method is called during reduce operation to store the computed reference line.
	 * 
	 * <p>
	 * The Context can use this method to prepare the query with necessary data.
	 *  
	 * @param reference
	 * @return
	 */
	public void useReference(ReferenceLine reference);
	
	/**
	 * This method is called during reduce operation to store the computed reference line relative to a linked entity.
	 * 
	 * <p>
	 * The Context can use this method to prepare the query with necessary data.
	 *  
	 * @param link
	 * @param reference this reference can be null if the criteria use only the link information.
	 */
	public void useLinkReference(MetaDataLink link, ReferenceLine reference);

	/**
	 * This method is used to determine if the domain of used attributes is unique.
	 * 
	 * @return true if an unique domain is used into this criteria.
	 */
	public boolean isMapperUnique();
	
	/**
	 * Get the corresponding reference line. this code has to be recorded first.
	 * 
	 * <p>
	 * Note that this method must be used, during or after the criteria reduction process.
	 * 
	 * @param code
	 * @return
	 */
	public ReferenceLine getReference(String code);
	
	/**
	 * Get the list of all unique references used into this Criteria context.
	 * 
	 * <p>
	 * Note that this method must be used, during or after the criteria reduction process.
	 * 
	 * @return
	 */
	public Collection<ReferenceLine> getReferences();
	
	/**
	 * Get the list of all used link into this criteria context.
	 * 
	 * <p>
	 * Note that this method must be used, during or after the criteria reduction process.
	 * 
	 * @return
	 */
	public Collection<MetaDataLink> getLinks();

	/**
	 * Get the references line recorded with this link.
	 * 
	 * <p>
	 * Note that this method must be used, during or after the criteria reduction process.
	 * 
	 * @param link
	 * @return
	 */
	public Collection<ReferenceLine> getReferences(MetaDataLink link);

	/**
	 * Get the recorded link reference.
	 * 
	 * <p>
	 * Note that this method must be used, during or after the criteria reduction process.
	 * 
	 * @param link
	 * @param code
	 * @return
	 */
	public ReferenceLine getReference(MetaDataLink link, String code);

}
