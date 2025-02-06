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
package com.arcadsoftware.metadata;

import java.util.List;

/**
 * This OSGi service ensure the centralized, coherent and persistent (local) store of BeanMap Entities definitions.
 * 
 * <p>
 * This assume that this service is able to load Entities definitions on request, or when they appear. 
 *
 */
public interface IEntityRegistry {
	
	/**
	 * The OSGi service class identification.
	 */
	public static final String clazz = IEntityRegistry.class.getName();
	
	/**
	 * OSGi Service property identify this Entity Registry.
	 */
	public static final String PROP_REGISTRYNAME = "registry.name"; //$NON-NLS-1$
	
	/**
	 * Return the Entity definition associated to this type name.
	 * 
	 * @param type an unique type name.
	 * @return can return null if the entity is not found into the repository.
	 */
	public MetaDataEntity getEntity(String type);

	/**
	 * Return the list of all the entities from de given domain name.
	 * 
	 * @param domain
	 * @return
	 */
	public List<MetaDataEntity> getEntities(String domain);
	
	/**
	 * Return the list of all registered the entities.
	 * 
	 * <p>
	 * This method may be time consuming.
	 * 
	 * @return
	 */
	public List<MetaDataEntity> getEntities();
	
	/**
	 * Patch and existing entity.
	 * 
	 * <p>
	 * If the entity does not currently exist the Registry record the modification for subsequent use.
	 * 
	 * <p>
	 * Patches does not update the Entity version level. Nevertheless, if the patch level is lower than the current Entity version level then it is ignored.
	 * 
	 * @param entityPatch
	 * @return
	 */
	public MetaDataEntity updateEntity(MetaDataEntity entityPatch);

	/**
	 * Declare a new entity.
	 * 
	 * <p>If this entity is already declared then it is ignored unless the new version level is higher than the current entity.
	 * In this case the current entity is destroyed and replaced with the new one.
	 * 
	 * @param entity
	 * @return
	 */
	public MetaDataEntity addEntity(MetaDataEntity entity);
	
	/**
	 * Remove an entity. 
	 * 
	 * <p>
	 * If the current entity version level is higher than the removed one then the operation is ignored.
	 * 
	 * <p>
	 * Just entity type (or id) and version level are required into the removed object.
	 *  
	 * @param entity
	 * @return
	 */
	public MetaDataEntity removeEntity(MetaDataEntity entity);
}
