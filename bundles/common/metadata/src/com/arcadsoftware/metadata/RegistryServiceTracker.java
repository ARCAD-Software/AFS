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
package com.arcadsoftware.metadata;

import java.util.ArrayList;
import java.util.List;

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.util.tracker.ServiceTracker;
import org.osgi.util.tracker.ServiceTrackerCustomizer;

/**
 * Helper class that can be used to get Entity from registry services.
 * 
 * @author ARCAD Software
 *
 */
@SuppressWarnings("rawtypes")
public class RegistryServiceTracker extends ServiceTracker {

	@SuppressWarnings("unchecked")
	public RegistryServiceTracker(BundleContext context) {
		super(context, IEntityRegistry.clazz, null);
		open();
	}

	@SuppressWarnings("unchecked")
	public RegistryServiceTracker(BundleContext context, ServiceTrackerCustomizer customizer) {
		super(context, IEntityRegistry.clazz, customizer);
		open();
	}

	/** 
	 * Return first registry.
	 * 
	 * @return
	 */
	public IEntityRegistry getRegistry() {
		return (IEntityRegistry)getService();
	}
	
	/**
	 * Get a specific Entities registry service.
	 * 
	 * @param name a Registry name.
	 * @return a Entities registry service, null if none registry are associated this the given name.
	 */
	@SuppressWarnings("unchecked")
	public IEntityRegistry getRegistry(String name) {
		if (name == null) {
			return (IEntityRegistry)getService();
		}
		ServiceReference[] refs = getServiceReferences();
		if (refs != null) {
			for(ServiceReference ref: refs) {
				if (name.equals(ref.getProperty(IEntityRegistry.PROP_REGISTRYNAME))) {
					return (IEntityRegistry)getService(ref);
				}
			}
		}
		return null;
	}
	
	/**
	 * Return an Entity.
	 * 
	 * @param type
	 * @return
	 */
	public MetaDataEntity getEntity(String type) {
		ServiceReference[] refs = getServiceReferences();
		if (refs == null) {
			return null;
		}
		for (ServiceReference ref:refs) {
			@SuppressWarnings("unchecked")
			MetaDataEntity entity = ((IEntityRegistry)getService(ref)).getEntity(type);
			if (entity != null) {
				return entity;
			}
		}
		return null;
	}

	/**
	 * Return the list of the entities declared into the given domain.
	 * 
	 * @param domain
	 * @return
	 */
	public List<MetaDataEntity> getEntities(String domain) {
		ArrayList<MetaDataEntity> result = new ArrayList<MetaDataEntity>();
		ServiceReference[] refs = getServiceReferences();
		if (refs == null) {
			return result;
		}
		for (ServiceReference ref:refs) {
			@SuppressWarnings("unchecked")
			List<MetaDataEntity> entities = ((IEntityRegistry)getService(ref)).getEntities(domain);
			result.addAll(entities);
		}
		return result;
	}

	/**
	 * Return the list of all registered entities.
	 * 
	 * @param domain
	 * @return
	 */
	public List<MetaDataEntity> getEntities() {
		ArrayList<MetaDataEntity> result = new ArrayList<MetaDataEntity>();
		ServiceReference[] refs = getServiceReferences();
		if (refs == null) {
			return result;
		}
		for (ServiceReference ref:refs) {
			@SuppressWarnings("unchecked")
			List<MetaDataEntity> entities = ((IEntityRegistry)getService(ref)).getEntities();
			result.addAll(entities);
		}
		return result;
	}
	
	/**
	 * Return the first registry that own an entiy of the same type
	 * @param entity
	 * @return null if this entity as no type or if non registry own it.
	 */
	public IEntityRegistry getRegistry(MetaDataEntity entity) {
		ServiceReference[] refs = getServiceReferences();
		if ((refs == null) || (entity == null)) {
			return null;
		}
		String type = entity.getType();
		if ((type == null) || (type.length() == 0)) {
			return null;
		}
		for (ServiceReference ref:refs) {
			@SuppressWarnings("unchecked")
			IEntityRegistry registry = (IEntityRegistry)getService(ref);
			MetaDataEntity e = registry.getEntity(type);
			if (e != null) {
				return registry;
			}
		}
		return null;
	}
}
