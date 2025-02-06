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

import org.osgi.framework.ServiceRegistration;
import org.osgi.service.event.Event;

import com.arcadsoftware.osgi.AbstractActivator;

/**
 * This class declare an Event broker that will run the abstract methods 
 * when the given entities are ready to be manipulated.
 * 
 * @author ARCAD Software
 */
public abstract class MetaDataEntityWaiter {

	private final AbstractActivator activator;
	private final String[] types;
	private final ServiceRegistration<?> ers;
	
	/**
	 * Declare a object which will wait for the declaration of the given Entities.
	 * @param activator
	 * @param type
	 */
	public MetaDataEntityWaiter(AbstractActivator activator, String... type) {
		super();
		this.activator = activator;
		types = type;
		int c = 0;
		for(int i = 0; i < types.length; i++) {
			final MetaDataEntity entity = MetaDataEntity.loadEntity(types[i]);
			if ((entity != null) && (entity.getMapper() != null)) {
				types[i] = null;
				c++;
				run(entity);
			}
		}
		if (c < types.length) {
			ers = activator.registerService(MetaDataEventHandler.clazz, new MetaDataEventHandler(activator) {
				public void handleEvent(Event event) {
					testEntities();
				}
			}, MetaDataEventHandler.getProperties(MetaDataEventHandler.TOPIC_ENTITY_CREATED, MetaDataEventHandler.TOPIC_MAPPER_CREATED));
		} else {
			runAll();
			ers = null;
		}
	}

	private synchronized void testEntities() {
		int c = 0;
		for(int i = 0; i < types.length; i++) {
			if (types[i] == null) {
				c++;
			} else {
				final MetaDataEntity entity = MetaDataEntity.loadEntity(types[i]);
				if ((entity != null) && (entity.getMapper() != null)) {
					types[i] = null;
					c++;
					run(entity);
				}
			}
		}
		if (c == types.length) {
			if (ers != null) {
				activator.unregister(ers);
			}
			runAll();
		}
	}
	
	public AbstractActivator getActivator() {
		return activator;
	}

	/**
	 * Called as soon as the Entity is ready to be used.
	 * @param entity
	 */
	public abstract void run(MetaDataEntity entity);
	
	/**
	 * Called when all the given Entities are ready to be used.
	 */
	public abstract void runAll();
}
