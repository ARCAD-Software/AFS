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

import java.util.Dictionary;
import java.util.Hashtable;

import org.osgi.service.event.Event;
import org.osgi.service.event.EventConstants;
import org.osgi.service.event.EventHandler;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.osgi.AbstractActivator;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Facility class to handle Metadata related events.
 * 
 * @author ARCAD Software
 *
 */
public abstract class MetaDataEventHandler implements EventHandler {
	
	public static final String PREFIX_TOPIC = "com/arcadsoftware/metadata/"; //$NON-NLS-1$
	public static final String PREFIX_TOPIC_MAPPER = PREFIX_TOPIC + "mapper/"; //$NON-NLS-1$
	public static final String PREFIX_TOPIC_REGISTRY = PREFIX_TOPIC + "registry/"; //$NON-NLS-1$
	public static final String PREFIX_TOPIC_ENTITY = PREFIX_TOPIC + "entity/"; //$NON-NLS-1$
	public static final String PREFIX_TOPIC_BEANMAP = "com/arcadsoftware/beanmap/"; //$NON-NLS-1$
	public static final String PREFIX_TOPIC_BPM = "com/arcadsoftware/bpm/"; //$NON-NLS-1$
	
	public static final String TOPIC_ALLEVENTS = PREFIX_TOPIC + "*"; //$NON-NLS-1$

	public static final String TOPIC_MAPPER_CREATED = PREFIX_TOPIC_MAPPER + "create"; //$NON-NLS-1$
	public static final String TOPIC_MAPPER_DESTROYED = PREFIX_TOPIC_MAPPER + "destroy"; //$NON-NLS-1$
	public static final String TOPIC_MAPPER_ALLEVENTS = PREFIX_TOPIC_MAPPER + "*"; //$NON-NLS-1$

	public static final String TOPIC_REGISTRY_CREATED = PREFIX_TOPIC_REGISTRY + "create"; //$NON-NLS-1$
	public static final String TOPIC_REGISTRY_DESTROYED = PREFIX_TOPIC_REGISTRY + "destroy"; //$NON-NLS-1$
	public static final String TOPIC_REGISTRY_ALLEVENTS = PREFIX_TOPIC_REGISTRY + "*"; //$NON-NLS-1$
	
	public static final String TOPIC_ENTITY_CREATED = PREFIX_TOPIC_ENTITY + "create"; //$NON-NLS-1$
	public static final String TOPIC_ENTITY_DESTROYED = PREFIX_TOPIC_ENTITY + "destroy"; //$NON-NLS-1$
	public static final String TOPIC_ENTITY_MODIFIED = PREFIX_TOPIC_ENTITY + "change"; //$NON-NLS-1$
	public static final String TOPIC_ENTITY_ATTRIBUTE_CREATED = PREFIX_TOPIC_ENTITY + "attribute/create"; //$NON-NLS-1$
	public static final String TOPIC_ENTITY_ATTRIBUTE_DESTROYED = PREFIX_TOPIC_ENTITY + "attribute/destroy"; //$NON-NLS-1$
	public static final String TOPIC_ENTITY_ATTRIBUTE_MODIFIED = PREFIX_TOPIC_ENTITY + "attribute/change"; //$NON-NLS-1$
	public static final String TOPIC_ENTITY_ATTRIBUTE_ALLEVENTS = PREFIX_TOPIC_ENTITY + "attribute/*"; //$NON-NLS-1$
	public static final String TOPIC_ENTITY_LINK_CREATED = PREFIX_TOPIC_ENTITY + "link/create"; //$NON-NLS-1$
	public static final String TOPIC_ENTITY_LINK_DESTROYED = PREFIX_TOPIC_ENTITY + "link/destroy"; //$NON-NLS-1$
	public static final String TOPIC_ENTITY_LINK_MODIFIED = PREFIX_TOPIC_ENTITY + "link/change"; //$NON-NLS-1$
	public static final String TOPIC_ENTITY_LINK_ALLEVENTS = PREFIX_TOPIC_ENTITY + "link/*"; //$NON-NLS-1$
	public static final String TOPIC_ENTITY_ALLEVENTS = PREFIX_TOPIC_ENTITY + "*"; //$NON-NLS-1$

	public static final String TOPIC_BEANMAP_SELECTED = PREFIX_TOPIC_BEANMAP + "selected"; //$NON-NLS-1$
	public static final String TOPIC_BEANMAP_READ = PREFIX_TOPIC_BEANMAP + "read"; //$NON-NLS-1$
	public static final String TOPIC_BEANMAP_CREATED = PREFIX_TOPIC_BEANMAP + "create"; //$NON-NLS-1$
	public static final String TOPIC_BEANMAP_MODIFIED = PREFIX_TOPIC_BEANMAP + "change"; //$NON-NLS-1$
	public static final String TOPIC_BEANMAP_DELETED = PREFIX_TOPIC_BEANMAP + "delete"; //$NON-NLS-1$
	public static final String TOPIC_BEANMAP_LINKEDTO = PREFIX_TOPIC_BEANMAP + "link"; //$NON-NLS-1$
	public static final String TOPIC_BEANMAP_UNLINKEDTO = PREFIX_TOPIC_BEANMAP + "unlink"; //$NON-NLS-1$
	public static final String TOPIC_BEANMAP_UNDELETED = PREFIX_TOPIC_BEANMAP + "undelete"; //$NON-NLS-1$
	public static final String TOPIC_BEANMAP_LOCKED = PREFIX_TOPIC_BEANMAP + "lock"; //$NON-NLS-1$
	public static final String TOPIC_BEANMAP_UNLOCKED = PREFIX_TOPIC_BEANMAP + "unlock"; //$NON-NLS-1$
	public static final String TOPIC_BEANMAP_ALLEVENTS = PREFIX_TOPIC_BEANMAP + "*"; //$NON-NLS-1$

	public static final String TOPIC_BPM_CREATED = PREFIX_TOPIC_BPM + "created"; //$NON-NLS-1$
	public static final String TOPIC_BPM_TRANSITION = PREFIX_TOPIC_BPM + "transition"; //$NON-NLS-1$
	public static final String TOPIC_BPM_ALLEVENTS = PREFIX_TOPIC_BPM + "*"; //$NON-NLS-1$
	
	public static final String EVENT_PROP_BEANMAP = "beanmap"; //$NON-NLS-1$
	public static final String EVENT_PROP_BEANMAPLIST = "beanmaplist"; //$NON-NLS-1$
	public static final String EVENT_PROP_OLDBEANMAP = "oldbeanmap"; //$NON-NLS-1$
	public static final String EVENT_PROP_SUBBEANMAP = "subbeanmap"; //$NON-NLS-1$
	public static final String EVENT_PROP_USER = "user"; //$NON-NLS-1$
	public static final String EVENT_PROP_ENTITY = "entity"; //$NON-NLS-1$
	public static final String EVENT_PROP_REGISTRY = "registry"; //$NON-NLS-1$
	public static final String EVENT_PROP_MAPPER = "mapper"; //$NON-NLS-1$
	public static final String EVENT_PROP_BPMENGINE = "bpm.engine"; //$NON-NLS-1$
	public static final String EVENT_PROP_LINK = "link"; //$NON-NLS-1$
	public static final String EVENT_PROP_TYPE = "type"; //$NON-NLS-1$
	public static final String EVENT_PROP_CODES = "codes"; //$NON-NLS-1$
	public static final String EVENT_PROP_DOMAINS = "domain"; //$NON-NLS-1$
	public static final String EVENT_PROP_HARDDELETE = "harddelete"; //$NON-NLS-1$

	/**
	 * The Service interface code.
	 */
	public static final String clazz = EventHandler.class.getName();
	
	/**
	 * Set the topics sub set linked to this dispatcher. 
	 * 
	 * <p>By default you must use this method with a null parameter</p>
	 * 
	 * @param topics the associated topics (defined into the <code>BeanMapEventHandler</code> class.
	 * @return A Properties object with correct topics.
	 * @see org.osgi.service.event.EventHandler
	 */
	public static Dictionary<String, Object> getProperties(String... topics) {
		Hashtable<String, Object> prop = new Hashtable<String, Object>();
		if ((topics == null) || (topics.length == 0)) {
			prop.put(EventConstants.EVENT_TOPIC, new String[]{TOPIC_ALLEVENTS}); //$NON-NLS-1$
		} else {
			prop.put(EventConstants.EVENT_TOPIC, topics);
		}
		return prop;
	}
	
	private AbstractActivator activator;
	
	/**
	 * Create the Dispatcher service.
	 * 
	 * @param activator an activator with logging capabilities.
	 */
	public MetaDataEventHandler(AbstractActivator activator) {
		super();
		this.activator = activator;
	}

	/**
	 * Get the bundle Activator object used to create this object.
	 * 
	 * @return the activator
	 */
	public AbstractActivator getActivator() {
		return activator;
	}

	/**
	 * Return the first Mapper associated to the given Domain name.
	 * 
	 * @param domainName
	 * @return
	 */
	public IMapperService getMapper(String domainName) {
		return Activator.getInstance().getBeanMapper(domainName);
	}

	/**
	 * Return the BeanMap event property.
	 * @param event
	 * @return
	 */
	protected BeanMap getBeanMap(Event event) {
		Object o = event.getProperty(EVENT_PROP_BEANMAP);
		if (o instanceof BeanMap) {
			return (BeanMap)o;
		}
		getActivator().error(Messages.BeanMapEventHandler_Log_No_BeanMap);
		return null;
	}

	/**
	 * Return the selected list of BeanMap.
	 * <p>Only accessible through <code>TOPIC_BEANMAP_SELECTED</code> event.
	 * 
	 * @param event
	 * @return The selected list of beanmaps may return null if an error occurs.
	 */
	protected BeanMapList getBeanMapList(Event event) {
		Object o = event.getProperty(EVENT_PROP_BEANMAPLIST);
		if (o instanceof BeanMapList) {
			return (BeanMapList)o;
		}
		getActivator().error(Messages.MetaDataEventHandler_Error_SelectionEventWithOutSelection);
		return null;
	}

	/**
	 * Return the BeanMap event property.
	 * @param event
	 * @return
	 */
	protected BeanMap getOldBeanMap(Event event) {
		Object o = event.getProperty(EVENT_PROP_OLDBEANMAP);
		if (o instanceof BeanMap) {
			return (BeanMap) o;
		}
		getActivator().debug(Messages.BeanMapEventHandler_Log_No_BeanMap);
		return null;
	}

	/**
	 * Return the User event property if any.
	 * @param event
	 * @return null if non user is associated to this event.
	 */
	protected IConnectionUserBean getUser(Event event) {
		Object o = event.getProperty(EVENT_PROP_USER);
		if (o instanceof IConnectionUserBean) {
			return (IConnectionUserBean)o;
		}
		getActivator().debug(Messages.BeanMapEventHandler_Log_No_User);
		return null;
	}
	
	protected IMapperService getMapper(Event event) {
		Object o = event.getProperty(EVENT_PROP_MAPPER);
		if (o instanceof IMapperService) {
			return (IMapperService)o;
		}
		MetaDataEntity entity = getEntity(event);
		if (entity != null) {
			return entity.getMapper();
		}
		getActivator().debug(Messages.MetaDataEventHandler_Debug_NoMapper);
		return null;
	}
	
	protected BeanMap getAuxiliaryBeanMap(Event event) {
		Object o = event.getProperty(EVENT_PROP_SUBBEANMAP);
		if (o instanceof BeanMap) {
			return (BeanMap)o;
		}
		getActivator().error(Messages.BeanMapEventHandler_Log_No_second_BeanMap);
		return null;
	}
	
	protected MetaDataEntity getEntity(Event event) {
		Object o = event.getProperty(EVENT_PROP_ENTITY);
		if (o instanceof MetaDataEntity) {
			return (MetaDataEntity)o;
		}
		getActivator().debug(Messages.MetaDataEventHandler_Debug_NoEntity);
		return null;
	}
	
	protected IEntityRegistry getRegistry(Event event) {
		Object o = event.getProperty(EVENT_PROP_REGISTRY);
		if (o instanceof IEntityRegistry) {
			return (IEntityRegistry)o;
		}
		MetaDataEntity entity = getEntity(event);
		if (entity != null) {
			return entity.getRegistry();
		}
		getActivator().debug(Messages.MetaDataEventHandler_Debug_NoRegistry);
		return null;
	}
}
