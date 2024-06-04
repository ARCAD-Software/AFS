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

import java.util.Dictionary;
import java.util.Hashtable;
import java.util.Properties;

import org.osgi.framework.BundleContext;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;
import org.osgi.util.tracker.ServiceTracker;

import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.metadata.internal.FakeConnectionUserBean;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

/**
 * Service tracker used to fire events about BeanMaps modifications.
 */
public class BeanMapEventTracker extends ServiceTracker<EventAdmin, EventAdmin> {

	/**
	 * Construct the Service EventAdmin tracker and automatically open it.
	 * 
	 * @param context
	 */
	public BeanMapEventTracker(BundleContext context) {
		super(context, EventAdmin.class, null);
		open();
	}
	
	/**
	 * Fire an asynchronous BeanMap Event.
	 * 
	 * @param topic The type of the proceed operation.
	 * @param type The Entity Type.
	 * @param id The item Id.
	 * @param userId The user Id.
	 * @return true if the event is fired.
	 */
	public boolean fireEvent(String topic, String type, int id, int userId) {
		return fireEvent(topic, new BeanMap(type,id), new FakeConnectionUserBean(userId));
	}
	
	/**
	 * Fire an asynchronous BeanMap Event.
	 * 
	 * @param topic The type of the proceed operation.
	 * @param type The Entity Type.
	 * @param id The item Id.
	 * @param userId The user Id.
	 * @return true if the event is fired.
	 */
	public boolean fireEvent(String topic, BeanMap bean, int userId) {
		return fireEvent(topic, bean, new FakeConnectionUserBean(userId));
	}

	/**
	 * Fire an asynchronous BeanMap Event.
	 * 
	 * @param topic
	 * @param type
	 * @param id
	 * @param user
	 * @return
	 */
	public boolean fireEvent(String topic, String type, int id, IConnectionUserBean user) {
		return fireEvent(topic, new BeanMap(type,id), user);
	}
	
	/**
	 * Fire an asynchronous BeanMap Event.
	 * 
	 * @param topic The type of the proceed operation.
	 * @param bean The BeanMap impacted.
	 * @param user The user that have performed the operation.
	 * @return true if the event is fired.
	 */
	public boolean fireEvent(String topic, BeanMap bean, IConnectionUserBean user) {
		EventAdmin ea = (EventAdmin) getService();
		if (ea != null) {
			Dictionary<String, Object> properties = new Hashtable<String, Object>();
			if (bean != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_BEANMAP, bean);
			}
			if (user != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_USER, user);
			}
			try {
				ea.postEvent(new Event(topic, properties));
				return true;
			} catch (SecurityException e) {}
		}
		return false;
	}

	/**
	 * Fire an assynchronous event.
	 * 
	 * @param topic
	 * @param properties
	 * @return
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public boolean fireEvent(String topic, Properties properties) {
		EventAdmin ea = (EventAdmin) getService();
		if (ea != null) {
			try {
				ea.postEvent(new Event(topic, (Dictionary) properties));
				return true;
			} catch (SecurityException e) {}
		}
		return false;
	}

	/**
	 * Fire an assynchronous event.
	 * 
	 * @param topic
	 * @param properties
	 * @return
	 */
	public boolean fireEvent(String topic, Dictionary<String, Object> properties) {
		EventAdmin ea = (EventAdmin) getService();
		if (ea != null) {
			try {
				ea.postEvent(new Event(topic, properties));
				return true;
			} catch (SecurityException e) {}
		}
		return false;
	}

	/**
	 * Fire an synchronous event.
	 * 
	 * @param topic
	 * @param properties
	 * @return
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public boolean fireSyncEvent(String topic,Properties properties) {
		EventAdmin ea = (EventAdmin)getService();
		if (ea != null) {
			try {
				ea.sendEvent(new Event(topic, (Dictionary)properties));
				return true;
			} catch (SecurityException e) {}
		}
		return false;
	}

	/**
	 * Fire an asynchronous BeanMap Event.
	 * 
	 * @param topic
	 * @param type
	 * @param id
	 * @param subType
	 * @param subId
	 * @param userId
	 */
	public boolean fireEvent(String topic, String type, int id, String subType, int subId, int userId) {
		return fireEvent(topic, null, new BeanMap(type,id), new BeanMap(subType,subId), new FakeConnectionUserBean(userId));
	}

	/**
	 * Fire an asynchronous BeanMap Event.
	 * 
	 * @param topic
	 * @param bean
	 * @param subBean
	 * @param userId
	 */
	public boolean fireEvent(String topic, BeanMap bean, BeanMap subBean, int userId) {
		return fireEvent(topic, null, bean, subBean, new FakeConnectionUserBean(userId));
	}
	
	/**
	 * Fire an asynchronous BeanMap Event.
	 * 
	 * @param topic
	 * @param entity
	 * @param bean
	 * @param user
	 * @return
	 */
	public boolean fireEvent(String topic, MetaDataEntity entity, BeanMap bean, IConnectionUserBean user) {
		return fireEvent(topic, entity, bean, null, user);
	}
	
	/**
	 * Fire an asynchronous BeanMap Event.
	 * 
	 * @param topic The Event Topic 
	 * @param entity
	 * @param bean
	 * @param user
	 * @param prop
	 * @param value
	 * @return
	 */
	public boolean fireEvent(String topic, MetaDataEntity entity, BeanMap bean, IConnectionUserBean user, String prop, Object value) {
		return fireEvent(topic, entity, bean, null, user, prop, value);
	}

	/**
	 * Fire an asynchronous BeanMap Event.
	 * 
	 * @param topic
	 * @param bean
	 * @param subBean
	 * @param user
	 * @return
	 */
	public boolean fireEvent(String topic, MetaDataEntity entity, BeanMap bean, BeanMap subBean, IConnectionUserBean user) {
		return fireEvent(topic, entity, bean, subBean, user, null, null);
	}
	/**
	 * Fire an asynchronous BeanMap Event.
	 * 
	 * @param topic
	 * @param bean
	 * @param subBean
	 * @param user
	 * @return
	 */
	public boolean fireEvent(String topic, MetaDataEntity entity, BeanMap bean, BeanMap subBean, IConnectionUserBean user, String prop, Object value) {
		EventAdmin ea = (EventAdmin)getService();
		if (ea != null) {
			Dictionary<String, Object> properties = new Hashtable<String, Object>();
			if (entity != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_ENTITY, entity);
				properties.put(MetaDataEventHandler.EVENT_PROP_TYPE, entity.getType());
			}
			if (bean != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_BEANMAP, bean);
				if (bean.getType() != null) {
					properties.put(MetaDataEventHandler.EVENT_PROP_TYPE, bean.getType());
				}
			}
			if (subBean != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_SUBBEANMAP, subBean);
			}
			if (user != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_USER, user);
			}
			if (prop != null) {
				properties.put(prop, value);
			}
			try {
				ea.postEvent(new Event(topic, properties));
				return true;
			} catch (SecurityException e) {
				Activator.getInstance().error(Messages.BeanMapEventTracker_Post_Error, e);
			}
		}
		return false;
	}

	public boolean fireEvent(String topic, MetaDataEntity entity, MetaDataLink link, BeanMap bean, BeanMap subBean, IConnectionUserBean user) {
		EventAdmin ea = (EventAdmin)getService();
		if (ea != null) {
			Dictionary<String, Object> properties = new Hashtable<String, Object>();
			if (entity != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_ENTITY, entity);
				properties.put(MetaDataEventHandler.EVENT_PROP_TYPE, entity.getType());
			}
			if (entity != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_LINK, link);
			}
			if (bean != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_BEANMAP, bean);
				if (bean.getType() != null) {
					properties.put(MetaDataEventHandler.EVENT_PROP_TYPE, bean.getType());
				}
			}
			if (subBean != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_SUBBEANMAP, subBean);
			}
			if (user != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_USER, user);
			}
			try {
				ea.postEvent(new Event(topic, properties));
				return true;
			} catch (SecurityException e) {
				Activator.getInstance().error(Messages.BeanMapEventTracker_Post_Error, e);
			}
		}
		return false;
	}

	public boolean fireEventoldValue(String topic, MetaDataEntity entity, BeanMap oldBean, BeanMap newBean, IConnectionUserBean user) {
		EventAdmin ea = (EventAdmin)getService();
		if (ea != null) {
			Dictionary<String, Object> properties = new Hashtable<String, Object>();
			if (entity != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_ENTITY, entity);
				properties.put(MetaDataEventHandler.EVENT_PROP_TYPE, entity.getType());
			}
			if (newBean != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_BEANMAP, newBean);
			}
			if (oldBean != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_OLDBEANMAP, oldBean);
			}
			if (user != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_USER, user);
			}
			try {
				ea.postEvent(new Event(topic, properties));
				return true;
			} catch (SecurityException e) {
				Activator.getInstance().error(Messages.BeanMapEventTracker_Post_Error, e);
			}
		}
		return false;
	}

	public boolean fireEvent(String topic, MetaDataEntity entity, BeanMapList list, IConnectionUserBean user) {
		EventAdmin ea = (EventAdmin) getService();
		if (ea != null) {
			Dictionary<String, Object> properties = new Hashtable<String, Object>();
			if (list != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_BEANMAPLIST, list);
			}
			if (entity != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_ENTITY, entity);
				properties.put(MetaDataEventHandler.EVENT_PROP_TYPE, entity.getType());
			}
			if (user != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_USER, user);
			}
			try {
				ea.postEvent(new Event(topic, properties));
				return true;
			} catch (SecurityException e) {
				Activator.getInstance().error(Messages.BeanMapEventTracker_Post_Error, e);
			}
		}
		return false;
	}
	
}
