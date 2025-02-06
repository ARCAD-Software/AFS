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

import org.osgi.service.event.Event;

import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.osgi.AbstractActivator;
import com.arcadsoftware.beanmap.BeanMap;

/**
 * This utility class define an abstract Event handler linked to metadata Web-services events.
 *  
 * <p>This Event Handler will be automatically registered as an OSGi service. You do not need to keep a pointer on this object. Basically, to
 * create and handler you just need to implement this class and put a <i>new</i> into the start method of your activator.
 * 
 * @see org.osgi.service.event.EventHandler
 */
public abstract class BeanMapEventHandler extends MetaDataEventHandler {

	private String trackedType;
	
	/**
	 * Create the Dispatcher service.
	 * 
	 * <p>
	 * Automatically register this EventHandler as an OSGi service.
	 * 
	 * @param activator the parent activator.
	 * @param topics optional Event topics to filter.
	 */
	public BeanMapEventHandler(AbstractActivator activator, String... topics) {
		super(activator);
		activator.registerService(clazz, this, getProperties(topics));
	}

	public BeanMapEventHandler(String trackedType, AbstractActivator activator, String... topics) {
		this(activator, topics);
		setTrackedType(trackedType);
	}

	/* 
	 * Process the OSGi event and retrieve the parameters.
	 */
	public final void handleEvent(Event event) {
		// Topic court.
		String topic = event.getTopic();
		//topic = topic.substring(topic.lastIndexOf('/') + 1, topic.length());
		if ((topic == null) || !topic.startsWith(PREFIX_TOPIC_BEANMAP)) {
			return;
		}
		BeanMap bean = getBeanMap(event);
		if (bean == null) {
			return;
		}
		if ((trackedType != null) && !trackedType.equals(bean.getType())) {
			return;
		}
		IConnectionUserBean user = getUser(event);
		// Dispatch the Event !
		if (TOPIC_BEANMAP_LINKEDTO.equals(topic) || 
				TOPIC_BEANMAP_UNLINKEDTO.equals(topic)) {
			try {
				BeanMap xBean = getAuxiliaryBeanMap(event);
				if (xBean == null) {
					return;
				}
				dispatchLink(event, topic, user, bean, xBean);
			} catch (Throwable e) {
				getActivator().error(Messages.BeanMapEventHandler_Log_Dispatch_Error, e);
			}
		} else {
			BeanMap oldBean = getOldBeanMap(event);
			try {
				dispatch(event, topic, user, oldBean, bean);
			} catch (Throwable e) {
				getActivator().error(Messages.BeanMapEventHandler_Log_Dispatch_Error, e);
			}
		}
	}

	/**
	 * Dispatch the fired event.
	 * 
	 * @param topic The OSGi Event Topic other than TOPIC_BEANMAP_LINKEDTO or TOPIC_BEANMAP_UNLINKEDTO.
	 * @param user The User that has issued the modification, may be null.
	 * @param oldBean The object that has been modified, as it were just an instant before the modification. If the event is due to a creation then this object is null.
	 * @param changedBean The list of modified attributes.
	 */
	protected abstract void dispatch(Event event, String topic, IConnectionUserBean user, BeanMap oldBean, BeanMap changedBean);

	/**
	 * Dispatch the fired event.
	 * 
	 * <p>
	 * this method is called for "linking" event, between two elements, this only concern the 
	 * topic TOPIC_BEANMAP_LINKEDTO or TOPIC_BEANMAP_UNLINKEDTO. 
	 * 
	 * @param topic The OSGi Event Topic: TOPIC_BEANMAP_LINKEDTO or TOPIC_BEANMAP_UNLINKEDTO.
	 * @param user The User that has issued the modification, may be null.
	 * @param firstBean The first element implied into the link.
	 * @param secondBean The second one.
	 */
	protected abstract void dispatchLink(Event event, String topic, IConnectionUserBean user, BeanMap firstBean, BeanMap secondBean);

	/**
	 * Define the current type that is trapped by this dispatcher. There can only be one type at a time to be tracked, but
	 * this type may change.
	 * 
	 * <p>
	 * If this type is set to null then any Entity modification is trapped.
	 * 
	 * @param type the Entity type to be tracked.
	 */
	public void setTrackedType(String type) {
		this.trackedType = type;
	}

	/**
	 * @return the type the is currently tracked, null if all types are tracked.
	 */
	public String getTrackedType() {
		return trackedType;
	}

	/**
	 * Test if the types of the two beanmap matches the given types (in any order).
	 * 
	 * @param firstBean
	 * @param secondBean
	 * @param type1
	 * @param type2
	 * @return
	 */
	protected boolean typesEquals(BeanMap firstBean, BeanMap secondBean, String type1, String type2) {
		if ((firstBean == null) || (secondBean == null)) {
			return false;
		}
		if (type1.equals(firstBean.getType())) {
			return type2.equals(secondBean.getType());
		}
		return type2.equals(firstBean.getType()) && type1.equals(secondBean.getType());
	}
	
}
