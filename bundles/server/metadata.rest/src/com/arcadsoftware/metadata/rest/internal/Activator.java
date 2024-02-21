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
package com.arcadsoftware.metadata.rest.internal;

import java.util.ArrayList;
import java.util.Date;
import java.util.Dictionary;
import java.util.List;

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.util.tracker.ServiceTracker;
import org.restlet.data.Language;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.BeanMapEventTracker;
import com.arcadsoftware.metadata.IEntityTesterService;
import com.arcadsoftware.metadata.IMetaDataDeleteListener;
import com.arcadsoftware.metadata.IMetaDataLinkingListener;
import com.arcadsoftware.metadata.IMetaDataModifyListener;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataEventHandler;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.osgi.AbstractActivator;
import com.arcadsoftware.rest.MultiLanguageMessages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public class Activator extends AbstractActivator implements IEntityTesterService {

	// Metadata utilisé pour enregistrer les dates de modifications des listes.
	private static final String METADATA_LISTCHANGEDATE = "listChangeDate";

	//Droit d'administration des entités.
	public static final int METADATA_ADMINRIGHT_CRT = 150;
	public static final int METADATA_ADMINRIGHT_EDT = 151;
	
	private static Activator instance;
	private static MultiLanguageMessages messages = new MultiLanguageMessages("com.arcadsoftware.metadata.rest.internal.usermessages",Activator.class.getClassLoader()); //$NON-NLS-1$

	public static Activator getInstance() {
		return instance;
	}

	public static String getMessage(String key, Language language) {
		return messages.get(key, language);
	}

	private BeanMapEventTracker eventTracker;
	private ServiceTracker<IMetaDataModifyListener, Object> modifyTracker;
	private ServiceTracker<IMetaDataDeleteListener, Object> deleteTracker;
	private ServiceTracker<IMetaDataLinkingListener, Object> linkingTracker;

	@Override
	public void start(BundleContext bundleContext) throws Exception {
		instance = this;
		super.start(bundleContext);
		registerService(Branch.clazz, new Branch(), Branch.URI, Branch.SECUREDBRANCH);
		eventTracker = new BeanMapEventTracker(bundleContext);
		modifyTracker = new ServiceTracker<IMetaDataModifyListener, Object>(bundleContext, IMetaDataModifyListener.clazz, null);
		modifyTracker.open();
		deleteTracker = new ServiceTracker<IMetaDataDeleteListener, Object>(bundleContext, IMetaDataDeleteListener.clazz, null);
		deleteTracker.open();
		linkingTracker = new ServiceTracker<IMetaDataLinkingListener, Object>(bundleContext, IMetaDataLinkingListener.clazz, null);
		linkingTracker.open();
	}

	@Override
	public void stop(BundleContext bundleContext) throws Exception {
		super.stop(bundleContext);
		eventTracker.close();
		eventTracker = null;
		modifyTracker.close();
		modifyTracker = null;
		deleteTracker.close();
		deleteTracker = null;
		linkingTracker.close();
		linkingTracker = null;
		instance = null;
	}

	public List<IMetaDataModifyListener> getModifyListener(String type) {
		ArrayList<IMetaDataModifyListener> result = new ArrayList<IMetaDataModifyListener>();
		ServiceReference<IMetaDataModifyListener>[] refs = modifyTracker.getServiceReferences();
		if (refs != null) {
			for(ServiceReference<IMetaDataModifyListener> ref: refs) {
				Object o = ref.getProperty(IMetaDataModifyListener.PROP_TYPE);
				if ((o == null) || (type == null) || type.equals(o)) {
					result.add((IMetaDataModifyListener) modifyTracker.getService(ref));
				}
			}
		}
		return result;
	}

	public List<IMetaDataDeleteListener> getDeleteListener(String type) {
		ArrayList<IMetaDataDeleteListener> result = new ArrayList<IMetaDataDeleteListener>();
		ServiceReference<IMetaDataDeleteListener>[] refs = deleteTracker.getServiceReferences();
		if (refs != null) {
			for(ServiceReference<IMetaDataDeleteListener> ref:refs) {
				Object o = ref.getProperty(IMetaDataDeleteListener.PROP_TYPE);
				if ((o == null) || (type == null) || type.equals(o)) {
					result.add((IMetaDataDeleteListener) deleteTracker.getService(ref));
				}
			}
		}
		return result;
	}

	public List<IMetaDataLinkingListener> getLinkingListener(MetaDataLink link) {
		ArrayList<IMetaDataLinkingListener> result = new ArrayList<IMetaDataLinkingListener>();
		ServiceReference<IMetaDataLinkingListener>[] refs = linkingTracker.getServiceReferences();
		if (refs != null) {
			for(ServiceReference<IMetaDataLinkingListener> ref:refs) {
				Object o = ref.getProperty(IMetaDataLinkingListener.PROP_TYPE);
				if ((o == null) || link.getParent().getType().equals(o)) {
					o = ref.getProperty(IMetaDataLinkingListener.PROP_LINK);
					if ((o == null) || link.getCode().equals(o)) {
						result.add((IMetaDataLinkingListener) linkingTracker.getService(ref));
					}
				}
			}
		}
		return result;
	}

	public void fireSelectionEvent(MetaDataEntity entity, BeanMapList list, IConnectionUserBean user) {
		if (eventTracker != null) {
			if (!eventTracker.fireEvent(MetaDataEventHandler.TOPIC_BEANMAP_SELECTED, entity, list, user)) {
				debug(String.format(Messages.Activator_Debug_EventNotFired, MetaDataEventHandler.TOPIC_BEANMAP_SELECTED, user.toString(), list.toString()));
			}
		}
	}

	public void fireCreateEvent(MetaDataEntity entity, BeanMap bean, IConnectionUserBean user) {
		if (eventTracker != null) {
			if (!eventTracker.fireEvent(MetaDataEventHandler.TOPIC_BEANMAP_CREATED, entity, bean, user)) {
				debug(String.format(Messages.Activator_Debug_EventNotFired, MetaDataEventHandler.TOPIC_BEANMAP_CREATED, user.toString(), bean.toString()));
			}
		}
	}

	public void fireDeleteEvent(MetaDataEntity entity, BeanMap bean, IConnectionUserBean user) {
		if (eventTracker != null) {
			if (!eventTracker.fireEvent(MetaDataEventHandler.TOPIC_BEANMAP_DELETED, entity, bean, user)) {
				debug(String.format(Messages.Activator_Debug_EventNotFired, MetaDataEventHandler.TOPIC_BEANMAP_DELETED, user.toString(), bean.toString()));
			}
		}
	}

	public void fireUpdateEvent(MetaDataEntity entity, BeanMap oldValue, BeanMap bean, IConnectionUserBean user) {
		if (eventTracker != null) {
			if (!eventTracker.fireEventoldValue(MetaDataEventHandler.TOPIC_BEANMAP_MODIFIED, entity, oldValue, bean, user)) {
				debug(String.format(Messages.Activator_Debug_EventNotFired, MetaDataEventHandler.TOPIC_BEANMAP_MODIFIED, user.toString(), bean.toString()));
			}
		}
	}

	public void fireUndeleteEvent(MetaDataEntity entity, BeanMap bean, IConnectionUserBean user) {
		if (eventTracker != null) {
			if (!eventTracker.fireEvent(MetaDataEventHandler.TOPIC_BEANMAP_UNDELETED, entity, bean, user)) {
				debug(String.format(Messages.Activator_Debug_EventNotFired, MetaDataEventHandler.TOPIC_BEANMAP_UNDELETED, user.toString(), bean.toString()));
			}
		}
	}

	public void fireUnlinkEvent(MetaDataEntity entity, MetaDataLink link, BeanMap item, BeanMap linked, IConnectionUserBean user) {
		if (eventTracker != null) {
			if (!eventTracker.fireEvent(MetaDataEventHandler.TOPIC_BEANMAP_UNLINKEDTO, entity, link, item, linked, user)) {
				debug(String.format(Messages.Activator_Debug_EventNotFired, MetaDataEventHandler.TOPIC_BEANMAP_UNLINKEDTO, user.toString(), item.toString() + " -> " + linked.toString())); //$NON-NLS-1$
			}
		}
	}

	public void fireLinkEvent(MetaDataEntity entity, MetaDataLink link, BeanMap item, BeanMap linked, IConnectionUserBean user) {
		if (eventTracker != null) {
			if (!eventTracker.fireEvent(MetaDataEventHandler.TOPIC_BEANMAP_LINKEDTO, entity, link, item, linked, user)) {
				debug(String.format(Messages.Activator_Debug_EventNotFired, MetaDataEventHandler.TOPIC_BEANMAP_LINKEDTO, user.toString(), item.toString() + " -> " + linked.toString())); //$NON-NLS-1$
			}
		}
	}

	public void fireBroadCastEvent(Dictionary<String, Object> props) {
		if (eventTracker != null) {
			if (!eventTracker.fireEvent("com/arcadsoftware/user/action", props)) {
				debug(String.format(Messages.Activator_Debug_EventNotFired, "com/arcadsoftware/user/action", "--", props.get("message"))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			}
		}		
	}

	private IEntityTesterService getTester() {
		ServiceReference<?> ref = getContext().getServiceReference(IEntityTesterService.clazz);
		if (ref == null) {
			return null;
		}
		Object service = getContext().getService(ref);
		if (service instanceof IEntityTesterService) {
			return (IEntityTesterService) service;
		}
		return null;
	}
	
	public boolean test(String event, MetaDataEntity entity, BeanMapList list, IConnectionUserBean user, Language language) {
		IEntityTesterService tester = getTester();
		if (tester == null) {
			return true;
		}
		return tester.test(event, entity, list, user, language);
	}

	public boolean test(String event, MetaDataEntity entity, BeanMap bean, List<MetaDataAttribute> attributes,
			IConnectionUserBean user, Language language) {
		IEntityTesterService tester = getTester();
		if (tester == null) {
			return true;
		}
		return tester.test(event, entity, bean, attributes, user, language);
	}

	public boolean test(String event, MetaDataEntity entity, BeanMap bean, IConnectionUserBean user, Language language) {
		IEntityTesterService tester = getTester();
		if (tester == null) {
			return true;
		}
		return tester.test(event, entity, bean, user, language);
	}

	public boolean test(String event, MetaDataEntity entity, BeanMap bean, BeanMap newBean,
			ArrayList<MetaDataAttribute> attributes, IConnectionUserBean user, Language language) {
		IEntityTesterService tester = getTester();
		if (tester == null) {
			return true;
		}
		return tester.test(event, entity, bean, newBean, attributes, user, language);
	}

	public Date getListLastChange(MetaDataEntity entity) {
		Date date = entity.getMetadata().get(METADATA_LISTCHANGEDATE, Date.class);
		if (date == null) {
			return entity.getDate();
		}
		return date;
	}
	
	public void setListLastChange(MetaDataEntity entity) {
		entity.getMetadata().put(METADATA_LISTCHANGEDATE, new Date());
		entity.getRegistry().updateEntity(entity);
	}

}
