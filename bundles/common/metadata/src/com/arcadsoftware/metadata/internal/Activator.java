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
package com.arcadsoftware.metadata.internal;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Date;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.eclipse.osgi.framework.console.CommandInterpreter;
import org.eclipse.osgi.framework.console.CommandProvider;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;
import org.osgi.util.tracker.ServiceTracker;
import org.osgi.util.tracker.ServiceTrackerCustomizer;
import org.restlet.data.Language;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.BeanMapEventTracker;
import com.arcadsoftware.metadata.IEntityTesterService;
import com.arcadsoftware.metadata.MapperServiceTracker;
import com.arcadsoftware.metadata.IEntityRegistry;
import com.arcadsoftware.metadata.IMapperService;
import com.arcadsoftware.metadata.IMetaDataModifyListener;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataEventHandler;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.RegistryServiceTracker;
import com.arcadsoftware.metadata.client.DataAccess;
import com.arcadsoftware.osgi.AbstractConfiguredActivator;
import com.arcadsoftware.rest.ITranslationService;
import com.arcadsoftware.rest.connection.ConnectionUserBean;
import com.arcadsoftware.rest.connection.IConnectionInfoService;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public class Activator extends AbstractConfiguredActivator implements ServiceTrackerCustomizer<IMapperService, IMapperService>, CommandProvider {

	private static final Object PROP_DEFAULTREGISTRY = "registry.default"; //$NON-NLS-1$
	private static final String DEFAULTREGISTRYNAME = "XML Entities Registry"; //$NON-NLS-1$
	public static final String TRANLATEDOMAIN_DATA = "data"; //$NON-NLS-1$
	public static final String TRANLATEDOMAIN_ENTITY = "entities"; //$NON-NLS-1$
	private static final String USERTYPE = "user"; //$NON-NLS-1$
	public static final String PROP_MAPPERALIAS = "alias."; //$NON-NLS-1$

	private static Activator instance;

	public static Activator getInstance() {
		return instance;
	}

	private RegistryServiceTracker regTracker;
	private MapperServiceTracker mapperTracker;
	private String defaultRegistry;
	private BeanMapEventTracker eventTracker;
	private ServiceTracker<ITranslationService, ITranslationService> translateTracker;
	private ServiceTracker<IMetaDataModifyListener, IMetaDataModifyListener> modifyTracker;
	private ServiceTracker<IConnectionInfoService, IConnectionInfoService> userInfoTracker;
	private Map<String, String> mapperAliases = new Hashtable<String, String>();
	private Map<String, ServiceRegistration<?>> aliases = new Hashtable<String, ServiceRegistration<?>>();
	
	@Override
	public void start(BundleContext context) throws Exception {
		instance = this;
		mapperTracker = new MapperServiceTracker(context, this);
		super.start(context);
		eventTracker = new BeanMapEventTracker(context);
		translateTracker = new ServiceTracker<ITranslationService, ITranslationService>(context, ITranslationService.class, null);
		translateTracker.open();
		modifyTracker = new ServiceTracker<IMetaDataModifyListener, IMetaDataModifyListener>(context, IMetaDataModifyListener.class, null);
		modifyTracker.open();
		userInfoTracker = new ServiceTracker<IConnectionInfoService, IConnectionInfoService>(context, IConnectionInfoService.class, null);
		userInfoTracker.open();
		regTracker = new RegistryServiceTracker(context, new ServiceTrackerCustomizer<Object, Object>() {
			
			public void removedService(ServiceReference<Object> reference, Object service) {
				try {
					sendRegistryEvent(MetaDataEventHandler.TOPIC_REGISTRY_DESTROYED,(IEntityRegistry)service);
				} catch (ClassCastException e) {
					debug(Messages.Activator_Debug_BadRegistry + service.getClass().getCanonicalName());
				}
			}
			
			public void modifiedService(ServiceReference<Object> reference, Object service) {}
			
			public Object addingService(ServiceReference<Object> reference) {
				Object service = getContext().getService(reference);
				try {
					sendRegistryEvent(MetaDataEventHandler.TOPIC_REGISTRY_CREATED, (IEntityRegistry) service);
				} catch (ClassCastException e) {
					debug(Messages.Activator_Debug_BadRegistry + service.getClass().getCanonicalName());
				}
				return service;
			}
		});
		// Register an Equinox Console command provider.
		registerService(CommandProvider.class.getName(), this);
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		regTracker.close();
		mapperAliases.clear();
		aliases.clear();
		regTracker = null;
		mapperTracker.close();
		mapperTracker = null;
		userInfoTracker.close();
		userInfoTracker = null;
		eventTracker.close();
		eventTracker = null;
		translateTracker.close();
		translateTracker = null;
		modifyTracker.close();
		modifyTracker = null;
		super.stop(context);
		instance = null;
	}

	@Override
	public synchronized void updatedConfiguration(Dictionary<String,Object> properties) {
		if (properties != null) {
			defaultRegistry = (String)properties.get(PROP_DEFAULTREGISTRY);
			Map<String, String> newaliases = new Hashtable<String, String>();
			Enumeration<?> enu = properties.keys();
			while (enu.hasMoreElements()) {
				String key = enu.nextElement().toString();
				if (key.startsWith(PROP_MAPPERALIAS)) {
					newaliases.put(key.substring(PROP_MAPPERALIAS.length()), properties.get(key).toString());
				}
			}
			// Cet algo n'est pas bon, les ajout récursifs d'alias peuvent passer au travers !
			for(Entry<String, String> oke:mapperAliases.entrySet()) {
				if (!newaliases.containsKey(oke.getKey())) {
					unregisterMapperAlias(oke.getKey());
				}
			}
			for(Entry<String, String> nke:newaliases.entrySet()) {
				String olddomain = mapperAliases.get(nke.getKey());
				if (olddomain == null) {
					ServiceReference<IMapperService> ref = mapperTracker.getServiceReference(nke.getValue());
					if (ref != null) {
						registerMapperAlias(nke.getKey(), ref, mapperTracker.getService(ref));
					}
				} else if (!olddomain.equals(nke.getValue())) {
					unregisterMapperAlias(nke.getKey());
					ServiceReference<IMapperService> ref = mapperTracker.getServiceReference(nke.getValue());
					if (ref != null) {
						registerMapperAlias(nke.getKey(), ref, mapperTracker.getService(ref));
					}
				}
			}
			// Redefine default aliases.
			if (mapperTracker.getServiceReferences() != null) {
				for (ServiceReference<?> sr:mapperTracker.getServiceReferences()) {
					String domain = (String)sr.getProperty(IMapperService.PROP_DOMAINNAME);
					if (domain != null) {
						int i = domain.indexOf(':');
						if (i > 0) {
							String a = domain.substring(i+1);
							if ((a != null) && (a.length() > 0) && (!newaliases.containsKey(a))) {
								newaliases.put(a, domain);
							}
						}
					}
				}
			}
			mapperAliases = newaliases;
		}
	}
	
	private void registerMapperAlias(String domain, ServiceReference<?> ref, IMapperService mapper) {
		if (ref != null) {
			Hashtable<String, Object> props = new Hashtable<String, Object>();
			for (String p: ref.getPropertyKeys()) {
				props.put(p, ref.getProperty(p));
			}
			props.put(IMapperService.PROP_ALIAS, ref.getProperty(IMapperService.PROP_DOMAINNAME));
			props.put(IMapperService.PROP_DOMAINNAME, domain);
			mapper.addDomain(domain);
			ServiceRegistration<?> sr = registerService(IMapperService.clazz, mapper, props);
			aliases.put(domain, sr);
			debug("Domain alias registered: " + domain);
		}
	}

	private void unregisterMapperAlias(String domain) {
		ServiceRegistration<?> sr = aliases.get(domain);
		if (sr != null) {
			unregister(sr);
			aliases.remove(domain);
		}
	}

	public IEntityRegistry getRegistry() {
		if (regTracker == null) {
			return null;
		}
		return regTracker.getRegistry();
	}
	
	public IEntityRegistry getDefaultRegistry() {
		if (regTracker == null) {
			return null;
		}
		if ((defaultRegistry == null) || (defaultRegistry.length() == 0)) {
			return regTracker.getRegistry(DEFAULTREGISTRYNAME);
		}
		return regTracker.getRegistry(defaultRegistry);
	}

	public IEntityRegistry getRegistry(String name) {
		if (regTracker == null) {
			return null;
		}
		return regTracker.getRegistry(name);
	}

	public IEntityRegistry getRegistry(MetaDataEntity metaDataEntity) {
		if (regTracker == null) {
			return null;
		}
		return regTracker.getRegistry(metaDataEntity);
	}

	public MetaDataEntity getEntity(String type) {
		if ((regTracker == null) || (type == null)) {
			return null;
		}
		return regTracker.getEntity(type);
	}

	public List<MetaDataEntity> getEntities(String domain) {
		if ((regTracker == null) || (domain == null)) {
			return new ArrayList<MetaDataEntity>();
		}
		return regTracker.getEntities(domain);
	}

	public List<MetaDataEntity> getEntities() {
		if (regTracker == null) {
			return new ArrayList<MetaDataEntity>();
		}
		return regTracker.getEntities();
	}
	
	/**
	 * Return the Mapper service associated to the given domain name.
	 * 
	 * @param name a Mapper domain name.
	 * @return a beanmap or null.
	 */
	public IMapperService getBeanMapper(String name) {
		if (mapperTracker == null) {
			return null;
		}
		ServiceReference<IMapperService>[] serefs = mapperTracker.getServiceReferences();
		if (serefs == null) {
			return null;
		}
		if (name == null) {
			for(ServiceReference<IMapperService> se: serefs) {
				if (se.getProperty(IMapperService.PROP_DOMAINNAME) == null) {
					try {
						return mapperTracker.getService(se);
					} catch (ClassCastException e) {
						warn(Messages.Activator_Warn_BadMapper,e);
						return null;
					}
				}
			}
			return null;
		}
		for(ServiceReference<IMapperService> se: serefs) {
			Object sn = se.getProperty(IMapperService.PROP_DOMAINNAME);
			if ((sn != null) && name.equalsIgnoreCase(sn.toString())) {
				try {
					return mapperTracker.getService(se);
				} catch (ClassCastException e) {
					warn(Messages.Activator_Warn_BadMapper,e);
					return null;
				}
			}
		}
		return null;
	}
	
	@Override
	public synchronized IMapperService addingService(ServiceReference<IMapperService> reference) {
		// Ajout d'un mapper
		IMapperService mapper = getContext().getService(reference);
		if (mapper == null) {
			debug(Messages.Activator_Debug_BadMapper);
			return null;
		}
		Object o = reference.getProperty(IMapperService.PROP_DOMAINNAME);
		if ((o == null) || (o.toString().length() == 0)) {
			debug(Messages.Activator_Debug_BadMapper + mapper.getClass().getCanonicalName());
			return null;
		}
		String domain = o.toString();
		// Définition automatique d'un alias réduit:
		int i = domain.indexOf(':');
		if (i > 0) {
			String a = domain.substring(i + 1);
			if ((a != null) && (a.length() > 0) && (!mapperAliases.containsKey(a))) {
				mapperAliases.put(a, domain);
			}
		}
		StringBuilder domains = new StringBuilder(domain);
		for(Entry<String,String> e:mapperAliases.entrySet()) {
			if (domain.equals(e.getValue())) {
				registerMapperAlias(e.getKey(), reference, (IMapperService) mapper);
				domains.append(' ');
				domains.append(e.getKey());
			}
		}
		sendMapperEvent(MetaDataEventHandler.TOPIC_MAPPER_CREATED, (IMapperService) mapper, domains.toString());
		return mapper;
	}

	@Override
	public void modifiedService(ServiceReference<IMapperService> reference, IMapperService service) {
		// Quand un mapper change de domaine ! (on ne peut pas connaître son ancien domaine...)
	}

	@Override
	public synchronized void removedService(ServiceReference<IMapperService> reference, IMapperService mapper) {
		// Suppression d'un mapper...
		if (mapper == null) {
			debug(Messages.Activator_Debug_BadMapper);
			return;
		}
		Object o = reference.getProperty(IMapperService.PROP_DOMAINNAME);
		if ((o == null) || (o.toString().length() == 0)) {
			debug(Messages.Activator_Debug_BadMapper + mapper.getClass().getCanonicalName());
			return;
		}
		String domain = o.toString();
		StringBuilder domains = new StringBuilder(domain); 
		for(Entry<String,String> e:mapperAliases.entrySet()) {
			if (domain.equals(e.getValue())) {
				unregisterMapperAlias(e.getKey());
				domains.append(' ');
				domains.append(e.getKey());
			}
		}
		sendMapperEvent(MetaDataEventHandler.TOPIC_MAPPER_DESTROYED, mapper, domains.toString());
	}

	public void sendEvent(String topic, Dictionary<String, ?> properties) {
		ServiceReference<?> ref = getContext().getServiceReference(EventAdmin.class.getName());
		if (ref == null) {
			return;
		}
		try {
			EventAdmin ea = (EventAdmin) getContext().getService(ref);
			ea.postEvent(new Event(topic, properties));
		} catch (RuntimeException e) {
			debug(e);
		}
	}
	
	private void sendMapperEvent(String topic, IMapperService mapper, String domains) {
		ServiceReference<?> ref = getContext().getServiceReference(EventAdmin.class.getName());
		if (ref == null) {
			return;
		}
		try {
			EventAdmin ea = (EventAdmin)getContext().getService(ref);
			HashMap<String, Object> properties = new HashMap<String, Object>();
			properties.put(MetaDataEventHandler.EVENT_PROP_MAPPER, mapper);
			if (domains != null) {
				properties.put(MetaDataEventHandler.EVENT_PROP_DOMAINS, domains);
			}
			ea.postEvent(new Event(topic, properties));
		} catch (RuntimeException e) {
			debug(e);
		}
	}
	
	private void sendRegistryEvent(String topic, IEntityRegistry service) {
		ServiceReference<?> ref = getContext().getServiceReference(EventAdmin.class.getName());
		if (ref == null) {
			return;
		}
		try {
			EventAdmin ea = (EventAdmin) getContext().getService(ref);
			HashMap<String, Object> properties = new HashMap<String, Object>();
			properties.put(MetaDataEventHandler.EVENT_PROP_REGISTRY, service);
			ea.postEvent(new Event(topic, properties));
		} catch (RuntimeException e) {
			debug(e);
		}
	}

	public String translate(String domain, String code, Language language) {
		if (translateTracker == null) {
			return code;
		}
		Object ts = translateTracker.getService();
		if (ts instanceof ITranslationService) {
			return ((ITranslationService)ts).translate(domain, code, language);
		}
		return code;
	}

	@SuppressWarnings("unchecked")
	public List<IMetaDataModifyListener> getModifyListener(String type) {
		ArrayList<IMetaDataModifyListener> result = new ArrayList<IMetaDataModifyListener>();
		ServiceReference<? extends Object>[] refs = modifyTracker.getServiceReferences();
		if (refs != null) {
			for(@SuppressWarnings("rawtypes") ServiceReference ref:refs) {
				Object o = ref.getProperty(IMetaDataModifyListener.PROP_TYPE);
				if ((o == null) || (type == null) || type.equals(o)) {
					result.add(modifyTracker.getService(ref));
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

	private IEntityTesterService getTester() {
		ServiceReference<?> ref = getContext().getServiceReference(IEntityTesterService.clazz);
		if (ref == null) {
			return null;
		}
		Object service = getContext().getService(ref);
		if (service instanceof IEntityTesterService) {
			return (IEntityTesterService)service;
		}
		return null;
	}
	
	public boolean test(String event, MetaDataEntity entity, BeanMapList list, IConnectionUserBean user,
			Language language) {
		IEntityTesterService tester = getTester();
		if (tester == null) {
			return true;
		}
		return ((IEntityTesterService)tester).test(event, entity, list, user, language);
	}

	public boolean test(String event, MetaDataEntity entity, BeanMap bean, List<MetaDataAttribute> attributes,
			IConnectionUserBean user, Language language) {
		IEntityTesterService tester = getTester();
		if (tester == null) {
			return true;
		}
		return ((IEntityTesterService)tester).test(event, entity, bean, attributes, user, language);
	}

	public boolean test(String event, MetaDataEntity entity, BeanMap bean, IConnectionUserBean user, Language language) {
		IEntityTesterService tester = getTester();
		if (tester == null) {
			return true;
		}
		return ((IEntityTesterService)tester).test(event, entity, bean, user, language);
	}

	public boolean test(String event, MetaDataEntity entity, BeanMap bean, BeanMap newBean,
			ArrayList<MetaDataAttribute> attributes, IConnectionUserBean user, Language language) {
		IEntityTesterService tester = getTester();
		if (tester == null) {
			return true;
		}
		return ((IEntityTesterService)tester).test(event, entity, bean, newBean, attributes, user, language);
	}

	public IConnectionUserBean getConnectionUser(int id) {
		if (userInfoTracker == null) {
			return null;
		}
		ServiceReference<IConnectionInfoService>[] refs = userInfoTracker.getServiceReferences();
		if (refs != null) {
			for (ServiceReference<IConnectionInfoService> ref:refs) {
				ConnectionUserBean user = userInfoTracker.getService(ref).loadUser(USERTYPE, id);
				if (user != null) {
					return user;
				}
			}
		}
		return null;
	}
	
	public String getHelp() {
		return Messages.Activator_ConsoleHelp_MapperAlias;
	}

	public void _entities(CommandInterpreter ci) throws Exception {
		String param = ci.nextArgument();
		if (param != null) {
			if ("check".equalsIgnoreCase(param)) {
				_entities_check(ci);
				return;
			}
			if ("count".equalsIgnoreCase(param) || "cnt".equalsIgnoreCase(param)) {
				ci.println("Data Counts:");
				for (MetaDataEntity entity: getEntities()) {
					ci.println(String.format("%-20s (%s) v%d - count: %d", entity.getType(), entity.getDomain(), entity.getVersion(), entity.dataCount()));
				}
				return;
			}
			if ("del".equalsIgnoreCase(param) || "delete".equalsIgnoreCase(param) || "deleted".equalsIgnoreCase(param)) {
				ci.println("Deleted data Counts:");
				for (MetaDataEntity entity: getEntities()) {
					ci.println(String.format("%-20s (%s) v%d - count: %d", entity.getType(), entity.getDomain(), entity.getVersion(), entity.dataCount(true,null,false) - entity.dataCount()));
				}
				return;
			}
		}
		for (MetaDataEntity entity: getEntities()) {
			ci.println(String.format("%-20s (%s) v%d", entity.getType(), entity.getDomain(), entity.getVersion()));
		}
	}

	public void _entity(CommandInterpreter ci) throws Exception {
		String type = ci.nextArgument();
		if ((type == null) || (type.length() == 0)) {
			_entities(ci);
		}
		MetaDataEntity entity = getEntity(type);
		if (entity == null) {
			if ("check".equalsIgnoreCase(type)) {
				_entities_check(ci);
				return;
			}
			ci.println(String.format("The entity \"%s\" is not registered.", type));
			return;
		}
		ci.println(String.format("Entity %s (Version %d)", entity.getType(), entity.getVersion()));
		ci.println("Domain: " + entity.getDomain());
		if (entity.getMetadata().size() > 0) {
			ci.println("Metadata: " + entity.getMetadata().prettyprint());
		}
		if (entity.getAttributes().size() > 0) {
			ci.println("Attributes: ");
			for(Entry<String, MetaDataAttribute> e:entity.getAttributes().entrySet()) {
				ci.println(String.format("%-15s (%s) %d/%d Mdt:%b Lst:%b", e.getKey(), e.getValue().getType(), e.getValue().getLength(), e.getValue().getPrecision(), e.getValue().getMandatory(), e.getValue().getListable()));
			}
		}
		if (entity.getLinks().size() > 0) {
			ci.println();
			ci.println("Links: ");
			for(Entry<String, MetaDataLink> e:entity.getLinks().entrySet()) {
				ci.println(String.format("%-15s (%s)", e.getKey(), e.getValue().getType()));
			}
		}
	}
	
	public void _mapper(CommandInterpreter ci) throws Exception {
		_mappers(ci);
	}

	public void _mappers(CommandInterpreter ci) throws Exception {
		ServiceReference<IMapperService>[] serefs = mapperTracker.getServiceReferences();
		if ((serefs == null) || (serefs.length == 0)) {
			ci.println("No Mapper registered.");
		} else {
			for(ServiceReference<IMapperService> se:serefs) {
				Object o = se.getProperty(IMapperService.PROP_DOMAINNAME);
				if (o != null) {
					ci.print(o);
				} else {
					ci.print("Anonymous mapper");
				}
				o = mapperTracker.getService(se);
				if (o != null) {
					ci.print(" ("); //$NON-NLS-1$
					ci.print(o.getClass().getName());
					ci.print(')');
				}					
				o = se.getProperty(IMapperService.PROP_ALIAS);
				if (o != null) {
					ci.print(" alias from \"");
					ci.print(o);
					ci.print('"');
				}
				ci.println();
				o = se.getProperty(IMapperService.PROP_SUPPORT_EXTRAREFERENCES);
				if (o != null) {
					ci.print("\tSupport Extra ref = ");
					ci.println(o);
				}
				o = se.getProperty(IMapperService.PROP_SUPPORT_GROUPSENTITY);
				if (o != null) {
					ci.print("\tSupport groups = ");
					ci.println(o);
				}
				o = se.getProperty(IMapperService.PROP_SUPPORT_MULTILINKREFERENCES);
				if (o != null) {
					ci.print("\tSupport Multi links = ");
					ci.println(o);
				}
				o = se.getProperty(IMapperService.PROP_SUPPORT_PAGINATION);
				if (o != null) {
					ci.print("\tSupport Pagination = ");
					ci.println(o);
				}
				o = se.getProperty(IMapperService.PROP_SUPPORT_SOFTDELETION);
				if (o != null) {
					ci.print("\tSupport Soft deletion = ");
					ci.println(o);
				}
			}
		}
	}
	
	public void _mapperAlias(CommandInterpreter ci) throws Exception {
		String domain = ci.nextArgument();
		if (domain == null) {
			ci.println("Mapper aliases list:");
			for(Entry<String,String> e:mapperAliases.entrySet()) {
				ci.print(e.getKey());
				ci.print(" --> "); //$NON-NLS-1$
				ci.println(e.getValue());
			}
			return;
		}
		ArrayList<String> na = new ArrayList<String>();
		String s = ci.nextArgument();
		while (s != null) {
			na.add(s);
			s = ci.nextArgument();
		}
		ArrayList<String> rma = new ArrayList<String>();
		for(Entry<String,String> e:mapperAliases.entrySet()) {
			if (domain.equals(e.getValue())) {
				if (!na.contains(e.getKey())) {
					rma.add(e.getKey());
				}
			}
		}
		Dictionary<String, Object> props = getCurrentConfiguration();
		for(String a:rma) {
			props.remove(PROP_MAPPERALIAS + a);
		}
		for(String a:na) {
			props.put(PROP_MAPPERALIAS + a, domain);
		}
		updateConfiguration(props);
	}

	public void _mda(CommandInterpreter ci) throws Exception {
		_metadata(ci);
	}

	public void _metadata(CommandInterpreter ci) throws Exception {
		long t = System.currentTimeMillis();
		try {
			String url = ci.nextArgument();
			String login = ci.nextArgument();
			String pwd = ci.nextArgument();
			String cmd = ci.nextArgument();
			String type = ci.nextArgument();
			String id = ci.nextArgument();
			if ((url == null) || (login == null) || (pwd == null) || (cmd == null) || (type == null) || ((id == null) && !cmd.startsWith("r"))) { //$NON-NLS-1$
				ci.println("Wrong number of parameter type \"help\" for details.");
				return;
			}
			DataAccess da = new DataAccess(url, login, pwd.toCharArray());
			// <url> <login> <password> c|r|u|d <type> [<id> [<attribut> <value>...]]
			if (cmd.startsWith("r")) {
				if (id != null) {
					ci.println("Selected item:");
					ci.println(da.get(type, Integer.parseInt(id)));
				} else {
					ci.println("Selected items:");
					ci.println(da.getList(type));
				}
				return;
			}
			if (cmd.startsWith("d")) {
				da.delete(type, Integer.parseInt(id));
				ci.println("Item deleted.");
				return;
			}
			BeanMap b = new BeanMap(type, Integer.parseInt(id));
			String s;
			if (cmd.startsWith("c")) {
				s = id;
			} else {
				s = ci.nextArgument();
			}
			while(s != null) {
				b.put(s, ci.nextArgument());
				s = ci.nextArgument();
			}
			if (cmd.startsWith("c")) {
				ci.println("Item creation:");
				da.post(b);
				return;
			}
			if (cmd.startsWith("u")) {
				ci.println("Item update:");
				da.put(b);
				return;
			}
			ci.println("Unknown command: " + cmd);
		} finally {
			ci.println(String.format("Timing: %dms.", System.currentTimeMillis() - t));
		}
	}

	public void _entity_check(CommandInterpreter ci) throws Exception {
		_entities_check(ci);
	}

	public void _entities_check(CommandInterpreter ci) throws Exception {
		String type = ci.nextArgument();
		ci.println();
		ci.println("This command allow to verify the current declaration of the MetaData Entities.");
		ci.println("The result of this test depend on your current installation and the storage source (i.e. the database).");
		ci.println("Some error related to this test may be only reported in the server log, and not in the console.");
		ci.println("Please check your server log if it is not printed in this console.");
		ci.println("[Comments into bracket are not necessarily a problem.]");
		ci.println();
		if (type != null) {
			checkEntity(ci, getEntity(type));
			ci.println();
		} else {
			for(MetaDataEntity e: getEntities()) {
				checkEntity(ci, e);
				ci.println();
			}
		}
		ci.println("Verification terminated.");
	}
	
	private void checkEntity(CommandInterpreter ci, MetaDataEntity entity) {
		if (entity != null) {
			ci.println(String.format("Diagnose the entity %s (Version %d), problems if any:", entity.getType(), entity.getVersion()));
			boolean nopb = true;
			if (entity.getMapper() == null) {
				ci.println("  - No mapper available for domain: " + entity.getDomain());
				nopb = false;
			} else {
				try {
					int c = entity.dataCount(true, null, false, null);
					if (c == 0) {
						ci.println("  - [Empty entity in the storage source...]");
						nopb = false;
					}
					int id = 0;
					try {
						BeanMapList list = entity.dataSelection(entity.getAllAttributes(), true, null, false, null, null, 0, 1);
						if (list == null) {
							ci.println("  - [Entity selection return null list...]");
							nopb = false;
						}
						if (!list.isEmpty()) {
							id = list.get(0).getId();
						}
						if (!entity.isReadOnly()) {
							ArrayList<MetaDataAttribute> attributes = new ArrayList<>();
							ArrayList<Object> values = new ArrayList<>();
							boolean skip = false;
							for (MetaDataAttribute a: entity.getAttributes().values()) {
								if (!a.isLocalReference() && (a.getRefEntity() != null)) {
									ci.println("  - [Attribute referencing foreign entity may slow down the data access: " + a.getCode() + ']');
									nopb = false;
								}
								if (!a.isReadonly()) {
									MetaDataEntity ref = a.getRefEntity();
									if (ref != null) {
										list = ref.dataSelection("", true, null, false, null, null, 0, 1);
										if ((list == null) || list.isEmpty()) {
											if (a.isMandatory()) {
												ci.println("  - [Mandatory data can not be provided for the attribute: " + a.getCode() + ']');
												nopb = false;
												skip = true;
											}
										} else {
											attributes.add(a);
											values.add(list.get(0).getId());
										}
									} else if (a.isSimpleType()) {
										switch(a.getType()) {
										case MetaDataAttribute.TYPE_BOOLEAN:
											attributes.add(a);
											values.add(true);
											break;
										case MetaDataAttribute.TYPE_DATE:
											attributes.add(a);
											values.add(new Date());
											break;
										case MetaDataAttribute.TYPE_FLOAT:
											attributes.add(a);
											values.add(0.0d);
											break;
										case MetaDataAttribute.TYPE_ICON:
										case MetaDataAttribute.TYPE_INT:
										case MetaDataAttribute.TYPE_INTEGER:
											attributes.add(a);
											values.add(1);
											break;
										case MetaDataAttribute.TYPE_BIGINTEGER:
											attributes.add(a);
											values.add(BigInteger.valueOf(1l));
											break;
										case MetaDataAttribute.TYPE_LONG:
											attributes.add(a);
											values.add(1l);
											break;
										case MetaDataAttribute.TYPE_RANGE:
											int i = 1;
											if (a.getLength() > a.getPrecision()) {
												if (i > a.getLength()) {
													i = a.getLength();
												} else if (i < a.getPrecision()) {
													i = a.getPrecision();
												}
												attributes.add(a);
												values.add(i);
											} else {
												ci.println("  - [Range attribute do not provide a correct set of acceptable values: " + a.getCode() + ']');
												nopb = false;
												if (a.isMandatory()) {
													skip = true;
												}
											}
											break;
										case MetaDataAttribute.TYPE_EMAIL:
											attributes.add(a);
											values.add("contact@test.net"); //$NON-NLS-1$
											break;
										case MetaDataAttribute.TYPE_URL:
											attributes.add(a);
											values.add("https://www.arcadsoftware.com"); //$NON-NLS-1$
											break;
										case MetaDataAttribute.TYPE_STRING:
											attributes.add(a);
											if (a.getLength() > 0) {
												values.add("x".repeat(a.getLength()));
											} else {
												ci.println(String.format("  - [The String attribute \"%s\" does not define a string limit length.]", a.getCode()));
												nopb = false;
												values.add("z");
											}
											break;
										case MetaDataAttribute.TYPE_TRANSLATE:
											break;
										default:
											ci.println(String.format("  - [Unkown simple type \"%s\" (contact AFS dev team).]", a.getType()));
											nopb = false;
											if (a.isMandatory()) {
												skip = true;
											}
										}
									} else {
										ci.println(String.format("  - The attribute \"%s\" reference an unkown type \"%s\".", a.getCode(), a.getType()));
										nopb = false;
										if (a.isMandatory()) {
											skip = true;
										}
									}
								}
							}
							if (!skip) {
								try {
									BeanMap b = entity.dataCreate(attributes, values);
									if (b == null) {
										ci.println("  - Unable to create data with this attribute list [may be due to unique constraints...]: " + attributes.toString());
										ci.println("    - with these values: " + values.toString());
										nopb = false;
									} else if (b.getId() <= 0) {
										ci.println("  - Data creation returned an invalid ID, with this attribute list: " + attributes.toString());
										ci.println("    - with these values: " + values.toString());
										nopb = false;
									} else {
										try {
											entity.dataDelete(b.getId(), true);
										} catch (Throwable e) {
											nopb = false;
											ci.println("  - Unable to delete data with id: " + b.getId());
											ci.println("    - with these values: " + values.toString());
											if ((e.getCause() == null) || !(e instanceof ResourceException)) { 
												ci.println("    - " + e.getLocalizedMessage());
											}
											while (e.getCause() != null) {
												e = e.getCause();
												ci.println("    - " + e.getLocalizedMessage());
											}
										}
									}
								} catch (Throwable e) {
									nopb = false;
									ci.println("  - Unable to create data with this attribute list [may be due to unique constraints...]: " + attributes.toString());
									ci.println("    - with these values: " + values.toString());
									if ((e.getCause() == null) || !(e instanceof ResourceException)) { 
										ci.println("    - " + e.getLocalizedMessage());
									}
									while (e.getCause() != null) {
										e = e.getCause();
										ci.println("    - " + e.getLocalizedMessage());
									}
								}
							}
						}
					} catch (Throwable e) {
						nopb = false;
						ci.println("  - Unable to select the attributes of the entity.");
						if ((e.getCause() == null) || !(e instanceof ResourceException)) { 
							ci.println("    - " + e.getLocalizedMessage());
						}
						while (e.getCause() != null) {
							e = e.getCause();
							ci.println("    - " + e.getLocalizedMessage());
						}
					}
					if (id > 0) {
						for (MetaDataLink link: entity.getLinks().values()) {
							try {
								link.dataCount(id);
							} catch (Throwable e) {
								nopb = false;
								ci.println("  - Unable to reach the linked data: " + link.getCode());
								if ((e.getCause() == null) || !(e instanceof ResourceException)) { 
									ci.println("    - " + e.getLocalizedMessage());
								}
								while (e.getCause() != null) {
									e = e.getCause();
									ci.println("    - " + e.getLocalizedMessage());
								}
							}
						}
					}
				} catch (Throwable e) {
					nopb = false;
					ci.println("  - Unable to reach the data of this entity on domain: " + entity.getDomain());
					if ((e.getCause() == null) || !(e instanceof ResourceException)) { 
						ci.println("    - " + e.getLocalizedMessage());
					}
					while (e.getCause() != null) {
						e = e.getCause();
						ci.println("    - " + e.getLocalizedMessage());
					}
				}
			}
			if (nopb) {
				ci.println("  No problem found.");
			}
		}
	}

	public Map<String, String> getAliasesMap() {
		return mapperAliases;
	}
	
}
