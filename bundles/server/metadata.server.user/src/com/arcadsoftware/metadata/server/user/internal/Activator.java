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
package com.arcadsoftware.metadata.server.user.internal;

import java.util.Collection;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.Timer;
import java.util.TimerTask;

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.service.event.EventConstants;
import org.osgi.service.event.EventHandler;
import org.restlet.Context;
import org.restlet.routing.Router;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.IMapperService;
import com.arcadsoftware.metadata.IMetaDataDeleteListener;
import com.arcadsoftware.metadata.IMetaDataLinkingListener;
import com.arcadsoftware.metadata.IMetaDataModifyListener;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataEventHandler;
import com.arcadsoftware.metadata.criteria.AndCriteria;
import com.arcadsoftware.metadata.criteria.EqualCriteria;
import com.arcadsoftware.osgi.AbstractActivator;
import com.arcadsoftware.rest.RouteList;
import com.arcadsoftware.rest.SimpleBranch;
import com.arcadsoftware.rest.connection.IConnectionCache;

public class Activator extends AbstractActivator {

	protected static final boolean UPDATEALLRIGHTPROFILE = !Boolean.getBoolean("com.arcadsoftware.profile.allrights.disabled");
	protected static final boolean PROTECTADMINUSER = !Boolean.getBoolean("com.arcadsoftware.user.noadmin");
	
	protected static final String TYPE_USER = "user"; //$NON-NLS-1$
	protected static final String TYPE_PROFILE = "profile"; //$NON-NLS-1$
	protected static final String TYPE_PROFILERIGHT = "profileRight"; //$NON-NLS-1$
	protected static final String TYPE_RIGHT = "right"; //$NON-NLS-1$
	protected static final String PROFILERIGHT_RIGHT = TYPE_RIGHT;
	protected static final String PROFILERIGHT_PROFILE = TYPE_PROFILE;
	protected static final String PROFILERIGHT_PROFILE_RIGHT = PROFILERIGHT_PROFILE + ' ' + PROFILERIGHT_RIGHT;
	protected static final String LINK_PROFILES = "profiles"; //$NON-NLS-1$
	protected static final String LINK_PROFILERIGHTS = "profilerights"; //$NON-NLS-1$
	protected static final String LINK_USERS = "users"; //$NON-NLS-1$
	protected static final String ALIAS_USERDB = "alias.userdb"; //$NON-NLS-1$
	protected static final String METADATAPID = "com.arcadsoftware.metadata"; //$NON-NLS-1$
	protected static final String JDBC_PREFIX = "jdbc:"; //$NON-NLS-1$
	protected static final String USERDB = "userdb"; //$NON-NLS-1$
	
	@Override
	public void start(BundleContext bundleContext) throws Exception {
		super.start(bundleContext);
		// Listen to all Metadata change around the User <-> Rights linking... to purge connection caches... 
		registerService(IMetaDataDeleteListener.class, new DeleteListener(this), IMetaDataDeleteListener.PROP_TYPE, TYPE_USER);
		registerService(IMetaDataDeleteListener.class, new DeleteListener(this), IMetaDataDeleteListener.PROP_TYPE, TYPE_PROFILE);
		registerService(IMetaDataDeleteListener.class, new DeleteListener(this), IMetaDataDeleteListener.PROP_TYPE, TYPE_PROFILERIGHT);
		Dictionary<String, Object> props = new Hashtable<String, Object>();
		props.put(IMetaDataLinkingListener.PROP_TYPE, TYPE_PROFILE);
		props.put(IMetaDataLinkingListener.PROP_LINK, LINK_USERS);
		registerService(IMetaDataLinkingListener.class, new LinkingListener(this), props);
		props = new Hashtable<String, Object>();
		props.put(IMetaDataLinkingListener.PROP_TYPE, TYPE_USER);
		props.put(IMetaDataLinkingListener.PROP_LINK, LINK_PROFILES);
		registerService(IMetaDataLinkingListener.class, new LinkingListener(this), props);
		registerService(IMetaDataModifyListener.class, new ModifyListener(this), IMetaDataModifyListener.PROP_TYPE, TYPE_USER);
		registerService(IMetaDataModifyListener.class, new ModifyListener(this), IMetaDataModifyListener.PROP_TYPE, TYPE_PROFILERIGHT);
		props = new Hashtable<String, Object>();
		props.put(IMetaDataLinkingListener.PROP_TYPE, TYPE_PROFILE);
		props.put(IMetaDataLinkingListener.PROP_LINK, LINK_PROFILERIGHTS);
		registerService(IMetaDataLinkingListener.class, new LinkingProfileListener(), props);

		
		// Check if any Mapper is not already started...
		Collection<ServiceReference<IMapperService>> msrs = bundleContext.getServiceReferences(IMapperService.class, null);
		if (msrs != null) {
			String anyJCBCMapper = null;
			boolean found = false;
			for (ServiceReference<IMapperService> msr: msrs) {
				Object dm = msr.getProperty(IMapperService.PROP_DOMAINNAME);
				if (dm instanceof String) {
					if ((anyJCBCMapper == null) && ((String) dm).startsWith(JDBC_PREFIX)) {
						anyJCBCMapper = (String) dm;
					} else if (USERDB.equals(dm)) {
						found = true;
					}
				}
			}
			if (!found && (anyJCBCMapper != null)) {
				// A JDBC Mapper has been declared and it is used for userdb !
				props = getConfiguration(Activator.METADATAPID);
				// If there is no alias defined for the "userdb" use this domain !
				if (props == null) {
					props = new Hashtable<String, Object>();
					props.put(Activator.ALIAS_USERDB, anyJCBCMapper);
					setConfiguration(Activator.METADATAPID, props);
				} else if (props.get(ALIAS_USERDB) == null) {
					props.put(Activator.ALIAS_USERDB, anyJCBCMapper);
					setConfiguration(Activator.METADATAPID, props);
				}
				// FIXME this only work if the configuration admin service is already started !!!
			}
		}
		registerService(EventHandler.class, new MapperAliasConfiguration(this), EventConstants.EVENT_TOPIC, MetaDataEventHandler.TOPIC_MAPPER_CREATED);
		if (UPDATEALLRIGHTPROFILE) {
			// This delayed treatment is require because all other artifact (rights, entities and mapper) may be already loaded,
			// and moreover the association of the "userdb" mapper and the entities is not triggered !
			new Timer("Profile ALL update Delayed").schedule(new TimerTask() {
				@Override
				public void run() {
					updateAllRightProfileSync();
					// TODO Fix the order of synchronization to avoid multiple updates !
					Activator.this.registerService(EventHandler.class.getName(), new AllRightProfileUpgrade(Activator.this), EventConstants.EVENT_TOPIC, MetaDataEventHandler.TOPIC_ENTITY_CREATED);
					Activator.this.registerService(EventHandler.class.getName(), new AllRightProfileUpgrade(Activator.this), EventConstants.EVENT_TOPIC, "com/arcadsoftware/metadata/right/add"); //$NON-NLS-1$
				}
			}, 9450);
		}
		// Attach specific services related to user management...
		registerService(SimpleBranch.clazz, new SimpleBranch() {
			@Override
			protected RouteList createAttachedResources(Context context, Router router) {
				return new RouteList( //
						router.attach("/data/user/{id}/auths", UserAuthsRecource.class) //$NON-NLS-1$
						);
			}
		}, SimpleBranch.properties(SimpleBranch.SECUREDBRANCH));
	}

	/**
	 * Try to update the first Profile from the database with the list of all rights declared onto this server...
	 */
	protected void updateAllRightProfile() {
		new Thread(new Runnable() {
			@Override
			public void run() {
				
			}
		}, "User Profile ALL update").start();
	}
	/**
	 * Try to update the first Profile from the database with the list of all rights declared onto this server...
	 */
	protected synchronized void updateAllRightProfileSync() {
		// Check Entity accessibility...
		MetaDataEntity profiles = MetaDataEntity.loadEntity(TYPE_PROFILE);
		if (profiles == null) {
			debug("ALL Rigths Profile update: Entity Profile not yet declared.");
			return;
		}
		if (profiles.getMapper() == null) {
			debug("ALL Rigths Profile update: Entity Profile mapper not activated.");
			return;
		}
		MetaDataEntity profileRights = MetaDataEntity.loadEntity(TYPE_PROFILERIGHT);
		if (profileRights == null) {
			debug("ALL Rigths Profile update: Entity Profileright not yet declared.");
			return;
		}
		if (profileRights.getMapper() == null) {
			debug("ALL Rigths Profile update: Entity Profileright mapper not activated.");
			return;
		}
		MetaDataEntity rights = MetaDataEntity.loadEntity(TYPE_RIGHT);
		if (rights == null) {
			debug("ALL Rights Profile update: Entity Right not yet declared.");
			return;
		}
		if (rights.getMapper() == null) {
			debug("ALL Rights Profile update: Entity Right mapper not activated.");
			return;
		}
		final BeanMap p = profiles.dataSelectionFirst("", false, "code", "ALL"); //$NON-NLS-1 //$NON-NLS-2$$
		if ((p == null) || (p.getId() <= 0)) {
			warn("ALL Rigths Profile update: The profile code \"ALL\" not found in database, abort process.");
			return;
		}
		int changed = 0;
		final BeanMapList prs = profileRights.dataSelection(PROFILERIGHT_RIGHT, true, PROFILERIGHT_PROFILE, p.getId());
		debug(String.format("ALL Rights Profile update: The profile \"ALL\" (id:%d) currently contain %d Rights.", p.getId(), prs.size()));
		final BeanMapList list = rights.dataSelection();
		debug(String.format("ALL Rights Profile update: There is %d Rights to test.", list.size()));
		final EqualCriteria req = new EqualCriteria(PROFILERIGHT_RIGHT, 0);
		final AndCriteria test = new AndCriteria(new EqualCriteria(PROFILERIGHT_PROFILE, p.getId()), req); 
		for (BeanMap r: list) {
			if (r.getId() <= 0) {
				warn("ALL Rights Profile update: Some Rights are defined with null or negative ID !");
			} else if (prs.getFirst(PROFILERIGHT_RIGHT, r.getId()) == null) {
				synchronized (Activator.this) {
					// Add any missing right !
					req.setIntval(r.getId());
					if (profileRights.dataCount(true, test, false, null) == 0) {
						profileRights.dataCreate(PROFILERIGHT_PROFILE_RIGHT, p.getId(), r.getId());
						changed++;
					} else {
						debug(String.format("ALL Rights Profile update: The right %d was not in the original profile but the database found it anyway !", r.getId()));
					}
				}
			}
		}
		if (changed > 0) {
			info(String.format("ALL Rights Profile update: Profile updated. %d Rights added to Profile \"ALL\".", changed));
			debug("ALL Rights Profile update: Purge connection cache after Profile \"ALL\" update.");
			for (IConnectionCache cache: Activator.this.getServices(IConnectionCache.class)) {
				if (cache != null) {
					cache.purgeAll(Activator.TYPE_USER);
				}
			}
		} else if (list.size() > prs.size()) {
			warn("ALL Rights Profile Update: No right added to the profile but the list of rights is higher than the actual rights in this profile !");
		}
	}
}