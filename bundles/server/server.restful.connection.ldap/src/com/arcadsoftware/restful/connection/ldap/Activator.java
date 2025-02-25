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
package com.arcadsoftware.restful.connection.ldap;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Dictionary;
import java.util.HashMap;
import java.util.Hashtable;

import org.apache.felix.service.command.CommandProcessor;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;
import org.osgi.util.tracker.ServiceTracker;
import org.restlet.Context;
import org.restlet.data.Language;
import org.restlet.resource.ResourceException;
import org.restlet.routing.Router;

import com.arcadsoftware.rest.MultiLanguageMessages;
import com.arcadsoftware.rest.RouteList;
import com.arcadsoftware.rest.SimpleBranch;
import com.arcadsoftware.rest.connection.ConnectionUserBean;
import com.arcadsoftware.rest.connection.IAuthentificationService;
import com.arcadsoftware.rest.connection.IConnectionCache;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.unboundid.ldap.sdk.LDAPException;
import com.arcadsoftware.metadata.BeanMapEventTracker;
import com.arcadsoftware.metadata.IMetaDataModifyListener;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.criteria.DeletedCriteria;
import com.arcadsoftware.metadata.criteria.NotCriteria;
import com.arcadsoftware.osgi.AbstractConfiguredActivator;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.crypt.ConfiguredSSLContext;
import com.arcadsoftware.crypt.ConfiguredSSLContextException;
import com.arcadsoftware.crypt.Crypto;

public class Activator extends AbstractConfiguredActivator {

	public static final String TYPE_USER = "user"; //$NON-NLS-1$
	public static final String LDAPAUTH = "ldapauth"; //$NON-NLS-1$
	public static final String LDAPAUTH_LOGIN = "login"; //$NON-NLS-1$
	public static final String LDAPAUTH_USERID = "user"; //$NON-NLS-1$
	// Configuration properties:
	private static final String PROP_LOGIN_CASESENSITIVE = "login.casesensitive"; //$NON-NLS-1$
	// See LdapAuthentificationService for other properties !
	// Import/export facilities:
	public static final String USER_PROFILELINK = "profiles";
	private static final String HTTP_MESSAGES = Activator.class.getPackage().getName() + ".clientmessages"; //$NON-NLS-1$

	private ServiceTracker<IConnectionCache, IConnectionCache> connectionCacheTracker;
	private ServiceRegistration<IAuthentificationService> ldas;
	private boolean caseSensitive;
	private HashMap<Integer, BeanMap> authCache;
	private HashMap<String, Integer> authCacheLogin;
	private MultiLanguageMessages messages;
	private BeanMapEventTracker eventtracker;
	
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		caseSensitive = true;
		eventtracker = new BeanMapEventTracker(context);
		messages = new MultiLanguageMessages(HTTP_MESSAGES,Activator.class.getClassLoader());
		// Connection Cache access.
		connectionCacheTracker = new ServiceTracker<>(context, IConnectionCache.class, null);
		connectionCacheTracker.open();
		// Register a branch for password modifications.
		if (ConnectionUserBean.STANDALONECONNECTIONS == null) {
			registerService(SimpleBranch.clazz, new SimpleBranch() {
				@Override
				protected RouteList createAttachedResources(Context context, Router router) {
					return new RouteList(router.attach("/ldapauth/{login}", LdapAuthLoginResource.class), //$NON-NLS-1$
						router.attach("/ldapauth/{login}/{oldpassword}", new TestPasswordRestlet(context)), //$NON-NLS-1$
						router.attach("/ldapauth/{login}/{oldpassword}/{newpassword}", new TestPasswordRestlet(context)), //$NON-NLS-1$
						router.attach("/admin/ldap/import/isenabled", LDAPImportActivationResource.class), //$NON-NLS-1$
						router.attach("/admin/ldap/import", LDAPImportResource.class), //$NON-NLS-1$
						router.attach("/admin/ldap/import/{id}", LDAPImportResource.class)); //$NON-NLS-1$
				}
			}, SimpleBranch.properties(SimpleBranch.SECUREDBRANCH));
		}
		registerService(IMetaDataModifyListener.clazz, new LdapAuthModifyListener(this), IMetaDataModifyListener.PROP_TYPE, LDAPAUTH);
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		eventtracker.close();
		connectionCacheTracker.close();
		ldas = null;
	}
	
	@Override
	public boolean initializeConfiguration(Dictionary<String, Object> properties) {
		boolean changed = false;
		// Ascendant compatibility... as far as possible...
		if (properties != null) {
			Object o = properties.get("dn.attribute"); //$NON-NLS-1$
			if (o != null) {
				properties.remove("dn.attribute"); //$NON-NLS-1$
				if (properties.get(LdapAuthentificationService.PROP_LOGINATTRIBUTE) == null) {
					properties.put(LdapAuthentificationService.PROP_LOGINATTRIBUTE, o);
				}
				changed = true;
			}
			if (properties.get("dn.search") != null) { //$NON-NLS-1$
				properties.remove("dn.search"); //$NON-NLS-1$
				changed = true;
			}
			o = properties.get("login"); //$NON-NLS-1$
			if (o != null) {
				properties.remove("login"); //$NON-NLS-1$
				if (properties.get(LdapAuthentificationService.PROP_SEARCHUSER) == null) {
					properties.put(LdapAuthentificationService.PROP_SEARCHUSER, o);
				}
				changed = true;
			}
			o = properties.get("password"); //$NON-NLS-1$
			if (o != null) {
				properties.remove("password"); //$NON-NLS-1$
				if (properties.get(LdapAuthentificationService.PROP_SEARCHPWD) == null) {
					properties.put(LdapAuthentificationService.PROP_SEARCHPWD, o);
				}
				changed = true;
			}
			if (properties.get("password.encoded") != null) { //$NON-NLS-1$
				properties.remove("password.encoded"); //$NON-NLS-1$
				changed = true;
			}
			if (properties.get("password.simple") != null) { //$NON-NLS-1$
				properties.remove("password.simple"); //$NON-NLS-1$
				changed = true;
			}
			if (properties.get("user.class") != null) { //$NON-NLS-1$
				properties.remove("user.class"); //$NON-NLS-1$
				changed = true;
			}
			if (properties.get("user.dnpattern") != null) { //$NON-NLS-1$
				properties.remove("user.dnpattern"); //$NON-NLS-1$
				changed = true;
			}
			if (properties.get("user.forcebinding") != null) { //$NON-NLS-1$
				properties.remove("user.forcebinding"); //$NON-NLS-1$
				changed = true;
			}
			if (properties.get("ssl.type") != null) { //$NON-NLS-1$
				properties.remove("ssl.type"); //$NON-NLS-1$
				changed = true;
			}
			if (properties.get("attributes.expired") != null) { //$NON-NLS-1$
				properties.remove("attributes.expired"); //$NON-NLS-1$
				changed = true;
			}
			if (properties.get("attributes.locked") != null) { //$NON-NLS-1$
				properties.remove("attributes.locked"); //$NON-NLS-1$
				changed = true;
			}
			if (properties.get("attributes.timemap") != null) { //$NON-NLS-1$
				properties.remove("attributes.timemap"); //$NON-NLS-1$
				changed = true;
			}
			if (properties.get("public.system.parameters") != null) { //$NON-NLS-1$
				properties.remove("public.system.parameters"); //$NON-NLS-1$
				changed = true;
			}
			// dn.password and attribute.password do not contain passwords !!!
			o = properties.get("dn.password"); //$NON-NLS-1$
			if (o != null) {
				properties.remove("dn.password"); //$NON-NLS-1$
				if (properties.get(LdapAuthentificationService.PROP_PWDATTRIBUTE) == null) {
					properties.put(LdapAuthentificationService.PROP_PWDATTRIBUTE, o);
				}
				changed = true;
			}
			o = properties.get("dn.login"); //$NON-NLS-1$
			if (o != null) {
				properties.remove("dn.login"); //$NON-NLS-1$
				if (properties.get(LdapAuthentificationService.PROP_LOGINATTRIBUTE) == null) {
					properties.put(LdapAuthentificationService.PROP_LOGINATTRIBUTE, o);
				}
				changed = true;
			}
			// Force encryption of non encrypted passwords...
			o = properties.get(LdapAuthentificationService.PROP_SEARCHPWD);
			if ((o != null) && !Crypto.isCryptSecure(o.toString().trim())) {
				properties.put(LdapAuthentificationService.PROP_SEARCHPWD, Crypto.encrypt(Crypto.decrypt(o.toString().trim())));
				changed = true;
			}
			o = properties.get(ConfiguredSSLContext.PROP_KEYSTORE_KEYPWD);
			if ((o != null) && !Crypto.isCryptSecure(o.toString().trim())) {
				properties.put(ConfiguredSSLContext.PROP_KEYSTORE_KEYPWD, Crypto.encrypt(Crypto.decrypt(o.toString().trim())));
				changed = true;
			}
			o = properties.get(ConfiguredSSLContext.PROP_KEYSTORE_PWD);
			if ((o != null) && !Crypto.isCryptSecure(o.toString().trim())) {
				properties.put(ConfiguredSSLContext.PROP_KEYSTORE_PWD, Crypto.encrypt(Crypto.decrypt(o.toString().trim())));
				changed = true;
			}
			o = properties.get(ConfiguredSSLContext.PROP_TRUSTSTORE_PWD);
			if ((o != null) && !Crypto.isCryptSecure(o.toString().trim())) {
				properties.put(ConfiguredSSLContext.PROP_TRUSTSTORE_PWD, Crypto.encrypt(Crypto.decrypt(o.toString().trim())));
				changed = true;
			}
		}
		return changed;
	}

	@Override
	public void updatedConfiguration(Dictionary<String,Object> properties) {
		if (properties != null) {
			// Unregister previous service.
			if (ldas != null) {
				unregister(ldas);
				ldas = null;
			}
			purgeConnectionCache();
			caseSensitive = parseBooleanParameter(properties.get(PROP_LOGIN_CASESENSITIVE), true);
			// Create a new authentification service.
			if (LdapAuthentificationService.isConfigurationComplete(properties)) {
				try {
					Dictionary<String, Object> props = new Hashtable<>();
					props.put(LdapAuthentificationService.ENTITYNAME, LDAPAUTH);
					props.put(LdapAuthentificationService.PRIORITY, 5);
					props.put(CommandProcessor.COMMAND_SCOPE, "arcad"); //$NON-NLS-1$
					props.put(CommandProcessor.COMMAND_FUNCTION, new String[] {"bind"}); //$NON-NLS-1$
					ldas = registerService(IAuthentificationService.class, new LdapAuthentificationService(this, properties), props);
				} catch (LDAPException | ConfiguredSSLContextException e) {
					error("LDAP Connection invalid configuration.", e);
				}
			}
		}
	}
	
	public boolean isLoginCaseSensitive() {
		return caseSensitive;
	}
	
	public void purgeConnectionCache(int userId) {
		if (connectionCacheTracker != null) {
			IConnectionCache connectionCache = connectionCacheTracker.getService();
			if (connectionCache != null) {
				connectionCache.purge(TYPE_USER, userId);
			}
		}
	}

	public void purgeConnectionCache() {
		if (connectionCacheTracker != null) {
			IConnectionCache connectionCache = connectionCacheTracker.getService();
			if (connectionCache != null) {
				connectionCache.purgeAll();
			}
		}
	}

	protected void cacheClear() {
		authCache = null;
		authCacheLogin = null;
	}
	
	private void initializeAuthCache() {
		MetaDataEntity entity = MetaDataEntity.loadEntity(LDAPAUTH);
		if ((entity == null) || (entity.getMapper() == null)) {
			return;
		}
		BeanMapList list = null;
		try {
			list = entity.dataSelection(null, false, new NotCriteria(new DeletedCriteria(LDAPAUTH_USERID)), false, null, null, 0, -1);
		} catch (ResourceException e) {
			debug(e);
		}
		if (list != null) {
			authCache = new HashMap<Integer, BeanMap>();
			authCacheLogin = new HashMap<String, Integer>();
			String s;
			for (BeanMap bm: list) {
				int uid = bm.getInt(LDAPAUTH_USERID);
				if (uid > 0) {
					authCache.put(uid, bm);
					s = (String) bm.get(LDAPAUTH_LOGIN);
					if (s != null) {
						if (!caseSensitive) {
							s = s.toLowerCase();
						}
						authCacheLogin.put(s, uid);
					}
				}
			}
		}
	}

	public BeanMap getAuth(int id) {
		if (authCache == null) {
			initializeAuthCache();
			if (authCache == null) {
				return null;
			}
		}
		return authCache.get(id);
	}
	
	public int getAuth(String login) {
		if ((login == null) || login.isEmpty()) {
			return 0;
		}
		if (authCacheLogin == null) {
			initializeAuthCache();
			if (authCacheLogin == null) {
				return 0;
			}
		}
		if (!caseSensitive) {
			login = login.toLowerCase();
		}
		Integer i = authCacheLogin.get(login);
		if (i == null) {
			return 0;
		}
		return i;
	}
	
	public void setAuth(BeanMap auth, String oldlogin, int oldUid) {
		if (authCache == null) {
			initializeAuthCache();
			if (authCache == null) {
				return;
			}
		}
		if (oldUid > 0) {
			authCache.remove(oldUid);
		}
		if (auth != null) {
			oldUid = auth.getInt(LDAPAUTH_USERID);
			if (oldUid > 0) {
				authCache.put(oldUid, auth);
			}
		}
		if (oldlogin != null) {
			if (!caseSensitive) {
				oldlogin = oldlogin.toLowerCase();
			}
			authCacheLogin.remove(oldlogin);
		}
		if (auth != null) {
			oldUid = auth.getInt(LDAPAUTH_USERID);
			oldlogin = (String) auth.get(LDAPAUTH_LOGIN);
			if (oldlogin != null) {
				if (!caseSensitive) {
					oldlogin = oldlogin.toLowerCase();
				}
				authCacheLogin.put(oldlogin, oldUid);
			}
		}
	}

	public Collection<String> getAuthLogins() {
		if (authCacheLogin == null) {
			initializeAuthCache();
			if (authCacheLogin == null) {
				return new ArrayList<String>();
			}
		}
		return authCacheLogin.keySet();
	}
	
	public String getMessage(String key, Language language) {
		return messages.get(key, language);
	}

	public void fireBeanMapEvent(String topic, BeanMap bean, IConnectionUserBean user) {
		eventtracker.fireEvent(topic, bean, user);
	}
}
