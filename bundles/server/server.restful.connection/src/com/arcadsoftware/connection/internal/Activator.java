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
package com.arcadsoftware.connection.internal;

import java.io.File;
import java.util.ArrayList;
import java.util.Dictionary;
import java.util.List;

import org.eclipse.osgi.framework.console.CommandInterpreter;
import org.eclipse.osgi.framework.console.CommandProvider;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;
import org.osgi.util.tracker.ServiceTracker;
import org.restlet.Request;
import org.restlet.data.Language;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.osgi.AbstractConfiguredActivator;
import com.arcadsoftware.rest.MultiLanguageMessages;
import com.arcadsoftware.rest.connection.ConnectionUserBean;
import com.arcadsoftware.rest.connection.IAuthentificationService;
import com.arcadsoftware.rest.connection.IBasicAuthentificationService;
import com.arcadsoftware.rest.connection.IConnectionCache;
import com.arcadsoftware.rest.connection.IConnectionCredential;
import com.arcadsoftware.rest.connection.IConnectionInfoService;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.arcadsoftware.rest.connection.IConnectionUserCheck;
import com.arcadsoftware.rest.connection.IConnectionUserRepository;

/**
 * Bundle Activator.
 */
public class Activator extends AbstractConfiguredActivator implements CommandProvider, IConnectionUserRepository {

	private static Activator instance;
	private static final String HTTP_MESSAGES = Activator.class.getPackage().getName() + ".clientmessages"; //$NON-NLS-1$
	private static final MultiLanguageMessages messages = new MultiLanguageMessages(HTTP_MESSAGES, Activator.class.getClassLoader());
	/*
	 * Avoid to recall the connection provider too early for the same connection.
	 * <p>
	 * This assume that during this period of time any revocation of the login will be ignored.
	 * If the password is changed, either the old password or the new one will be accepted.
	 */
	private static final String PROP_CACHEDURATION = "cache"; //$NON-NLS-1$
	// Disable any connection to this server...
	private static final String PROP_INACTIVATE = "inactive"; //$NON-NLS-1$
	// Define the HTTP BASIC realm.
	private static final String PROP_REALM = "realm"; //$NON-NLS-1$
	private static final int CACHE_DURATION = 30; // in seconds

	public static String getMessage(String key, Language language) {
		return messages.get(key, language);
	}

	/**
	 * Return the activator first instance.
	 */
	public static Activator getInstance() {
		return instance;
	}

	private int cacheDuration = CACHE_DURATION;
	private ConnectionCache cache;
	private ServiceTracker<IAuthentificationService, IAuthentificationService> secTracker;
	private ServiceTracker<IConnectionInfoService, IConnectionInfoService> userTracker;
	private ServiceTracker<IConnectionUserCheck, IConnectionUserCheck> checkTracker;
	private boolean inactivate;
	private String realm = "arcad-evolution"; //$NON-NLS-1$
	private ServiceRegistration<?> breg;
	
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		if (instance == null) {
			instance = this;
		}
		secTracker = new ServiceTracker<IAuthentificationService, IAuthentificationService>(context, IAuthentificationService.clazz, null);
		secTracker.open();
		userTracker = new ServiceTracker<IConnectionInfoService, IConnectionInfoService>(context, IConnectionInfoService.clazz, null);
		userTracker.open();
		checkTracker = new ServiceTracker<IConnectionUserCheck, IConnectionUserCheck>(context, IConnectionUserCheck.class, null);
		checkTracker.open();
		// Add a secure branch, each authentification scheme must add a new secure branch.
		breg = registerService(SecureBranch.clazz, new SecureBranch(this), SecureBranch.properties("/")); //$NON-NLS-1$
		// Propose the Connection cache as a OSGi service.
		cache = new ConnectionCache(this);
		registerService(IConnectionCache.clazz, cache);
		registerService(IConnectionUserRepository.clazz, this);
		// Register itself as a console command provider.
		registerService(CommandProvider.class.getName(), this);
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		if (instance == this) {
			instance = null;
		}
		cache.purgeAll();
		cache = null;
		secTracker.close();
		secTracker = null;
		userTracker.close();
		userTracker = null;
		checkTracker.close();
		checkTracker = null;
		super.stop(context);
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean initializeConfiguration(@SuppressWarnings("rawtypes") Dictionary properties) {
		boolean result = false;
		if (properties.get(PROP_CACHEDURATION) == null) {
			properties.put(PROP_CACHEDURATION, Integer.toString(CACHE_DURATION));
			result = true;
		}
		// Add other parameters initialization here...
		return result;
	}

	@Override
	public void updatedConfiguration(@SuppressWarnings("rawtypes") Dictionary properties) {
		if (properties == null) {
			return;
		}
		if (properties.get(PROP_CACHEDURATION) != null) {
			cacheDuration = parseIntegerParameter(properties.get(PROP_CACHEDURATION).toString(), cacheDuration);
		}
		if (properties.get(PROP_INACTIVATE) != null) {
			inactivate = "true".equalsIgnoreCase(properties.get(PROP_INACTIVATE).toString()); //$NON-NLS-1$
		}
		if ((properties.get(PROP_REALM) != null) && !realm.equals(properties.get(PROP_REALM).toString())) {
			realm = properties.get(PROP_REALM).toString();
			unregister(breg);
			breg = registerService(SecureBranch.clazz, new SecureBranch(this), SecureBranch.properties("/")); //$NON-NLS-1$
		}
	}

	public int getCacheDuration() {
		return cacheDuration;
	}

	public String getHelp() {
		return Messages.Activator_Console_Command_Help+
		Messages.Activator_Console_Command_Help_Cache;
	}

	/**
	 * Cache console command.
	 */
	public void _cache(CommandInterpreter ci) throws Exception {
		String option = ci.nextArgument();
		if (option == null) {
			ci.println(Messages.Activator_Console_Command_Cache_Containt1 + cache.count() + Messages.Activator_Console_Command_Cache_Containt2);
		} else if (option.equalsIgnoreCase("clear")) { //$NON-NLS-1$
			cache.purgeAll();
			// propagate the clean operation to locals caches...
			Object[] services = secTracker.getServices();
			if (services != null) {
				for (Object service:services) {
					((IAuthentificationService) service).purgeConnectionCache();
				}
			}
			ci.println(Messages.Activator_Console_Command_Cache_cleared);
		} else {
			String s = ci.nextArgument();
			String utype;
			if (s != null) {
				utype = option;
				option = s;
			} else {
				utype = "user"; //$NON-NLS-1$
			}
			try {
				int id = Integer.valueOf(option);
				cache.purge(utype,id);
				// propagate the clean operation to locals caches...
				Object[] services = secTracker.getServices();
				if (services != null) {
					for (Object service:services) {
						((IAuthentificationService) service).purgeConnectionCache(id);
					}
				}
				ci.println(Messages.Activator_Console_Command_Cache_user + id);
			} catch (NumberFormatException e) {
				ci.println(Messages.Activator_Console_Command_Error_Option + option);
			}
		}
	}

	public IAuthentificationService[] getAuthentificationServices() {
		ServiceReference<IAuthentificationService>[] services = secTracker.getServiceReferences();
		if (services == null) {
			return new IAuthentificationService[0];
		}
		IAuthentificationService[] result = new IAuthentificationService[services.length];
		int p = Integer.MAX_VALUE;
		int i = 0;
		while (i < services.length) {
			int z = Integer.MIN_VALUE;
			for (ServiceReference<IAuthentificationService> service: services) {
				int priority = 0;
				Object o = service.getProperty(IAuthentificationService.PRIORITY);
				if (o != null) {
					if (o instanceof Integer) {
						priority = (Integer) o;
					} else {
						try {
							priority = Integer.parseInt(o.toString());
						} catch (NumberFormatException e) {}
					}
				}
				if ((priority > z) && (priority < p)) {
					z = priority;
				}
			}
			p = z;
			for (ServiceReference<IAuthentificationService> service: services) {
				int priority = 0;
				Object o = service.getProperty(IAuthentificationService.PRIORITY);
				if (o != null) {
					if (o instanceof Integer) {
						priority = (Integer) o;
					} else {
						try {
							priority = Integer.parseInt(o.toString());
						} catch (NumberFormatException e) {}
					}
				}
				if (priority == p) {
					result[i++] = secTracker.getService(service);
				}
			}
		}
		return result;
	}

	public List<String> getAuthentificationEntities() {
		ArrayList<String> list = new ArrayList<String>();
		for (ServiceReference<IAuthentificationService> sr: secTracker.getServiceReferences()) {
			Object o = sr.getProperty(IAuthentificationService.ENTITYNAME);
			if (o != null) {
				String v = o.toString();
				if ((v.length() > 0) && !list.contains(v)) {
					list.add(v);
				}
			}
		}
		return list;
	}
	
	public boolean isInactivate() {
		return inactivate;
	}

	public File getSchema(String path) {
		return toFile(getContext().getBundle().getEntry(path));
	}

	public String getRealm() {
		return realm;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public ConnectionUserBean createUser(String userType, int id) {
		ServiceReference[] refs = userTracker.getServiceReferences();
		if (refs != null) {
			for (ServiceReference ref: refs) {
				ConnectionUserBean user = ((IConnectionInfoService) getContext().getService(ref)).loadUser(userType, id);
				if (user != null) {
					if (user.getUserType() == null) { //Blindage...
						user.setUserType(userType);
					}
					return user;
				}
			}
		}
		return null;
	}

	public ConnectionUserBean getCachedUser(String uid) {
		return cache.get((Object)uid);
	}

	public void cachePurge(String userType, int id) {
		cache.purge(userType,id);
	}

	public void cachePut(ConnectionUserBean user) {
		cache.put(user);
	}

	public void cachepurgeAll() {
		cache.purgeAll();
	}

	public IConnectionUserBean getUser(String userType, int userId) {
		ConnectionUserBean user = cache.getUser(userType, userId);
		if (user == null) {
			user = createUser(userType, userId);
			if (user == null) {
				return null;
			}
			cache.put(user);
		}
		return (IConnectionUserBean) user.clone();
	}
	
	public IConnectionUserBean getUser(String login) {
		// Le login est en fait un identifiant unique d'utilisateur...
		// Principe de l'identification "PréfixeProvider:login"
		int i = login.indexOf(':');
		if (i > 0) {
			ConnectionUserBean user = getCachedUser(login);
			if (user != null) {
				user = user.clone();
				user.setLogin(login.substring(i+1));
				return user;
			}
		}
		// A défaut un tente d'utiliser le "login" comme un identifiant de connexion.
		// Cela ne fonctionnera pas avec les mode de connexion utilisant d'autres
		// entètes HTTP...
		IAuthentificationService[] services = getAuthentificationServices();
		if (services == null) {
			debug(Messages.SecureGuard_NoAuthentificationService);
			return null;
		}
		IConnectionCredential credential = null;
		for (IAuthentificationService o: services) {
			if (o instanceof IBasicAuthentificationService) {
				credential = ((IBasicAuthentificationService) o).generateCredential(new Request(), login);
				if (credential != null) {
					break;
				}
			}
		}
		if (credential == null) {
			return null;
		}
		// 2. on recherche se credential dans le cache, grâce à son UniqueID.
		String uid = credential.getUniqueId();
		ConnectionUserBean user = getCachedUser(uid);
		if (user == null) {
			user = createUser(credential.getUserType(), credential.loadUserId());
			if (user == null) {
				return null;
			}
			// Là on peut ajouter l'utilisateur (sans lui affecter le Credential qui n'a pas été validé).
			cache.put(user);
		}
		user = user.clone();
		user.setLogin(login.substring(i+1));
		return user;
	}

	public List<String> getUserLogins(String userType, int userId) {
		ArrayList<String> result = new ArrayList<String>();
		Object[] services = secTracker.getServices();
		if (services != null) {
			for (Object service: services) {
				result.addAll(((IAuthentificationService) service).getUserLogins(userType, userId));
				// TODO Vérifier qu'il n'y a pas de doublons !
			}
		}
		return result;
	}
	
	protected void checkUserConnection(IConnectionUserBean user) throws ResourceException {
		Object[] services = checkTracker.getServices();
		if (services != null) {
			for (Object service: services) {
				if (service instanceof IConnectionUserCheck) {
					((IConnectionUserCheck) service).check(user);
				}
			}
		}
	}
}
