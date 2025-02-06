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
package com.arcadsoftware.restful.connection.config.internal;

import java.util.ArrayList;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map.Entry;

import org.eclipse.osgi.framework.console.CommandInterpreter;
import org.eclipse.osgi.framework.console.CommandProvider;
import org.osgi.framework.BundleContext;
import org.restlet.Context;
import org.restlet.routing.Router;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.osgi.AbstractConfiguredActivator;
import com.arcadsoftware.rest.RouteList;
import com.arcadsoftware.rest.SimpleBranch;

public class Activator extends AbstractConfiguredActivator implements CommandProvider {

	private static final String CONFIGAUTH = "configauth"; //$NON-NLS-1$
	private static Activator instance;
	
	public static Activator getInstance() {
		return instance;
	}

	@Override
	public void start(BundleContext bundleContext) throws Exception {
		instance = this;
		super.start(bundleContext);
		registerService(CommandProvider.class.getName(), this);
		Dictionary<String, Object> props = new Hashtable<>();
		props.put(AuthentificationService.ENTITYNAME, CONFIGAUTH);
		props.put(AuthentificationService.PRIORITY, 1);
		registerService(AuthentificationService.clazz, new AuthentificationService(this), props);
		registerService(SimpleBranch.clazz, new SimpleBranch() {
			protected RouteList createAttachedResources(Context context, Router router) {
				RouteList routes = new RouteList(router.attach("/configauth/{login}",new AuthLoginRestlet(context))); //$NON-NLS-1$
				routes.add(router.attach("/configauth/",AuthListResource.class)); //$NON-NLS-1$
				return routes;
			}
		}, SimpleBranch.properties(SimpleBranch.SECUREDBRANCH));
	}

	@Override
	public void stop(BundleContext bundleContext) throws Exception {
		super.stop(bundleContext);
		instance = null;
	}

	@Override
	public boolean initializeConfiguration(Dictionary<String, Object> properties) {
		if (properties != null) {
			Enumeration<String> keys = properties.keys();
			HashMap<String, Object> changes = new HashMap<String, Object>();
			boolean removeOldAdminAtQuadra = false;
			while (keys.hasMoreElements()) {
				String key = keys.nextElement();
				if (!key.startsWith("service.")) { //$NON-NLS-1$
					Object o = properties.get(key);
					if (o  != null) {
						String s = o.toString().trim();
						int p = s.indexOf(' ');
						if (p > 0) {
							String pwd = s.substring(p + 1).trim();
							if ((pwd.length() < 128) && !Crypto.isHashSecure(pwd)) {
								changes.put(key, s.substring(0, p).trim() + ' ' + Crypto.hash(pwd.toCharArray()));
							}
						}
					}
					// Legacy initialization "realm.login" conversion to new format...
					if (key.equalsIgnoreCase("quadra.admin")) { //$NON-NLS-1$
						removeOldAdminAtQuadra = true;
						Object value = changes.get(key);
						if (value == null) {
							value = o;
						} else {
							changes.remove(key);
						}
						changes.put("admin@quadra", value); //$NON-NLS-1$
					}
				}
			}
			for (Entry<String, Object> e : changes.entrySet()) {
				properties.put(e.getKey(), e.getValue());
			}
			if (removeOldAdminAtQuadra) {
				properties.remove("quadra.admin"); //$NON-NLS-1$
			}
			return !changes.isEmpty();
		}
		return false;
	}

	public ConnectionCredential getConnectionCredential(String login) {
		Object o = getCurrentConfiguration().get(login);
		if (o == null) {
			return null;
		}
		String os = o.toString().trim();
		int i = os.indexOf(' ');
		if (i <= 0) {
			return null;
		}
		try {
			return new ConnectionCredential(this, login, Integer.parseInt(os.substring(0, i).trim()), os.substring(i + 1).trim());
		} catch (NumberFormatException e) {
			debug(e);
			return null;
		}
	}

	@SuppressWarnings("unchecked")
	public void setConnectionCredentials(String login, int id, String hash) {
		@SuppressWarnings("rawtypes")
		Dictionary prop = getCurrentConfiguration();
		prop.put(login, Integer.toString(id) + ' ' + hash);
		debug(String.format(Messages.Activator_Debug_ConnectionChanged, login));
		updateConfiguration(prop);
	}

	public String getHelp() {
		return Messages.Activator_Help;
	}

	public void _hash(CommandInterpreter ci) throws Exception {
		StringBuilder pwd = new StringBuilder(ci.nextArgument());
		String s = ci.nextArgument();
		while (s != null) {
			pwd.append(' ');
			pwd.append(s);
			s = ci.nextArgument();
		}
		if (pwd.length() == 0) {
			ci.println(Messages.Activator_HashUsage);
			return;
		}
		ci.println(Messages.Activator_HashPassword);
		ci.print("     "); //$NON-NLS-1$
		ci.println(Crypto.hash(pwd.toString().toCharArray()));
	}
	
	public void _connectionConf(CommandInterpreter ci) throws Exception {
		String login = ci.nextArgument();
		if (login == null) {
			listRealm(ci);
			return;
		}
		String id = ci.nextArgument();
		if (id == null) {
			listLogin(login, ci);
			return;
		}
		StringBuilder pwd = new StringBuilder();
		String s = ci.nextArgument();
		while (s != null) {
			if (pwd.length() > 0) {
				pwd.append(' ');
			}
			pwd.append(s);
			s = ci.nextArgument();
		}
		if (pwd.length() == 0) {
			ci.println(Messages.Activator_Error_EmptyPassword);
		}
		try {
			setConnectionCredentials(login, Integer.parseInt(id), Crypto.hash(pwd.toString().toCharArray()));
		} catch (NumberFormatException e) {
			ci.println(Messages.Activator_Error_IdInteger);
		}
	}

	private void listLogin(String login, CommandInterpreter ci) {
		Dictionary<?, ?> prop = getCurrentConfiguration();
		Object o = prop.get(login);
		if (o == null) {
			ci.println(Messages.Activator_NoLoginDetails);
			return;
		}
		String os = o.toString().trim();
		int i = os.indexOf(' ');
		if (i <= 0) {
			ci.println(Messages.Activator_NoLoginDetails);
			return;
		}
		try {
			int id = Integer.parseInt(os.substring(0, i).trim());
			if (id > 0) {
				ci.println(String.format(Messages.Activator_LoginDetailsUser, login, id));
			} else {
				ci.println(String.format(Messages.Activator_LoginDetailsSpecial, login, -id));
			}
		} catch (NumberFormatException e) {
			ci.println(Messages.Activator_NoLoginDetails);
			return;
		}
	}

	private void listRealm(CommandInterpreter ci) {
		Dictionary<?, ?> prop = getCurrentConfiguration();
		Enumeration<?> e = prop.keys();
		if (!e.hasMoreElements()) {
			ci.println(Messages.Activator_NoLogin);
		} else {
			ci.print(Messages.Activator_LoginList);
			while (e.hasMoreElements()) {
				String key = e.nextElement().toString();
				Object o = prop.get(key);
				if ((o != null) && (o.toString().trim().indexOf(' ') > 0)) {
					ci.println(key);
				}
			}
		}
	}

	public boolean exists(String login) {
		Dictionary<?, ?> prop = getCurrentConfiguration();
		Object o = prop.get(login);
		return (o != null) && (o.toString().trim().length() > 0);
	}
	
	public ArrayList<LoginBean> getLogins() {
		ArrayList<LoginBean> result = new ArrayList<LoginBean>();
		Dictionary<?, ?> prop = getCurrentConfiguration();
		if (prop != null) {
			Enumeration<?> e = prop.keys();
			while(e.hasMoreElements()) {
				String key = e.nextElement().toString();
				Object o = prop.get(key);
				if (o != null) {
					String s = o.toString().trim();
					int i = s.indexOf(' ');
					if (i > 0) {
						try {
							i = Integer.parseInt(s.substring(0, i).trim());
							if (i > 0) {
								result.add(new LoginBean(key, i));
							}
						} catch (NumberFormatException ex) {}
					}
				}
			}
		}
		return result;
	}
}
