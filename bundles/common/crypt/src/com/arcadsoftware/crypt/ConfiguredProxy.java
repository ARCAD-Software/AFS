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
package com.arcadsoftware.crypt;

import java.net.Authenticator;
import java.net.InetSocketAddress;
import java.net.PasswordAuthentication;
import java.net.Proxy;
import java.net.Proxy.Type;
import java.util.Dictionary;
import java.util.Map;

/**
 * Simple wrapper around the Java Proxy class, allowing to declare the proxy properties in a configuration properties file.
 * 
 * <p>
 * This class assume that the followinfg properties are defined in the given configuration. Note that these properties may be prefixed by any string.
 * 
 * <ul>
 * <li>proxy.type
 * <li>proxy.hostname and proxy.port (default value 8080).
 * <li>proxy.login and proxy.password (may be encrypted with the master key).
 * </ul>
 * 
 * @author ARCAD Software
 */
public class ConfiguredProxy {
	
	private class ProxyAuthenticator extends Authenticator {

		private final String proxyUser;
		private final char[] proxyPassword;

		protected ProxyAuthenticator(String proxyUser, char[] proxyPassword) {
			super();
			this.proxyUser = proxyUser;
			this.proxyPassword = proxyPassword;
		}
		
		@Override
        protected PasswordAuthentication getPasswordAuthentication() {
            // Verify that the authentication request is for the proxy
			if (getRequestorType() == RequestorType.PROXY) {
				return new PasswordAuthentication(proxyUser, proxyPassword);
            }
            return super.getPasswordAuthentication();
        }
    };
    
	public static final String PROP_PROXY_TYPE = "proxy.type"; //$NON-NLS-1$
	public static final String PROP_PROXY_HOSTNAME = "proxy.hostname"; //$NON-NLS-1$
	public static final String PROP_PROXY_PORT = "proxy.port"; //$NON-NLS-1$
	public static final String PROP_PROXY_LOGIN = "proxy.login"; //$NON-NLS-1$
	public static final String PROP_PROXY_PASSWORD = "proxy.password"; //$NON-NLS-1$
	
	private final Proxy proxy;
	private final ProxyAuthenticator authenticator;
	
	/**
	 * Create the required Proxy client objects from the given configuration.
	 * 
	 * @param props the configuration properties.
	 */
	public ConfiguredProxy(Dictionary<String, Object> props) {
		this("", getType(props.get(PROP_PROXY_TYPE)), props); //$NON-NLS-1$
	}
	
	/**
	 * Create the required Proxy client objects from the given configuration.
	 * 
	 * @param props the configuration properties.
	 * @param prefix a prefix which is appended to the properties names. 
	 */
	public ConfiguredProxy(Dictionary<String, Object> props, String prefix) {
		this(prefix, getType(props.get(prefix + PROP_PROXY_TYPE)), props);
	}

	/**
	 * Create the required Proxy client objects from the given configuration.
	 * 
	 * @param type The proxy type (http, https, tpc or socks).
	 * @param props the configuration properties.
	 */
	public ConfiguredProxy(String type, Dictionary<String, Object> props) {
		this("", getType(type), props); //$NON-NLS-1$
	}
	
	/**
	 * Create the required Proxy client objects from the given configuration.
	 * 
	 * @param prefix a prefix which is appended to the properties names. 
	 * @param type The proxy type (http, https, tpc or socks).
	 * @param props the configuration properties.
	 */
	public ConfiguredProxy(String prefix, String type, Dictionary<String, Object> props) {
		this(prefix, getType(type), props);
	}

	/**
	 * Create the required Proxy client objects from the given configuration.
	 * 
	 * @param type The proxy type.
	 * @param props the configuration properties.
	 */
	public ConfiguredProxy(Type type, Dictionary<String, Object> props) {
		this("", type, props); //$NON-NLS-1$
	}

	/**
	 * Create the required Proxy client objects from the given configuration.
	 * 
	 * @param prefix a prefix which is appended to the properties names. 
	 * @param type The proxy type.
	 * @param props the configuration properties.
	 */
	public ConfiguredProxy(String prefix, Type type, Dictionary<String, Object> props) {
		super();
		InetSocketAddress a = getAddress(prefix, props);
		if ((type != Type.DIRECT) && (a != null)) {
			proxy = new Proxy(type, a);
			authenticator = createAuthenticator(props.get(prefix + PROP_PROXY_LOGIN), props.get(prefix + PROP_PROXY_PASSWORD));
		} else {
			proxy = null;
			authenticator = null;
		}
	}
	
	/**
	 * Create the required Proxy client objects from the given configuration.
	 * 
	 * @param props the configuration properties.
	 */
	public ConfiguredProxy(Map<String, Object> props) {
		this("", getType(props.get(PROP_PROXY_TYPE)), props); //$NON-NLS-1$
	}
	
	/**
	 * Create the required Proxy client objects from the given configuration.
	 * 
	 * @param props the configuration properties.
	 * @param prefix a prefix which is appended to the properties names. 
	 */
	public ConfiguredProxy(Map<String, Object> props, String prefix) {
		this(prefix, getType(props.get(prefix + PROP_PROXY_TYPE)), props);
	}
	
	/**
	 * Create the required Proxy client objects from the given configuration.
	 * 
	 * @param type The proxy type (http, https, tpc or socks).
	 * @param props the configuration properties.
	 */
	public ConfiguredProxy(String type, Map<String, Object> props) {
		this("", getType(type), props); //$NON-NLS-1$
	}

	/**
	 * Create the required Proxy client objects from the given configuration.
	 * 
	 * @param prefix a prefix which is appended to the properties names. 
	 * @param type The proxy type (http, https, tpc or socks).
	 * @param props the configuration properties.
	 */
	public ConfiguredProxy(String prefix, String type, Map<String, Object> props) {
		this(prefix, getType(type), props);
	}
	

	/**
	 * Create the required Proxy client objects from the given configuration.
	 * 
	 * @param type The proxy type.
	 * @param props the configuration properties.
	 */
	public ConfiguredProxy(Type type, Map<String, Object> props) {
		this("", type, props); //$NON-NLS-1$
	}
	

	/**
	 * Create the required Proxy client objects from the given configuration.
	 * 
	 * @param prefix a prefix which is appended to the properties names. 
	 * @param type The proxy type.
	 * @param props the configuration properties.
	 */
	public ConfiguredProxy(String prefix, Type type, Map<String, Object> props) {
		super();
		InetSocketAddress a = getAddress(prefix, props);
		if ((type != Type.DIRECT) && (a != null)) {
			proxy = new Proxy(type, a);
			authenticator = createAuthenticator(props.get(prefix + PROP_PROXY_LOGIN), props.get(prefix + PROP_PROXY_PASSWORD));
		} else {
			proxy = null;
			authenticator = null;
		}
	}
	
	private ProxyAuthenticator createAuthenticator(Object login, Object pwd) {
		if ((login instanceof String) && !((String) login).isEmpty()) {
			return new ProxyAuthenticator((String) login, Crypto.decrypt((String) pwd));
		}
		return null;
	}

	private InetSocketAddress getAddress(String prefix, Dictionary<String, Object> props) {
		Object o = props.get(prefix + PROP_PROXY_HOSTNAME);
		if ((o != null) && !o.toString().isEmpty()) {
			Object po = props.get(prefix + PROP_PROXY_PORT);
			int p = 8080;
			if (po != null) {
				try {
					p = Integer.parseInt(po.toString());
				} catch (NumberFormatException e) {}
			}
			return new InetSocketAddress(o.toString(), p);
		}
		return null;
	}
	
	private InetSocketAddress getAddress(String prefix, Map<String, Object> props) {
		Object o = props.get(prefix + PROP_PROXY_HOSTNAME);
		if ((o != null) && !o.toString().isEmpty()) {
			Object po = props.get(prefix + PROP_PROXY_PORT);
			int p = 8080;
			if (po != null) {
				try {
					p = Integer.parseInt(po.toString());
				} catch (NumberFormatException e) {}
			}
			return new InetSocketAddress(o.toString(), p);
		}
		return null;
	}

	private static Type getType(Object t) {
		if (t != null) {
			String s = t.toString().trim();
			if ("http".equalsIgnoreCase(s) || //$NON-NLS-1$
					"https".equalsIgnoreCase(s)) { //$NON-NLS-1$
				return Type.HTTP;
			}
			if ("tcp".equalsIgnoreCase(s) || //$NON-NLS-1$
					"socks".equalsIgnoreCase(s) || //$NON-NLS-1$
					"sock".equalsIgnoreCase(s) || //$NON-NLS-1$
					"sockets".equalsIgnoreCase(s) || //$NON-NLS-1$
					"socket".equalsIgnoreCase(s)) { //$NON-NLS-1$
				return Type.SOCKS;
			}
		}
		return Type.DIRECT;
	}
	
	/**
	 * @return true is and only is a proxy connection is included in the given configuration. 
	 */
	public boolean isActive() {
		return proxy != null;
	}
	
	/**
	 * Get the Proxy Object.
	 *  
	 * @return null if there is no proxy configured.
	 */
	public Proxy getProxy() {
		return proxy;
	}
	
	/**
	 * Get the Proxy Authenticator.
	 * 
	 * @return null if there is no proxy authentication configured.
	 */
	public Authenticator getAuthenticator() {
		return authenticator;
	}

	/**
	 * Get the Proxy login if any.
	 * 
	 * @return null if there is no login in the configuration.
	 */
	public String getLogin() {
		if (authenticator != null) {
			return authenticator.proxyUser;
		}
		return null;
	}
	
	/**
	 * Get the Proxy password if any.
	 * 
	 * @return null if there is no password in the configuration.
	 */
	public char[] getPassword() {
		if (authenticator != null) {
			return authenticator.proxyPassword;
		}
		return null;
	}
}
