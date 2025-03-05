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
 * This class assume that the followinfg properties are defined in the given configuration.
 * 
 * <ul>
 * <li>
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
	private final Authenticator authenticator;
	
	public ConfiguredProxy(Dictionary<String, Object> props) {
		this(getType(props.get(PROP_PROXY_TYPE)), props);
	}
	
	public ConfiguredProxy(String type, Dictionary<String, Object> props) {
		this(getType(type), props);
	}
	
	public ConfiguredProxy(Type type, Dictionary<String, Object> props) {
		super();
		InetSocketAddress a = getAddress(props);
		if ((type != Type.DIRECT) && (a != null)) {
			proxy = new Proxy(type, a);
			authenticator = createAuthenticator(props.get(PROP_PROXY_LOGIN), props.get(PROP_PROXY_PASSWORD));
		} else {
			proxy = null;
			authenticator = null;
		}
	}
	
	public ConfiguredProxy(Map<String, Object> props) {
		this(getType(props.get(PROP_PROXY_TYPE)), props);
	}
	
	public ConfiguredProxy(String type, Map<String, Object> props) {
		this(getType(type), props);
	}
	
	public ConfiguredProxy(Type type, Map<String, Object> props) {
		super();
		InetSocketAddress a = getAddress(props);
		if ((type != Type.DIRECT) && (a != null)) {
			proxy = new Proxy(type, a);
			authenticator = createAuthenticator(props.get(PROP_PROXY_LOGIN), props.get(PROP_PROXY_PASSWORD));
		} else {
			proxy = null;
			authenticator = null;
		}
	}
	
	private Authenticator createAuthenticator(Object login, Object pwd) {
		if ((login instanceof String) && !((String) login).isEmpty()) {
			return new ProxyAuthenticator((String) login, Crypto.decrypt((String) pwd));
		}
		return null;
	}

	private InetSocketAddress getAddress(Dictionary<String, Object> props) {
		Object o = props.get(PROP_PROXY_HOSTNAME);
		if (o != null) {
			Object po = props.get(PROP_PROXY_PORT);
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
	
	private InetSocketAddress getAddress(Map<String, Object> props) {
		Object o = props.get(PROP_PROXY_HOSTNAME);
		if (o != null) {
			Object po = props.get(PROP_PROXY_PORT);
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
}
