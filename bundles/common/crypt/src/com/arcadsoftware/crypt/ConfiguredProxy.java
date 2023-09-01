/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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

import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.Proxy.Type;
import java.net.SocketAddress;
import java.util.Dictionary;

/**
 * Simple wrapper around the Java Proxy class, allowing to declare the proxy properties in a configuration properties file.
 * 
 * @author ARCAD Software
 */
public class ConfiguredProxy {

	public static final String PROP_PROXY_TYPE = "proxy.type"; //$NON-NLS-1$
	public static final String PROP_PROXY_HOSTNAME = "proxy.hostname"; //$NON-NLS-1$
	public static final String PROP_PROXY_PORT = "proxy.port"; //$NON-NLS-1$
	
	private final Proxy proxy;
	
	public ConfiguredProxy(Dictionary<String, Object> props) {
		this(getType(props.get(PROP_PROXY_TYPE)), props);
	}
	
	public ConfiguredProxy(String type, Dictionary<String, Object> props) {
		this(getType(type), props);
	}
	
	public ConfiguredProxy(Type type, Dictionary<String, Object> props) {
		super();
		SocketAddress a = getAddress(props);
		if ((type != Type.DIRECT) && (a != null)) {
			proxy = new Proxy(type, a);
		} else {
			proxy = null;
		}
	}
	
	private SocketAddress getAddress(Dictionary<String, Object> props) {
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
	
	public Proxy getProxy() {
		return proxy;
	}
}
