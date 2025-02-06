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
package com.arcadsoftware.afs.client.core.internal;

import java.net.Authenticator;
import java.net.InetAddress;
import java.net.PasswordAuthentication;
import java.util.Hashtable;

import org.eclipse.core.net.proxy.IProxyData;
import org.eclipse.core.net.proxy.IProxyService;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.ServiceReference;

import com.arcadsoftware.afs.client.core.connection.IUserAuthentication;

/**
 * This class override the Eclipse code to do not popup a dialog each time a Proxy login is required.
 *
 * @author ARCAD Software
 */
public class ProxyAuthenticator extends Authenticator {

	private static ProxyAuthenticator instance = new ProxyAuthenticator();

	public static ProxyAuthenticator getInstance() {
		return instance;
	}

	private final Hashtable<String, PasswordAuthentication> cache;

	protected ProxyAuthenticator() {
		super();
		cache = new Hashtable<>();
	}

	@Override
	protected PasswordAuthentication getPasswordAuthentication() {
		String scheme = getRequestingScheme();
		if ((scheme == null) || scheme.isEmpty()) {
			scheme = getRequestingProtocol();
		}
		final InetAddress address = getRequestingSite(); // can be null;
		String host = null;
		if (address != null) {
			host = address.getCanonicalHostName();
		}
		if (host == null) {
			host = getRequestingHost();
		}
		if ((scheme == null) || (host == null)) {
			return null;
		}
		host = host.toLowerCase();
		// Get Proxy auth from Eclipse preferences...
		IProxyService proxyService = null;
		final ServiceReference<IProxyService> sr = BaseActivator.getDefault().getBundle().getBundleContext()
				.getServiceReference(IProxyService.class);
		if (sr != null) {
			proxyService = BaseActivator.getDefault().getBundle().getBundleContext().getService(sr);
		}
		if ((proxyService != null) && proxyService.isProxiesEnabled()) {
			final IProxyData[] proxyDatas = proxyService.getProxyData();
			if (proxyDatas != null) {
				final String proxytype;
				if ("https".equalsIgnoreCase(scheme)) { //$NON-NLS-1$
					proxytype = IProxyData.HTTPS_PROXY_TYPE;
				} else {
					proxytype = IProxyData.HTTP_PROXY_TYPE;
				}
				for (final IProxyData pd : proxyDatas) {
					if (proxytype.equals(pd.getType()) && pd.isRequiresAuthentication()) {
						String proxyHost = pd.getHost();
						if (proxyHost != null) {
							proxyHost = proxyHost.toLowerCase();
							if ((proxyHost.startsWith(host) || host.startsWith(proxyHost)) &&
									(pd.getUserId() != null) && !pd.getUserId().isEmpty()
									&& (pd.getPassword() != null)) {
								return new PasswordAuthentication(pd.getUserId(), pd.getPassword().toCharArray());
							}
						}
					}
				}
			}
		}
		PasswordAuthentication auth = cache.get(scheme + '/' + host);
		if (auth != null) {
			return auth;
		}
		auth = getUserAuthentication(scheme, host, getRequestingPrompt());
		if (auth != null) {
			cache.put(scheme + '/' + host, auth);
		}
		return auth;
	}

	private PasswordAuthentication getUserAuthentication(String scheme, String host, String requestingPrompt) {
		for (final IConfigurationElement e : Platform.getExtensionRegistry()
				.getConfigurationElementsFor("com.arcadsoftware.afs.client.core.userAuthentication")) { //$NON-NLS-1$
			try {
				final Object o = e.createExecutableExtension("class"); //$NON-NLS-1$
				if (o instanceof IUserAuthentication) {
					final PasswordAuthentication auth = ((IUserAuthentication) o).getUserAuthentication(scheme, host,
							requestingPrompt);
					if (auth != null) {
						return auth;
					}
				}
			} catch (final CoreException e1) {
				BaseActivator.getDefault().error(e1.getLocalizedMessage(), e1);
			}
		}
		return null;
	}

}
