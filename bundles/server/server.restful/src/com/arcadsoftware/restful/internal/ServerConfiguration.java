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
package com.arcadsoftware.restful.internal;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Timer;
import java.util.TimerTask;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleEvent;
import org.osgi.framework.BundleListener;
import org.osgi.framework.Constants;
import org.osgi.framework.ServiceReference;
import org.osgi.service.cm.Configuration;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.cm.ConfigurationException;
import org.osgi.service.cm.ManagedService;

import com.arcadsoftware.crypt.Crypto;

public class ServerConfiguration implements ManagedService {

	private static final String PROP_BRANDING_BUNDLE = "branding.bundle"; //$NON-NLS-1$;

	private final Activator activator;
	private volatile boolean migrate;
	
	public ServerConfiguration(Activator activator) {
		super();
		this.activator = activator;
	}

	/**
	 * @return OSGi Service Properties.
	 */
	public Dictionary<String, ?> getProperties() {
		Dictionary<String, Object> props = new Hashtable<String, Object>();
		props.put(Constants.SERVICE_PID, activator.getContext().getBundle().getSymbolicName());
		return props;
	}
	
	private String getProperty(final Dictionary<String, ?> properties, final String key, final String defaultValue) {
		final Object value = properties.get(key);
		if (value == null) {
			return defaultValue;
		}
		final String sv = value.toString();
		if (sv.isEmpty()) {
			return defaultValue;
		}
		return sv;
	}
	
	private boolean getBooleanProperty(final Dictionary<String, ?> properties, final String key, final boolean defaultValue) {
		final Object value = properties.get(key);
		if (value == null) {
			return defaultValue;
		}
		final String sv = value.toString();
		if (sv.isEmpty()) {
			return defaultValue;
		}
		return sv.equalsIgnoreCase("true") || sv.equalsIgnoreCase("yes") || sv.equals("1"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}
	
	private Bundle getBrandingBundle(final Dictionary<String, ?> properties) {
		final String brandingBundleName = getProperty(properties, PROP_BRANDING_BUNDLE, null);
		if (brandingBundleName != null) {
			for(Bundle b: activator.getContext().getBundles()) {
				if (brandingBundleName.equalsIgnoreCase(b.getSymbolicName())) {
					return b;
				}
			}
			// The Branding bundle may not be installed yet ! (A restart of the server may be require to correct this !)
			activator.addBundleListener(new BundleListener() {
				@Override
				public void bundleChanged(BundleEvent event) {
					if (((event.getType() == BundleEvent.INSTALLED) || (event.getType() == BundleEvent.RESOLVED)) && //
							brandingBundleName.equals(event.getBundle().getSymbolicName())) {
						activator.info("Late definition of branding bundle...");
						activator.propertiesChange(new ServerProperties( //
								activator.getConfiguration(activator.getContext().getBundle().getSymbolicName()), //
								event.getBundle(), //
								activator.getContext().getBundle()));
					}
				}
			});
		}
		return null;
	}
	
	@Override
	public void updated(Dictionary<String, ?> properties) throws ConfigurationException {
		if ((properties == null) || initConfiguration(properties)) {
			return;
		}
		final ServerProperties sp = new ServerProperties(properties, getBrandingBundle(properties), activator.getContext().getBundle());
		// Call the synchronized update method from the Activator.
		final Timer timer = new Timer("Delayed REST Server Properties changes"); //$NON-NLS-1$
		timer.schedule(new TimerTask() {
			public void run() {
				timer.cancel();
				activator.propertiesChange(sp);
			}
		}, 500);
	}

	private boolean initConfiguration(final Dictionary<String, ?> properties) {
		// L'initialisation va provoquer un update de la configuration.
		// Si c'est le cas on sort direct.
		// On sort aussi si aucune initialisation n'est nécessaire.
		if (migrate || 
			((getProperty(properties, ServerProperties.PROP_DOMAINNAME, null) != null) && //
					(getProperty(properties, "ssl", null) == null) && //$NON-NLS-1$
					Crypto.isCryptSecure(getProperty(properties, ServerProperties.PROP_KEYSTOREPWD, null)) && //
					Crypto.isCryptSecure(getProperty(properties, ServerProperties.PROP_KEYPWD, null)) && //
					Crypto.isCryptSecure(getProperty(properties, ServerProperties.PROP_TRUSTSTOREPWD, null)))) {
			return false;
		}
		final Hashtable<String, Object> props = new Hashtable<>();
		for(Enumeration<String> keys = properties.keys(); keys.hasMoreElements();) {
			String key = keys.nextElement();
			if (!"ssl".equalsIgnoreCase(key)) { //$NON-NLS-1$
				props.put(key, properties.get(key));
			}
		}
		// On teste si les propriétés ont besoin d'être initialisées.
		if (getProperty(properties, ServerProperties.PROP_DOMAINNAME, null) == null) {
			String defaulthostname = "localhost"; //$NON-NLS-1$
			try {
				defaulthostname = InetAddress.getLocalHost().getHostName();
			} catch (UnknownHostException e) {
				activator.debug(e);
			}
			props.put(ServerProperties.PROP_DOMAINNAME, defaulthostname);
		}
		// On teste si les propriétés ont besoin d'être mises à jour.
		if (getBooleanProperty(properties, "ssl", false) && //$NON-NLS-1$
				!props.contains(ServerProperties.PROP_PORTSSL)) {
			props.put(ServerProperties.PROP_PORTSSL, properties.get(ServerProperties.PROP_PORTNUMBER));
			props.put(ServerProperties.PROP_PORTNUMBER, "0"); //$NON-NLS-1$
		}
		// Mise à jour du cryptage des mots de passe.
		String p = getProperty(properties, ServerProperties.PROP_KEYSTOREPWD, null);
		if ((p != null) && !p.isEmpty() && !Crypto.isCryptSecure(p)) {
			p = Crypto.encrypt(Crypto.decrypt(p));
			if (p != null) {
				props.put(ServerProperties.PROP_KEYSTOREPWD, p);
			}
				
		}
		p = getProperty(properties, ServerProperties.PROP_KEYPWD, null);
		if ((p != null) && !p.isEmpty() && !Crypto.isCryptSecure(p)) {
			p = Crypto.encrypt(Crypto.decrypt(p));
			if (p != null) {
				props.put(ServerProperties.PROP_KEYPWD, p);
			}
		}
		p = getProperty(properties, ServerProperties.PROP_TRUSTSTOREPWD, null);
		if ((p != null) && !p.isEmpty() && !Crypto.isCryptSecure(p)) {
			p = Crypto.encrypt(Crypto.decrypt(p));
			if (p != null) {
				props.put(ServerProperties.PROP_TRUSTSTOREPWD, p);
			}
		}
		ServiceReference<ConfigurationAdmin> sr = activator.getContext().getServiceReference(ConfigurationAdmin.class);
		if (sr != null) {
			ConfigurationAdmin ca = activator.getContext().getService(sr);
			if (ca != null) {
				try {
					migrate = true;
					try {
						Configuration conf = ca.getConfiguration(activator.getContext().getBundle().getSymbolicName(), null);
						conf.update(props);
					} catch (IOException e) {
						activator.debug(e);
					}
				} finally {
					migrate = false;
				}
			}
		}
		// If the initilialisation process has changed the configuration an new update has been, or is going to be throw...
		// so we can cancel the current one.
		return true;
	}

}
