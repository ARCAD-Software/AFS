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
package com.arcadsoftware.osgi.internal;

import java.util.Date;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.TimerTask;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;

public class SystemTestTask extends TimerTask {

	private SystemParameters tester;
	private BundleContext context;
	private ConfigurationTracker tracker;
	
	public SystemTestTask(BundleContext context, SystemParameters tester, ConfigurationTracker tracker) {
		super();
		this.tracker = tracker;
		this.context = context;
		this.tester = tester;
	}

	@Override
	public void run() {
		Activator.getInstance().debug(Messages.getString("SystemTestTask.DebugStarting")); //$NON-NLS-1$
		Dictionary<String, Object> dic = tracker.getCurrentConfiguration();
		if (dic == null) {
			return;
		}
		String key = null;
		String date = null;
		String ldate = null;
		int param = 0;
		if (dic.get(Activator.PROP_CONFIGDATE) != null) {
			date = dic.get(Activator.PROP_CONFIGDATE).toString();
		}
		if (dic.get(Activator.PROP_CONFIGDATELAST) != null) {
			ldate = dic.get(Activator.PROP_CONFIGDATELAST).toString();
		}
		if (dic.get(Activator.PROP_CONFIGSYSTEMKEY) != null) {
			key = dic.get(Activator.PROP_CONFIGSYSTEMKEY).toString();
		}
		if (dic.get(Activator.PROP_CONFIGPARAM) != null) {
			Object o = dic.get(Activator.PROP_CONFIGPARAM);
			if (o instanceof Integer) {
				param = (Integer)o;
			} else {
				try {
					param = Integer.parseInt(o.toString());
				} catch (NumberFormatException e) {}
			}
		}
		// Test Date validity...
		if ((date != null) && (date.length() > 0)) {
			try {
				int cd = Integer.parseInt(String.format("%1$tY%1$tm%1$td", new Date())); //$NON-NLS-1$
				int d = Integer.parseInt(date);
				int ld = 0;
				if ((ldate != null) && (ldate.length() > 0)) {
					ld = Integer.parseInt(ldate);
				}
				if (ld > cd) {
					Activator.getInstance().warn(Messages.getString("SystemTestTask.Limitdate_expired"), null); //$NON-NLS-1$
					shutdown();
					return;
				}
				if (ld < cd) {
					// Update current configuration.
					synchronized (Activator.getInstance()) {
						dic = tracker.getCurrentConfiguration();
						if (dic == null) {
							dic = new Hashtable<String, Object>();
						}
						dic.put(Activator.PROP_CONFIGDATELAST, Integer.toString(cd));
						tracker.update(dic);
					}
					if (d < cd) {
						Activator.getInstance().warn(Messages.getString("SystemTestTask.Limitdate_expired"), null); //$NON-NLS-1$
						shutdown();
					}
				}
			} catch (Exception e) {
				Activator.getInstance().debug(e.getLocalizedMessage(), e);
			}
		}
		if ((key != null) && (key.length() > 0) && (param > 0) && !tester.testParameters(key, param)) {
			Activator.getInstance().warn(Messages.getString("SystemTestTask.Sysparams_invalid"), null); //$NON-NLS-1$
			shutdown();
		}
	}

	private void shutdown() {
		// Stop all started bundles...
		for (Bundle bundle: context.getBundles()) {
			if (toShutdown(bundle.getSymbolicName())) {
				if ((bundle.getState() == Bundle.STARTING) || (bundle.getState() == Bundle.ACTIVE)) {
					try {
						bundle.stop();
					} catch (BundleException e) {}
				}
			}
		}
	}

	private boolean toShutdown(String sname) {
		for (String s: new String[] {
				"org.eclipse.osgi", //$NON-NLS-1$
				"org.eclipse.equinox", //$NON-NLS-1$
				"javax.", //$NON-NLS-1$
				"org.restlet", //$NON-NLS-1$
				"bc", //$NON-NLS-1$
				"org.ops4j.pax", //$NON-NLS-1$
				"org.apache.commons.commons", //$NON-NLS-1$
				"org.apache.felix"}) { //$NON-NLS-1$
			if (sname.startsWith(s)) {
				return false;
			}
		}
		for (String bn: new String[] {
				"com.arcadsoftware.osgi", //$NON-NLS-1$
				"com.arcadsoftware.runtime", //$NON-NLS-1$
				"com.arcadsoftware.crypt", //$NON-NLS-1$
				"com.arcadsoftware.groovy.security", //$NON-NLS-1$
				"groovy", //$NON-NLS-1$ 
				"com.arcadsoftware.rest", //$NON-NLS-1$ 
				"xstream", //$NON-NLS-1$
				"org.apache.sshd.osgi", //$NON-NLS-1$
				"com.arcadsoftware.cm.simple"}) { //$NON-NLS-1$
			if (sname.equals(bn)) {
				return false;
			}
		}
		return true;
	}
}
