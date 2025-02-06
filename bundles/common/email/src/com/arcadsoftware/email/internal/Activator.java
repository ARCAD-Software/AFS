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
package com.arcadsoftware.email.internal;

import java.util.Dictionary;
import java.util.Properties;

import org.osgi.framework.BundleContext;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;
import org.osgi.util.tracker.ServiceTracker;

import com.arcadsoftware.email.IncomingEmailProcessTask;
import com.arcadsoftware.email.SendMail;
import com.arcadsoftware.osgi.AbstractActivator;
import com.arcadsoftware.osgi.SysOutLogged;

public class Activator extends AbstractActivator {

	private static Activator instance;
	
	public static Activator getInstance() {
		return instance;
	}
	
	@SuppressWarnings("rawtypes")
	private ServiceTracker tracker;
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public void start(BundleContext context) throws Exception {
		instance = this;
		super.start(context);
		SendMail.setLogger(this);
		IncomingEmailProcessTask.setLogger(this);
		tracker = new ServiceTracker(context, EventAdmin.class.getName(), null);
		tracker.open();
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		tracker.close();
		SendMail.setLogger(new SysOutLogged());
		IncomingEmailProcessTask.setLogger(new SysOutLogged());
		instance = null;
	}

	@SuppressWarnings("unchecked")
	public void fireErrorNotification(String text) {
		EventAdmin ea = (EventAdmin) tracker.getService();
		if (ea != null) {
			@SuppressWarnings("rawtypes")
			Dictionary properties = new Properties();
			properties.put("module", "SYSTEMERROR"); //$NON-NLS-1$ //$NON-NLS-2$
			properties.put("bundle", getContext().getBundle().getSymbolicName()); //$NON-NLS-1$
			properties.put("details", text); //$NON-NLS-1$
			properties.put("label", "Error during incoming messages processing"); //$NON-NLS-1$
			properties.put("errortype", "Some Messages may be stuck into the Email Box, Possible Recurent Error."); //$NON-NLS-1$
			try {
				ea.postEvent(new Event("com/arcadsoftware/notification", properties)); //$NON-NLS-1$
			} catch (SecurityException e) {
				error(Messages.Activator_Error_fire_event, e);
			}
		}
	}
}
