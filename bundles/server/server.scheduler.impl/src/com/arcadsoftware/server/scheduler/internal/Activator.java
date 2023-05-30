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
package com.arcadsoftware.server.scheduler.internal;

import org.osgi.framework.BundleContext;

import com.arcadsoftware.osgi.AbstractConfiguredActivator;
import com.arcadsoftware.server.scheduler.ISchedulerService;
import com.arcadsoftware.server.scheduler.internal.resources.Branch;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractConfiguredActivator {

	private volatile ISchedulerService service;

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);		
		// Register the OGSi Service.
		service = new SchedulerService(this);
		registerService(ISchedulerService.class, service);
		service.start();
		registerService(Branch.clazz, new Branch(), Branch.properties("/secure")); //NON-NLS-1
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		//the service is automatically unregister from super implementation.
		super.stop(context);
		// stop scheduler service, after unregistration.
		service.stop();
	}

}