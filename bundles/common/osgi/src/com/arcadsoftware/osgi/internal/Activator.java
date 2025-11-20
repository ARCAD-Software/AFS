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
package com.arcadsoftware.osgi.internal;

import org.eclipse.core.runtime.adaptor.EclipseStarter;
import org.eclipse.osgi.framework.console.CommandProvider;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;

import com.arcadsoftware.osgi.AbstractActivator;

/**
 * This bundle activator provide some useful console commands.
 */
@SuppressWarnings("restriction")
public class Activator extends AbstractActivator implements Runnable {

	protected static final String PROP_CONFIGPARAM = "param.test"; //$NON-NLS-1$
	protected static final String PROP_CONFIGSYSTEMKEY = "system.key"; //$NON-NLS-1$
	protected static final String PROP_CONFIGDATE = "limitation"; //$NON-NLS-1$
	protected static final String PROP_CONFIGDATELAST = "checksum"; //$NON-NLS-1$
	
	private static Activator instance;

	public static Activator getInstance() {
		return instance;
	}
	
	private StartTaskManager taskManager;
	private volatile boolean shutdown;

	@Override
	public void start(BundleContext context) throws Exception {
		instance = this;
		super.start(context);
		Runtime.getRuntime().addShutdownHook(new Thread(this));
		// Tracker de Tache de démarrage multi-contraintes.
		taskManager = new StartTaskManager(this);
		registerService(CommandProvider.class.getName(), taskManager);
		// Console command provider...
		registerService(CommandProvider.class.getName(), new ConfigCommands(context));
		LogsCommands logc = new LogsCommands(this, context);
		ServiceRegistration<CommandProvider> sr = registerService(CommandProvider.class, logc);
		logc.setServiceRef(sr.getReference());
		registerService(CommandProvider.class, new SystemCommands(context));
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		shutdown = true;
		if (taskManager != null) {
			taskManager.close();
			taskManager = null;
		}
		super.stop(context);
		if (instance == this) {
			instance = null;
		}
	}

	@Override
	public void run() {
		if (!shutdown) {
			error("Shutdown Hook call (Invalid platform shutdown method... but restored just-in-time.)");
			System.out.println("Shutdown Hook call (Invalid platform shutdown method... but restored just-in-time.)");
			if (EclipseStarter.isRunning()) {
				try {
					EclipseStarter.shutdown();
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
	}
}
