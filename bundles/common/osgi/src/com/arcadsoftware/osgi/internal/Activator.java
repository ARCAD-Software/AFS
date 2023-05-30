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
package com.arcadsoftware.osgi.internal;

import java.util.Timer;

import org.eclipse.core.runtime.adaptor.EclipseStarter;
import org.eclipse.osgi.framework.console.CommandProvider;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;

import com.arcadsoftware.osgi.AbstractConfiguredActivator;
import com.arcadsoftware.osgi.ISystemParameters;

/**
 * This bundle activator provide some useful console commands.
 */
@SuppressWarnings("restriction")
public class Activator extends AbstractConfiguredActivator implements Runnable {

	protected static final String PROP_CONFIGPARAM = "param.test"; //$NON-NLS-1$
	protected static final String PROP_CONFIGSYSTEMKEY = "system.key"; //$NON-NLS-1$
	protected static final String PROP_CONFIGDATE = "limitation"; //$NON-NLS-1$
	protected static final String PROP_CONFIGDATELAST = "checksum"; //$NON-NLS-1$

	private static final String CONFIGCMD = "com.arcadsoftware.config.commands"; //$NON-NLS-1$
	
	private static Activator instance;

	public static Activator getInstance() {
		return instance;
	}
	
	private SystemParameters sysTester;
	private Timer timer;
	private StartTaskManager taskManager;
	private volatile boolean shutdown;

	@Override
	public void start(BundleContext context) throws Exception {
		instance = this;
		super.start(context);
		Runtime.getRuntime().addShutdownHook(new Thread(this));
		sysTester = new SystemParameters(this);
		// Tracker de Tache de démarrage multi-contraintes.
		taskManager = new StartTaskManager(this);
		registerService(CommandProvider.class.getName(), taskManager);
		// Service de protection de license...
		registerService(ISystemParameters.clazz, sysTester);
		// Console command provider...
		if ("true".equalsIgnoreCase(context.getProperty(CONFIGCMD)) || //$NON-NLS-1$
				(context.getBundle().getEntry("/.classpath") != null)) { //$NON-NLS-1$
			registerService(CommandProvider.class.getName(), new ConfigCommands(context));
		}
		LogsCommands logc = new LogsCommands(this, context);
		ServiceRegistration<CommandProvider> sr = registerService(CommandProvider.class, logc);
		logc.setServiceRef(sr.getReference());
		if (timer != null) {
			timer.cancel();
		}
		registerService(CommandProvider.class, new SystemCommands(context));
		// Timer pour tester les paramètres d'activation du système.
		timer = new Timer("System Process Monitor"); //$NON-NLS-1$
		timer.schedule(new SystemTestTask(getContext(),sysTester,(ConfigurationTracker) getConfigurationTracker()), 600000, 21600000); 
		// dans 10 minutes puis toutes les 6 heures
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		shutdown = true;
		if (taskManager != null) {
			taskManager.close();
			taskManager = null;
		}
		super.stop(context);
		if (timer != null) {
			timer.cancel();
			timer = null;
		}
		if (instance == this) {
			instance = null;
		}
	}

	@Override
	public void run() {
		if (!shutdown) {
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
