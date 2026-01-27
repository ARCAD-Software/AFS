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
package com.arcadsoftware.cleanup.internal;

import java.util.Dictionary;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.Properties;

import org.eclipse.osgi.framework.console.CommandInterpreter;
import org.eclipse.osgi.framework.console.CommandProvider;
import org.osgi.framework.BundleContext;

import com.arcadsoftware.cleanup.CleanupManager;
import com.arcadsoftware.cleanup.Preferences;
import com.arcadsoftware.cleanup.logger.ICleanupLogger;
import com.arcadsoftware.cleanup.operation.AbstractCleanOperation;
import com.arcadsoftware.osgi.AbstractConfiguredActivator;

public class Activator extends AbstractConfiguredActivator implements CommandProvider, ICleanupLogger {

	protected static final String OP_START = "start";
	protected static final String OP_STOP = "stop";
	protected static final String OP_STATUS = "status";
	protected static final String OP_CONFIG = "config";
	protected static final String OP_LIST = "list";
	protected static HashMap<String, String> commandLineOperations;

	static {
		commandLineOperations = new HashMap<>();
		commandLineOperations.put(OP_START, "starts the cleanup module and immediately executes the cleanup process");
		commandLineOperations.put(OP_STOP, "stops the cleanup module");
		commandLineOperations.put(OP_STATUS, "print the module status");
		commandLineOperations.put(OP_CONFIG, "list the current configuration of the module");
		commandLineOperations.put(OP_LIST, "list all the registered cleanup operations");
	}

	private final Preferences cleanupPreferences;
	private final CleanupManager cleanupManager;

	public Activator() {
		super();
		cleanupManager = CleanupManager.getInstance();
		cleanupManager.setLogger(this);
		cleanupPreferences = new Preferences(cleanupManager);
		cleanupManager.setPreferences(cleanupPreferences);
	}

	@Override
	public void start(BundleContext bundleContext) throws Exception {
		super.start(bundleContext);
		registerService(CommandProvider.class, this);
		if (findAfsCleanerThread() != null) {
			debug(String.format("[%1$s] thread has been found, can't launch new one !", CleanupManager.SERVICE_NAME));
		} else {
			debug(String.format("[%1$s] thread has not been found, launching new one !", CleanupManager.SERVICE_NAME));
			cleanupManager.start();
		}
	}

	private Thread findAfsCleanerThread() {
		final Thread[] lstThread = new Thread[Thread.activeCount()];
		Thread.enumerate(lstThread);
		for (final Thread thd : lstThread) {
			if ((thd != null) && thd.getName().equals(CleanupManager.SERVICE_NAME)) {
				return thd;
			}
		}
		return null;
	}

	@Override
	public boolean initializeConfiguration(Dictionary<String, Object> properties) {
		boolean changed = false;
		if (properties != null) {
			final Properties preferences = cleanupPreferences.getAllPreferences();
			for (final Entry<Object, Object> property : preferences.entrySet()) {
				final String key = (String) property.getKey();
				final Object value = properties.get(key);
				if (value == null) {
					properties.put(key, property.getValue());
					changed = true;
				}
			}
			cleanupPreferences.loadAll(properties);
		}
		return changed;
	}

	@Override
	public void updatedConfiguration(Dictionary<String, Object> properties) {
		if (properties != null) {
			cleanupPreferences.loadAll(properties);
		}
	}

	@Override
	public void stop(BundleContext bundleContext) throws Exception {
		cleanupManager.stop();
		super.stop(bundleContext);
	}

	@Override
	public String getHelp() {
		final StringBuilder operations = new StringBuilder();
		for (final String key : commandLineOperations.keySet()) {
			if (operations.length() > 0) {
				operations.append('|');
			}
			operations.append(key);
		}
		return "\n---Cleanup module---\n\tcleanup [" + //
				operations.toString() + //
				"] - available operations for the cleanup module:\n" + //
				getAvailableOperations("\t\t"); //$NON-NLS-1$
	}

	public String getAvailableOperations(String prefix) {
		final StringBuilder operations = new StringBuilder();
		for (final Entry<String, String> operation : commandLineOperations.entrySet()) {
			operations.append(prefix);
			operations.append(operation.getKey());
			operations.append(" - "); //$NON-NLS-1$
			operations.append(operation.getValue());
			operations.append('\n');
		}
		return operations.toString();
	}

	public Object _cleanup(CommandInterpreter interpreter) {
		final String operation = interpreter.nextArgument();
		if ((operation != null) && commandLineOperations.containsKey(operation)) {
			if (operation.equals(OP_START)) {
				cleanupManager.start();
			} else if (operation.equals(OP_STOP)) {
				cleanupManager.stop();
			} else if (operation.equals(OP_STATUS)) {
				if (cleanupManager.isRunning()) {
					interpreter.println("Cleanup module is running");
				} else {
					interpreter.println("Cleanup module is stopped");
				}
			} else if (operation.equals(OP_CONFIG)) {
				for (final Entry<Object, Object> preference : cleanupPreferences.getAllPreferences().entrySet()) {
					interpreter.println(String.format("%1$s => %2$s", preference.getKey(), preference.getValue()));
				}
			} else if (operation.equals(OP_LIST)) {
				for (final AbstractCleanOperation cleanOperation : cleanupManager.getCleanOperations().values()) {
					interpreter.println(cleanOperation.toString());
				}
			}
		} else {
			interpreter.println("Available operations are:\n" + getAvailableOperations("\t"));
		}
		return null;
	}

	@Override
	public void logException(String message, Throwable e) {
		log(message, e);
	}
}