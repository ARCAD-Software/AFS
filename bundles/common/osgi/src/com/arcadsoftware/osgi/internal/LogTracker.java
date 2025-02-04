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

import java.util.ArrayList;

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.util.tracker.ServiceTracker;

import com.arcadsoftware.osgi.AbstractActivator;

import org.osgi.service.log.Logger;
import org.osgi.service.log.LoggerFactory;

/**
 * This tracker is useful to log a message to all available LogServices.
 */
public class LogTracker extends ServiceTracker<LoggerFactory, Logger> {

	private final ArrayList<WaitingLogEntry> tempMessages = new ArrayList<WaitingLogEntry>();
	
	private final BundleContext context;
	private final String name;
	
	public LogTracker(BundleContext context) {
		super(context, LoggerFactory.class, null);
		this.context = context;
		name = context.getBundle().getSymbolicName();
	}
	
	public LogTracker(BundleContext context, String name) {
		super(context, LoggerFactory.class, null);
		this.context = context;
		this.name = name;
	}

	@Override
	public Logger addingService(ServiceReference<LoggerFactory> reference) {
		LoggerFactory loggerFactory = context.getService(reference);
		if (loggerFactory != null) {
			Logger logger = loggerFactory.getLogger(context.getBundle(), name, Logger.class);
			synchronized (this) {
				try {
					for (WaitingLogEntry entry : tempMessages) {
						log(logger, entry.getLevel(), entry.getMessage(), entry.getObjects());
					}
				} finally {
					tempMessages.clear();
				}
			}
			
		}
		return null;
	}

	@Override
	public void removedService(ServiceReference<LoggerFactory> reference, Logger service) {
		// TODO Auto-generated method stub
		super.removedService(reference, service);
	}

	/**
	 * Add a log line to any registered Log Service.
	 * 
	 * @param reference
	 * @param level
	 * @param message
	 * @param exception
	 */
	public void log(int level, String message, Object... objects) {
		Object[] loggers = getServices();
		boolean logged = false;
		if (loggers != null) {
			for (Object logger : loggers) {
				if (logger instanceof Logger) {
					try {
						log((Logger) logger, level, message, objects);
						logged = true;
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
			}
		} 
		// There is no OSGi log service implemented... yet.
		if (!logged) {
			if (level >= AbstractActivator.LOG_ERROR) {
				System.err.println(message);
			} else {
				System.out.println(message);
			}
			for (Object o: objects) {
				if (o instanceof Throwable) {
					((Throwable) o).printStackTrace();
				}
			}
			if (tempMessages.size() < 2000) {
				synchronized (this) {
					tempMessages.add(new WaitingLogEntry(level, message, objects));
				}
			}
		}
	}

	private void log(Logger logger, int level, String message, Object[] objects) {
		switch (level) {
		case AbstractActivator.LOG_AUDIT:
			logger.audit(message, objects);
			break;
		case AbstractActivator.LOG_ERROR:
			logger.error(message, objects);
			break;
		case AbstractActivator.LOG_WARNING:
			logger.warn(message, objects);
			break;
		case AbstractActivator.LOG_INFO:
			logger.info(message, objects);
			break;
		case AbstractActivator.LOG_DEBUG:
			logger.debug(message, objects);
			break;
		case AbstractActivator.LOG_TRACE:
			logger.trace(message, objects);
			break;
		}
	}
}