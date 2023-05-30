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

import java.util.ArrayList;

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.util.tracker.ServiceTracker;
import org.osgi.service.log.LogService;

/**
 * This tracker is useful to log a message to all available LogServices.
 */
public class LogTracker extends ServiceTracker<LogService, LogService> {

	private ArrayList<WaitingLogEntry> tempMessages = new ArrayList<WaitingLogEntry>();
	
	public LogTracker(BundleContext context) {
		super(context, LogService.class, null);
	}

	@Override
	public LogService addingService(ServiceReference<LogService> reference) {
		LogService logger = super.addingService(reference);
		if (logger != null) {
			synchronized (this) {
				try {
					for (WaitingLogEntry entry : tempMessages) {
						logger.log(entry.getReference(), entry.getLevel(), entry.getMessage(), entry.getException());
					}
				} finally {
					tempMessages.clear();
				}
			}
		}
		return logger;
	}

	/**
	 * Add a log line to any registered Log Service.
	 * 
	 * @param reference
	 * @param level
	 * @param message
	 * @param exception
	 */
	public void log(ServiceReference<?> reference, int level, String message, Throwable exception) {
		ServiceReference<LogService>[] references = getServiceReferences();
		int size = 0;
		if (references != null) {
			size = references.length;
			for (ServiceReference<LogService> logref : references) {
				LogService logger = getService(logref);
				if (logger == null) {
					size--;
				} else {
					try {
						logger.log(reference, level, message, exception);
					} catch (Exception e) {
						e.printStackTrace();
						size--;
					}
				}
			}
		} 
		// There is no OSGi log service implemented...
		if (size == 0) {
			if (level >= LogService.LOG_ERROR) {
				System.err.println(message);
			} else {
				System.out.println(message);
			}
			if (exception != null) { 
				exception.printStackTrace();
			}
			synchronized (this) {
				tempMessages.add(new WaitingLogEntry(reference,level,message,exception));
			}
		}
	}
}