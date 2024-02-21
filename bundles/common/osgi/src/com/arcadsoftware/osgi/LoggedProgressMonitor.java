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
package com.arcadsoftware.osgi;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;

/**
 * Use the ILoggedPlugin interface to log the progression.
 * 
 * @since 1.2.2
 * @author ARCAD Software
 */
public class LoggedProgressMonitor implements IProgressMonitor {

	/**
	 * An error message (Value 1).
	 * 
	 * <p>
	 * This log entry indicates the bundle or service may not be functional.
	 */
	public static final int LOG_ERROR = org.osgi.service.log.LogService.LOG_ERROR;

	/**
	 * A warning message (Value 2).
	 * 
	 * <p>
	 * This log entry indicates a bundle or service is still functioning but may experience problems in the future
	 * because of the warning condition.
	 */
	public static final int LOG_WARNING = org.osgi.service.log.LogService.LOG_WARNING;
	
	/**
	 * An informational message (Value 3).
	 * 
	 * <p>
	 * This log entry may be the result of any change in the bundle or service and does not indicate a problem.
	 */
	public static final int LOG_INFO = org.osgi.service.log.LogService.LOG_INFO;
	
	/**
	 * A debugging message (Value 4).
	 * 
	 * <p>
	 * This log entry is used for problem determination and may be irrelevant to anyone but the bundle developer.
	 */
	public static final int LOG_DEBUG = org.osgi.service.log.LogService.LOG_DEBUG;
	
	private final ILoggedPlugin logger;
	private final boolean logProgression;
	private final IProgressMonitor monitor;
	private final int logLevel;
	private final String prefix;
	private double work;
	private double totalWork = 100.0;
	private String currentTask;
	private String task;
	private volatile boolean canceled;
	

	/**
	 * Create a new monitor.
	 * 
	 * @param activator the Interface to the agrgregated logger.
	 */
	public LoggedProgressMonitor(final ILoggedPlugin activator) {
		super();
		logger = activator;
		logProgression = false;
		monitor = new NullProgressMonitor();
		logLevel = LOG_DEBUG;
		prefix = ""; //$NON-NLS-1$
	}
	

	/**
	 * Create a new monitor.
	 * 
	 * @param activator the Interface to the agrgregated logger.
	 * @param logProgression If true the progression will be logged too.
	 */
	public LoggedProgressMonitor(final ILoggedPlugin activator, final boolean logProgression) {
		super();
		logger = activator;
		this.logProgression = logProgression;
		monitor = new NullProgressMonitor();
		logLevel = LOG_DEBUG;
		prefix = ""; //$NON-NLS-1$
	}
	

	/**
	 * Create a new monitor.
	 * 
	 * @param activator the Interface to the agrgregated logger.
	 * @param logProgression If true the progression will be logged too.
	 * @param prefix a log message prefix.
	 */
	public LoggedProgressMonitor(final ILoggedPlugin activator, final boolean logProgression, final String prefix) {
		super();
		logger = activator;
		this.logProgression = logProgression;
		monitor = new NullProgressMonitor();
		logLevel = LOG_DEBUG;
		if (prefix != null) {
			this.prefix = prefix;
		} else {
			this.prefix = ""; //$NON-NLS-1$
		}
	}
	

	/**
	 * Create a new monitor.
	 * 
	 * @param activator the Interface to the agrgregated logger.
	 * @param logProgression If true the progression will be logged too.
	 * @param prefix a log message prefix.
	 * @param logLevel the log level for the messages.
	 */
	public LoggedProgressMonitor(final ILoggedPlugin activator, final boolean logProgression, final String prefix, final int logLevel) {
		super();
		logger = activator;
		this.logProgression = logProgression;
		monitor = new NullProgressMonitor();
		this.logLevel = logLevel;
		if (prefix != null) {
			this.prefix = prefix;
		} else {
			this.prefix = ""; //$NON-NLS-1$
		}
	}

	/**
	 * Create a new monitor.
	 * 
	 * @param activator the Interface to the agrgregated logger.
	 * @param logProgression If true the progression will be logged too.
	 * @param prefix a log message prefix.
	 * @param logLevel the log level for the messages.
	 * @param monitor the wrapped monitor.
	 */
	public LoggedProgressMonitor(final ILoggedPlugin activator, final boolean logProgression, final String prefix, final int logLevel, final IProgressMonitor monitor) {
		super();
		logger = activator;
		this.logProgression = logProgression;
		if (monitor != null) {
			this.monitor = monitor;
		} else {
			this.monitor = new NullProgressMonitor();
		}
		this.logLevel = logLevel;
		if (prefix != null) {
			this.prefix = prefix;
		} else {
			this.prefix = ""; //$NON-NLS-1$
		}
	}
	
	@Override
	public void beginTask(String name, int totalWork) {
		this.totalWork = totalWork;
		log("Starting task: " + name);
		currentTask = name;
		task = name;
		monitor.beginTask(name, totalWork);
	}

	private void log(String message) {
		switch (logLevel) {
		case LOG_DEBUG:
			logger.debug(prefix + message);
			break;
		case LOG_INFO:
			logger.log(prefix + message);
			break;
		case LOG_WARNING:
			logger.warn(prefix + message);
			break;
		case LOG_ERROR:
			logger.error(prefix + message, null);
			break;
		}
	}

	@Override
	public void done() {
		if (currentTask != null) {
			log("Ending task: " + currentTask);
		}
		monitor.done();
	}

	@Override
	public void internalWorked(double work) {
		this.work = work;
		monitor.internalWorked(work);
	}

	@Override
	public boolean isCanceled() {
		return monitor.isCanceled() || canceled;
	}

	@Override
	public void setCanceled(boolean value) {
		canceled = value;
		monitor.setCanceled(value);
	}

	@Override
	public void setTaskName(String name) {
		log("New task: " + name);
		currentTask = name;
		task = name;
		monitor.setTaskName(name);
	}

	@Override
	public void subTask(String name) {
		log("Starting sub-task: " + name);
		task = name;
		monitor.subTask(name);
	}

	@Override
	public void worked(int work) {
		this.work += work;
		if (logProgression && (totalWork > 0)) {
			if (task != null) {
				log(task + " progression: " + Double.toString(this.work * 100 / totalWork) + "%." );
			} else {
				log("Progression: " + Double.toString(this.work * 100 / totalWork) + "%." );
			}
		}
		monitor.worked(work);
	}

}
