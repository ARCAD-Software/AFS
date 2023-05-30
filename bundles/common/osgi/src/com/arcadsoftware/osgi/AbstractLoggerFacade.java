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
package com.arcadsoftware.osgi;

/**
 * This class implement a facade that can be used as a singleton to log message into
 * project that can be used into OSGi framework and pure Java Application. This façade can be
 * used to create a code independent from the OSGi classes, only the com.arcadsoftware.osgi.jar is
 * required to use this class. 
 * 
 * <p>
 * Into OSGi platform the Activator need to instantiate the logger facade singleton.
 * 
 * <p>
 * By default this facade print the log to the system.out stream.
 * 
 * @author ARCAD software
 *
 */
public class AbstractLoggerFacade implements ILoggedPlugin {

	/**
	 * Allow to print debug information about timing executions.
	 * 
	 * <p>
	 * The log level must be set to DEBUG.
	 */
	public static final boolean TIMINGTRACE = "true".equalsIgnoreCase(System.getProperty("com.arcadsoftware.trace.timing")); //$NON-NLS-1$ //$NON-NLS-2$
	
	/**
	 * Allow to add debug informations.
	 * 
	 * <p>
	 * The log level must be set to DEBUG.
	 */
	public static final boolean TRACE = "true".equalsIgnoreCase(System.getProperty("com.arcadsoftware.trace")); //$NON-NLS-1$ //$NON-NLS-2$

	private final ILoggedPlugin activator;
	
	/**
	 * Create a logger facade that will use the given ILoggedPlugin to log the messages.
	 * @param activator
	 */
	public AbstractLoggerFacade(ILoggedPlugin activator) {
		this.activator = activator;
	}
	
	/**
	 * Create a logger facade that will print all the messages to the System streams.
	 */
	public AbstractLoggerFacade() {
		this.activator = null;
	}

	@Override
	public void log(String message) {
		if (activator == null) {
			System.out.println(message);
		} else {
			activator.log(message);
		}
	}

	@Override
	public void log(String message, Throwable e) {
		if (activator == null) {
			System.out.println(message);
			if (e != null) {
				e.printStackTrace(System.out);
			}
		} else {
			activator.log(message, e);
		}
	}

	@Override
	public void log(Throwable e) {
		if (activator == null) {
			if (e != null) {
				e.printStackTrace(System.out);
			}
		} else {
			activator.log(e);
		}
	}

	public void info(String message) {
		log(message);
	}

	public void info(String message, Throwable e) {
		log(message, e);
	}

	public void info(Throwable e) {
		log(e);
	}

	@Override
	public void error(String message, Throwable e) {
		if (activator == null) {
			System.err.println(message);
			if (e != null) {
				e.printStackTrace();
			}
		} else {
			activator.error(message, e);
		}
	}

	public void error(Throwable e) {
		if (e != null) {
			if (activator == null) {
				e.printStackTrace();
			} else {
				activator.error(e.getLocalizedMessage(), e);
			}
		}
	}

	@Override
	public void warn(String message) {
		if (activator == null) {
			System.out.println("[WARNING] " + message); //$NON-NLS-1$
		} else {
			activator.warn(message);
		}
	}

	@Override
	public void warn(String message, Throwable e) {
		if (activator == null) {
			System.out.println("[WARNING] " + message); //$NON-NLS-1$
			if (e != null) {
				e.printStackTrace(System.out);
			}
		} else {
			activator.warn(message, e);
		}
	}

	@Override
	public void debug(String message) {
		if (activator == null) {
			System.out.println("[DEBUG] " + message); //$NON-NLS-1$
		} else {
			activator.debug(message);
		}
	}

	@Override
	public void debug(String message, Throwable e) {
		if (activator == null) {
			System.out.println("[DEBUG] " + message); //$NON-NLS-1$
			if (e != null) {
				e.printStackTrace(System.out);
			}
		} else {
			activator.debug(message, e);
		}
	}

	@Override
	public void debug(Throwable e) {
		if (activator == null) {
			if (e != null) {
				e.printStackTrace(System.out);
			}
		} else {
			activator.debug(e);
		}
	}

	/**
	 * Timing logging running from the given start date.
	 * 
	 * @param message
	 * @param t starting time (= System.currentTimeMillis())
	 * @see AbstractLoggerFacade#TIMINGTRACE
	 */
	public void logTiming(String message, long t) {
		if (TIMINGTRACE) {
			debug(String.format("Timing \"%s\" in %dms.", message, System.currentTimeMillis() - t)); //$NON-NLS-1$
		}
	}
	
	/**
	 * Log a detailed debug message. Only if the system property com.arcadsoftware.trace is true.
	 * 
	 * <p>
	 * For performance reasons the parameter are not not necessarily converted into string. This occurs only 
	 * if the message is effectively sent to the log, if the TRACE property is set to <b>true</b>.
	 * 
	 * @param message The message will be assembled from the given object list, only and only if the TRACE is activated.
	 * @see AbstractLoggerFacade#TIMINGTRACE
	 */
	public void trace(Object... message) {
		if (TRACE) {
			StringBuilder sb = new StringBuilder();
			for(Object o: message) {
				sb.append(o);
			}
			debug(sb.toString());
		}
	}

	/**
	 * Log a stack trace only if the "trace" system property is set.
	 * 
	 * @param e
	 */
	public void trace(Throwable e) {
		if (TRACE) {
			debug(e);
		}
	}

}