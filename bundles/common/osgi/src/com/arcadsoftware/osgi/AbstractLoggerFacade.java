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
package com.arcadsoftware.osgi;

import java.util.Arrays;

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

	protected static String format(String message, Object... objects) {
		if (message == null) {
			return ""; //$NON-NLS-1$
		}
		StringBuilder sb = new StringBuilder();
		int i = message.indexOf('{');
		int p = 0;
		int x = 0;
		while (i >= 0) {
			if ((i < message.length() - 1) && (message.charAt(i + 1) == '}')) {
				if ((i > 0) && (message.charAt(i - 1) == '\\')) {
					if ((i > 1) && (message.charAt(i - 2) == '\\')) {
						// Just remove le second '\' character and process to the placeholder replacement.
						sb.append(message.substring(p, i - 1));
						sb.append(format(x++, objects));
						p = i + 2;
					}
				}
			}
			i = message.indexOf('{', i + 1);
		}
		if (p < message.length()) {
			sb.append(message.substring(p));
		}
		return sb.toString();
	}
	
	private static Object format(int i, Object[] objects) {
		if ((i < objects.length)) {
			Object o = objects[i];
			if (o == null) {
				return "null"; //$NON-NLS-1$
			}
			Class<?> c = o.getClass();
			if (c.isArray()) {
				if (c == byte[].class) {
					return Arrays.toString((byte[]) o);
				}
				if (c == short[].class) {
					return Arrays.toString((short[]) o);
				}
				if (c == int[].class) {
					return Arrays.toString((int[]) o);
				}
				if (c == long[].class) {
					return Arrays.toString((long[]) o);
				}
				if (c == float[].class) {
					return Arrays.toString((float[]) o);
				}
				if (c == double[].class) {
					return Arrays.toString((double[]) o);
				} 
				if (c == boolean[].class) {
					return Arrays.toString((boolean[]) o);
				} 
				if (c == char[].class) {
					return Arrays.toString((char[]) o);
				}
				return Arrays.deepToString((Object[]) o);
			}
			return o.toString();
		}
		return null;
	}

	/**
	 * Allow to print debug information about timing executions.
	 * 
	 * <p>
	 * The log level must be set to DEBUG.
	 */
	public static final boolean TIMINGTRACE = "true".equalsIgnoreCase(System.getProperty("com.arcadsoftware.trace.timing")); //$NON-NLS-1$ //$NON-NLS-2$

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
			activator.info(message);
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
			activator.info(message, e);
		}
	}

	@Override
	public void log(Throwable e) {
		if (activator == null) {
			if (e != null) {
				e.printStackTrace(System.out);
			}
		} else {
			activator.info(e);
		}
	}

	@Override
	public void info(String message) {
		log(message);
	}

	@Override
	public void info(String message, Throwable e) {
		log(message, e);
	}

	@Override
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

	@Override
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
	public void error(String message) {
		if (activator == null) {
			System.err.println(message);
		} else {
			activator.error(message);
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
	public void warn(Throwable e) {
		if (activator == null) {
			if (e != null) {
				e.printStackTrace(System.out);
			}
		} else {
			activator.warn(e);
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

	@Override
	public void trace(String message) {
		if (activator == null) {
			System.out.println("[TRACE] " + message); //$NON-NLS-1$
		} else {
			activator.trace(message);
		}
	}

	@Override
	public void trace(String message, Throwable e) {
		if (activator == null) {
			System.out.println("[TRACE] " + message); //$NON-NLS-1$
			if (e != null) {
				e.printStackTrace(System.out);
			}
		} else {
			activator.trace(message, e);
		}
	}

	@Override
	public void trace(Throwable e) {
		if (activator == null) {
			if (e != null) {
				e.printStackTrace(System.out);
			}
		} else {
			activator.trace(e);
		}
	}

	@Override
	public void audit(String message, Throwable e) {
		if (activator == null) {
			System.err.println(message);
			if (e != null) {
				e.printStackTrace(System.out);
			}
		} else {
			activator.audit(message, e);
		}
	}

	@Override
	public void audit(String message) {
		if (activator == null) {
			System.err.println(message);
		} else {
			activator.audit(message);
		}
	}

	@Override
	public void audit(Throwable e) {
		if (activator == null) {
			if (e != null) {
				e.printStackTrace();
			}
		} else {
			activator.audit(e);
		}
	}

	@Override
	public void audit(String message, Object... objects) {
		if (activator == null) {
			System.err.println(format(message, objects));
			if ((objects.length > 0) && (objects[objects.length - 1] instanceof Throwable)) {
				((Throwable) objects[objects.length - 1]).printStackTrace(System.out);
			}
		} else {
			activator.audit(message, objects);
		}
	}

	@Override
	public void error(String message, Object... objects) {
		if (activator == null) {
			System.err.println(format(message, objects));
			if ((objects.length > 0) && (objects[objects.length - 1] instanceof Throwable)) {
				((Throwable) objects[objects.length - 1]).printStackTrace(System.out);
			}
		} else {
			activator.error(message, objects);
		}
	}

	@Override
	public void warn(String message, Object... objects) {
		if (activator == null) {
			System.out.println("[WARNING] " + format(message, objects));
			if ((objects.length > 0) && (objects[objects.length - 1] instanceof Throwable)) {
				((Throwable) objects[objects.length - 1]).printStackTrace(System.out);
			}
		} else {
			activator.warn(message, objects);
		}
	}

	@Override
	public void info(String message, Object... objects) {
		if (activator == null) {
			System.out.println(format(message, objects));
			if ((objects.length > 0) && (objects[objects.length - 1] instanceof Throwable)) {
				((Throwable) objects[objects.length - 1]).printStackTrace(System.out);
			}
		} else {
			activator.info(message, objects);
		}
	}

	@Override
	public void debug(String message, Object... objects) {
		if (activator == null) {
			System.out.println("[DEBUG] " + format(message, objects));
			if ((objects.length > 0) && (objects[objects.length - 1] instanceof Throwable)) {
				((Throwable) objects[objects.length - 1]).printStackTrace(System.out);
			}
		} else {
			activator.debug(message, objects);
		}
	}

	@Override
	public void trace(String message, Object... objects) {
		if (activator == null) {
			System.out.println("[TRACE] " + format(message, objects));
			if ((objects.length > 0) && (objects[objects.length - 1] instanceof Throwable)) {
				((Throwable) objects[objects.length - 1]).printStackTrace(System.out);
			}
		} else {
			activator.trace(message, objects);
		}
	}
	
}