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

import org.eclipse.osgi.framework.console.CommandInterpreter;

/**
 * Logger facade that use the OSGi console shell to print log messages.
 * 
 * @author ARCAD Software
 * @see CommandInterpreter
 */
public class LoggedToConsole implements ILoggedPlugin {

	private CommandInterpreter ci;

	public LoggedToConsole(CommandInterpreter ci) {
		super();
		this.ci = ci;
	}

	public void warn(String message, Throwable e) {
		ci.println("[WARN] " + message);
		if (e != null) {
			ci.printStackTrace(e);
		}
	}
	
	public void warn(String message) {
		ci.println("[WARN] " + message);
	}
	
	public void log(Throwable e) {
		if (e != null) {
			ci.printStackTrace(e);
		}
	}
	
	public void log(String message, Throwable e) {
		ci.println(message);
		if (e != null) {
			ci.printStackTrace(e);
		}
	}
	
	public void log(String message) {
		ci.println(message);
	}
	
	public void error(String message, Throwable e) {
		ci.println("[ERROR] " + message);
		if (e != null) {
			ci.printStackTrace(e);
		}
	}
	
	public void debug(Throwable e) {
		if (e != null) {
			ci.println("[DEBUG] " + e.getLocalizedMessage());
			ci.printStackTrace(e);
		}
	}
	
	public void debug(String message, Throwable e) {
		ci.println("[DEBUG] " + message);
		if (e != null) {
			ci.printStackTrace(e);
		}
	}
	
	public void debug(String message) {
		ci.println("[DEBUG] " + message);
	}

	@Override
	public void info(String message) {
		ci.println(message);
	}

	@Override
	public void info(String message, Throwable e) {
		ci.println(message);
		if (e != null) {
			ci.printStackTrace(e);
		}
	}

	@Override
	public void info(Throwable e) {
		if (e != null) {
			ci.printStackTrace(e);
		}
	}

	@Override
	public void error(String message) {
		ci.println("[ERROR] " + message);
	}

	@Override
	public void error(Throwable e) {
		if (e != null) {
			ci.printStackTrace(e);
		}
	}

	@Override
	public void warn(Throwable e) {
		if (e != null) {
			ci.printStackTrace(e);
		}
	}

	@Override
	public void trace(String message) {
		ci.println("[TRACE] " + message);
	}

	@Override
	public void trace(String message, Throwable e) {
		ci.println("[TRACE] " + message);
		if (e != null) {
			ci.printStackTrace(e);
		}
	}

	@Override
	public void trace(Throwable e) {
		if (e != null) {
			ci.printStackTrace(e);
		}
	}

	@Override
	public void audit(String message, Throwable e) {
		ci.println("[AUDIT] " + message);
		if (e != null) {
			ci.printStackTrace(e);
		}
	}

	@Override
	public void audit(String message) {
		ci.println("[AUDIT] " + message);
	}

	@Override
	public void audit(Throwable e) {
		if (e != null) {
			ci.printStackTrace(e);
		}
	}

	@Override
	public void trace(String message, Object... objects) {
		ci.println("[TRACE] " + AbstractLoggerFacade.format(message, objects));
		if ((objects.length > 0) && (objects[objects.length - 1] instanceof Throwable)) {
			ci.printStackTrace((Throwable) objects[objects.length - 1]);
		}
	}

	@Override
	public void debug(String message, Object... objects) {
		ci.println("[DEBUG] " + AbstractLoggerFacade.format(message, objects));
		if ((objects.length > 0) && (objects[objects.length - 1] instanceof Throwable)) {
			ci.printStackTrace((Throwable) objects[objects.length - 1]);
		}
	}

	@Override
	public void info(String message, Object... objects) {
		ci.println(AbstractLoggerFacade.format(message, objects));
		if ((objects.length > 0) && (objects[objects.length - 1] instanceof Throwable)) {
			ci.printStackTrace((Throwable) objects[objects.length - 1]);
		}
	}

	@Override
	public void warn(String message, Object... objects) {
		ci.println("[WARNING] " + AbstractLoggerFacade.format(message, objects));
		if ((objects.length > 0) && (objects[objects.length - 1] instanceof Throwable)) {
			ci.printStackTrace((Throwable) objects[objects.length - 1]);
		}
	}

	@Override
	public void error(String message, Object... objects) {
		ci.println("[ERROR] " + AbstractLoggerFacade.format(message, objects));
		if ((objects.length > 0) && (objects[objects.length - 1] instanceof Throwable)) {
			ci.printStackTrace((Throwable) objects[objects.length - 1]);
		}
	}

	@Override
	public void audit(String message, Object... objects) {
		ci.println("[AUDIT] " + AbstractLoggerFacade.format(message, objects));
		if ((objects.length > 0) && (objects[objects.length - 1] instanceof Throwable)) {
			ci.printStackTrace((Throwable) objects[objects.length - 1]);
		}
	}

}
