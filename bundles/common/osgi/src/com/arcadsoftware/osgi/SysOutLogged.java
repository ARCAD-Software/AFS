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

/**
 * Implement a simple logger that print out any message to the System console.
 * 
 * <p>
 * This class may be extended to redirect the log to another Stream. To do so override the log, warn, error and debug methods.
 * 
 * @author ARCAD Software
 */
public class SysOutLogged implements ILoggedPlugin {

	public final void log(String message) {
		log(message, null);
	}

	public void log(String message, Throwable e) {
		if (message != null) {
			System.out.println(message);
		}
		if (e != null) {
			e.printStackTrace();
		}
	}

	public final void log(Throwable e) {
		log(e.getLocalizedMessage(), e);
	}

	public void error(String message, Throwable e) {
		if (message != null) {
			System.err.println(message);
		} else if (e != null) {
			System.err.println(e.getLocalizedMessage());
		}
		if (e != null) {
			e.printStackTrace();
		}
	}

	public final void error(String message) {
		error(message, null);
	}

	public final void error(Throwable e) {
		error(e.getLocalizedMessage(), e);
	}

	public final void warn(String message) {
		warn(message, null);
	}

	public void warn(String message, Throwable e) {
		if (message != null) {
			System.err.println("[WARN] " + message); //$NON-NLS-1$
		}
		if (e != null) {
			e.printStackTrace();
		}
	}

	public final void warn(Throwable e) {
		warn(e.getLocalizedMessage(), e);
	}

	public final void debug(String message) {
		debug(message, null);
	}

	public void debug(String message, Throwable e) {
		if (message != null) {
			System.out.println("[DEBUG] " + message); //$NON-NLS-1$
		}
		if (e != null) {
			e.printStackTrace(System.out);
		}
	}

	public final void debug(Throwable e) {
		debug(e.getLocalizedMessage(), e);
	}

}