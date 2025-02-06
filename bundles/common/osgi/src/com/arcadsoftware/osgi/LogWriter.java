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

import java.io.Writer;

/**
 * Implement a PrintWriter interface to the OSGi Log service (through the ILoggedPlugin interface).
 *
 * @author ARCAD Software
 */
public class LogWriter extends Writer {

	private static final String[] LEVEL = new String[] {null,null,"WARN","INFO","DEBUG"};
	
	/**
	 * An error message (Value 1).
	 * 
	 * <p>
	 * This log entry indicates the bundle or service may not be functional.
	 */
	public static final int	LOG_ERROR	= 1;
	
	/**
	 * A warning message (Value 2).
	 * 
	 * <p>
	 * This log entry indicates a bundle or service is still functioning but may
	 * experience problems in the future because of the warning condition.
	 */
	public static final int	LOG_WARNING	= 2;
	
	/**
	 * An informational message (Value 3).
	 * 
	 * <p>
	 * This log entry may be the result of any change in the bundle or service
	 * and does not indicate a problem.
	 */
	public static final int	LOG_INFO	= 3;

	/**
	 * A debugging message (Value 4).
	 * 
	 * <p>
	 * This log entry is used for problem determination and may be irrelevant to
	 * anyone but the bundle developer.
	 */
	public static final int	LOG_DEBUG	= 4;
	
	private volatile String buffer = null;
	private final ILoggedPlugin activator;
	private volatile boolean autoFlush;
	private volatile int level = LOG_DEBUG;
	
	public LogWriter(ILoggedPlugin activator) {
		super();
		this.activator = activator; 
	}

	public LogWriter(ILoggedPlugin activator, int logLevel) {
		this(activator);
		level = logLevel;
	}
	
	public boolean isAutoFlush() {
		return autoFlush;
	}

	public void setAutoFlush(boolean autoFlush) {
		this.autoFlush = autoFlush;
	}

	public int getLogLevel() {
		return level;
	}

	public void setLogLevel(int level) {
		this.level = level;
	}

	@Override
	public void write(char[] cbuf, int off, int len) {
		if (buffer == null) {
			buffer = new String(cbuf, off, len);
		} else {
			buffer = buffer.concat(new String(cbuf, off, len));
		}
		if (autoFlush) {
			flush();
		}
	}

	@Override
	public void flush() {
		String b;
		synchronized (lock) {
			b = buffer;
			buffer = null;
		}
		if (b != null) {
			if (activator == null) {
				if (level == LOG_ERROR) {
					System.err.println(b);
				} else {
					System.out.print('[');
					System.out.print(LEVEL[level]);
					System.out.print(']');
					System.out.print(' ');
					System.out.println(b);
				}
			} else {
				
			}
		}
	}

	@Override
	public void close() {
		flush();
	}

}
