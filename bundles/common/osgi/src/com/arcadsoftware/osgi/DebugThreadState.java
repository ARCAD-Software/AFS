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
 * This class allow you to register a monitor that may be used to post the stack trace of a thread from time to time.
 *
 * <p>
 * To use it just start a new thread with a {@link DebugThreadState} and call stop() when the process to monitor is terminated.
 * 
 * <p>
 * Or call :
 * 
 * <code><pre>
 * DebugThreadState monitor = DebugThreadState.create(Thread.currentThread(), activator, 30000);
 * try {
 *     ....
 * } finally {
 *     monitor.stop();
 * }
 * </pre></code>
 * @author ARCAD Software
 */
public class DebugThreadState implements Runnable {

	public static final DebugThreadState create(ILoggedPlugin activator, long timeWait) {
		DebugThreadState target = new DebugThreadState(Thread.currentThread(), activator, timeWait);
		Thread t = new Thread(target, "Debug Thread State Monitor"); //$NON-NLS-1$
		t.start();
		return target;
	}

	public static final DebugThreadState create(Thread monitoredThread, ILoggedPlugin activator,
			long timeWait) {
		DebugThreadState target = new DebugThreadState(monitoredThread, activator, timeWait);
		Thread t = new Thread(target);
		t.start();
		return target;
	}
	
	private final ILoggedPlugin activator;
	private final Thread monitoredThread;
	private final long timeWait;
	private volatile boolean working = true;
	
	/**
	 * 
	 * @param monitoredThread the thread to monitor.
	 * @param activator and activator used to log the thread informations.
	 * @param timeWait the duration to wait before printing the thread information.
	 */
	public DebugThreadState(Thread monitoredThread, ILoggedPlugin activator,
			long timeWait) {
		super();
		this.monitoredThread = monitoredThread;
		this.activator = activator;
		if (timeWait < 1000) {
			this.timeWait = 1000;
		} else {
			this.timeWait = timeWait;
		}
	}

	private void error(String message, Throwable e) {
		if (activator != null) {
			activator.error(message, e);
		} else {
			System.err.println(message);
			e.printStackTrace();
		}
	}

	private void debug(String message) {
		if (activator != null) {
			activator.debug(message);
		} else {
			System.out.println(message);
		}
	}

	public void run() {
		if (monitoredThread != null) {
			try {
				while (working) {
					if (!monitoredThread.isAlive()) {
						return;
					}
					Thread.sleep(timeWait);
					if (!monitoredThread.isAlive()) {
						return;
					}
					if (!working) {
						return;
					}
					StringBuilder sb = new StringBuilder();
					sb.append(String.format("Delayed \"%s\" thread information. (This is NOT an error !!!)\n", monitoredThread.getName()));
					sb.append(String.format(" [State: %s, Priority: %d]", monitoredThread.getState().name(), monitoredThread.getPriority()));
					StackTraceElement[] st = monitoredThread.getStackTrace();
					if (st != null) {
						int i = 0;
						for(StackTraceElement e: st) {
							sb.append("\n  "); //$NON-NLS-1$
							sb.append(e.toString());
							if (i++ > 100) {
								break;
							}
						}
					}
					debug(sb.toString());
				}
			} catch (Throwable e) {
				error(e.getLocalizedMessage(), e);
			}
		}
	}

	public void stop() {
		working = false;
	}
}
