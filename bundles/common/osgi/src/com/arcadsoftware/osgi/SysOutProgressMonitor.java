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

import java.net.URL;

import org.eclipse.core.runtime.IProgressMonitor;

/**
 * A ProgressMonitor implementation that print messages to screen console.
 * 
 * <p>
 * Print the current Task name. and a progression bar, which is equal to the quart of the line length.
 * 
 * @author ARCAD Software
 * @since 1.2.2
 */
public class SysOutProgressMonitor implements IProgressMonitor {

	private static boolean NOTINDEBUG;
	
	static {
		// Get an information about Eclipse debug state (= not in a jar !)
		URL r = SysOutProgressMonitor.class.getResource('/' + SysOutProgressMonitor.class.getName().replace('.', '/') + ".class"); //$NON-NLS-1$
		NOTINDEBUG = (r == null) || r.toExternalForm().toLowerCase().startsWith("jar:"); //$NON-NLS-1$
	}
	
	private final int textlen;
	private final int barlen;
	private final boolean useCarriageReturn;
	private volatile boolean cancel;
	private double totalWork;
	private double work;
	private String task;
	private String latestBar = "";
	private int latestWork;
	
	public SysOutProgressMonitor() {
		super();
		barlen = 20;
		textlen = 54;
		useCarriageReturn = NOTINDEBUG;
	}
	
	public SysOutProgressMonitor(int lineLength) {
		super();
		if (lineLength < 0) {
			barlen = 20;
			textlen = 54;
		} else if (lineLength < 12) {
			barlen = 3;
			textlen = 3;
		} else {
			barlen = lineLength / 4;
			textlen = lineLength - barlen - 6;
		}
		this.useCarriageReturn = NOTINDEBUG;
	}
	
	public SysOutProgressMonitor(int lineLength, boolean useCarriageReturn) {
		super();
		if (lineLength < 0) {
			barlen = 20;
			textlen = 54;
		} else if (lineLength < 12) {
			barlen = 3;
			textlen = 3;
		} else {
			barlen = lineLength / 4;
			textlen = lineLength - barlen - 6;
		}
		this.useCarriageReturn = NOTINDEBUG && useCarriageReturn;
	}
	
	/**
	 * Print the current progression text.
	 * 
	 * @param task may be null.
	 * @param percent may be a negative or superior to 100% 
	 */
	protected void print(String task, double percent) {
		if (task == null) {
			task = ""; //$NON-NLS-1$
		}
		if (percent < 0) {
			percent = 0;
		} else if (percent > 100) {
			percent = 100;
		}
		if (useCarriageReturn) {
			StringBuilder bar = new StringBuilder(" "); //$NON-NLS-1$
			for(double i = (barlen * percent) / 100; i > 1; i--) {
				bar.append('#');
			}
			while (bar.length() < barlen) {
				bar.append('.');
			}
			bar.append(String.format("%3.0f%% ", percent)); //$NON-NLS-1$
			if (task.length() > textlen) {
				bar.append(task.substring(0, textlen));
			} else {
				bar.append(task);
				for (int i = task.length(); i < textlen; i++) {
					bar.append(' ');
				}
			}
			// Carriage Return does not work into Eclipse Console.
			// This is a know bug: https://bugs.eclipse.org/bugs/show_bug.cgi?id=76936
			bar.append('\r');
			String s = bar.toString();
			if (!latestBar.equals(s)) {
				latestBar = s;
				System.out.print(bar.toString());
			}
		} else {
			if (!latestBar.equals(task)) {
				latestBar = task;
				System.out.println();
				System.out.print("-->"); //$NON-NLS-1$
				if (task.length() > textlen) {
					System.out.print(task.substring(0, textlen));
					System.out.print(": "); //$NON-NLS-1$
				} else {
					System.out.print(task);
					System.out.print(": "); //$NON-NLS-1$
					for (int i = task.length(); i < textlen; i++) {
						System.out.print(" "); //$NON-NLS-1$
					}
				}
				for(int i = 1; i <= latestWork; i++) {
					System.out.print("#"); //$NON-NLS-1$
				}
			}
			int curwork = (int) ((barlen * percent) / 100);
			while (curwork > latestWork) {
				System.out.print("#"); //$NON-NLS-1$
				latestWork++;
			}
			if (percent == 100) {
				System.out.print("."); //$NON-NLS-1$
				latestWork++;
			}
		}
	}
	
	@Override
	public void beginTask(String name, int totalWork) {
		this.totalWork = totalWork;
		task = name;
		if (totalWork > 0) {
			print(task, (100 * work) / totalWork);
		}
	}

	@Override
	public void done() {
		work = totalWork;
		if (totalWork > 0) {
			print(task, 100);
			System.out.println();
		}
	}

	@Override
	public void internalWorked(double work) {
		this.work = work;
		if (totalWork > 0) {
			print(task, (100 * this.work) / totalWork);
		}
	}

	@Override
	public boolean isCanceled() {
		return cancel;
	}

	@Override
	public void setCanceled(boolean value) {
		cancel = value;
	}

	@Override
	public void setTaskName(String name) {
		task = name;
		if (totalWork > 0) {
			print(task, (100 * work) / totalWork);
		}
	}

	@Override
	public void subTask(String name) {
		task = name;
		if (totalWork > 0) {
			print(task, (100 * work) / totalWork);
		}
	}

	@Override
	public void worked(int work) {
		this.work += work;
		if (totalWork > 0) {
			print(task, (100 * this.work) / totalWork);
		}
	}
}