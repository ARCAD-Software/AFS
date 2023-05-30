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

import java.sql.Date;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;

/**
 * This class is a wrapper around any implementation of a IProgressMonitor.
 * 
 * It allow to compute an estimation date of completion of the current operation.
 * 
 * @author ARCAD Software
 */
public class TimerProgressMonitor implements IProgressMonitor {

	private final IProgressMonitor parent;
	private volatile double totalWork;
	private volatile double currentWork;
	private volatile long startDate;
	private volatile long endDate;
	private volatile double prevInc;
	private volatile long prevDate;
	private volatile long nextDate;
	
	public TimerProgressMonitor() {
		super();
		parent = new NullProgressMonitor();
	}
	
	public TimerProgressMonitor(IProgressMonitor parent) {
		super();
		if (parent == null) {
			this.parent = new NullProgressMonitor();
		} else {
			this.parent = parent;
		}
	}

	@Override
	public void beginTask(String name, int totalWork) {
		parent.beginTask(name, totalWork);
		startDate = System.currentTimeMillis();
		this.totalWork = totalWork;
		currentWork = 0;
		nextDate = startDate;
	}

	@Override
	public void done() {
		parent.done();
		prevInc = totalWork - currentWork;
		prevDate = nextDate;
		nextDate = System.currentTimeMillis();
		currentWork = totalWork;
		endDate = System.currentTimeMillis();
	}

	@Override
	public void internalWorked(double work) {
		parent.internalWorked(work);
		currentWork = work;
		prevInc = totalWork - currentWork;
		prevDate = nextDate;
		nextDate = System.currentTimeMillis();
		if (currentWork >= totalWork) {
			endDate = System.currentTimeMillis();
		}
	}

	@Override
	public boolean isCanceled() {
		return parent.isCanceled();
	}

	@Override
	public void setCanceled(boolean value) {
		parent.setCanceled(value);
		if (value) {
			endDate = System.currentTimeMillis();
		}
	}

	@Override
	public void setTaskName(String name) {
		parent.setTaskName(name);
	}

	@Override
	public void subTask(String name) {
		parent.subTask(name);
	}

	@Override
	public void worked(int work) {
		parent.worked(work);
		prevInc = work;
		prevDate = nextDate;
		nextDate = System.currentTimeMillis();
		currentWork += work;
		if (currentWork >= totalWork) {
			endDate = System.currentTimeMillis();
		}
	}

	/**
	 * Return the real ProgressMonitor attached to this one.
	 * @return
	 */
	public IProgressMonitor getParent() {
		return parent;
	}

	/**
	 * Return the date of the start of the first begin task call. This method return null if the task is not started yet.
	 * 
	 * @return may return null.
	 */
	public Date getStartDate() {
		if (startDate == 0) {
			return null;
		}
		return new Date(startDate);
	}

	/**
	 * Return the date of the moment where the task ended or was cancelled. This method return null if the task is not started or is currently still running.
	 * 
	 * @return may return null.
	 */
	public Date getEndDate() {
		if (endDate == 0) {
			return null;
		}
		return new Date(startDate);
	}
	
	/**
	 * Return the estimation of the upcoming end date, or the end date it self if the task is already terminated. This method return null if the task is not started yet.
	 * 
	 * <p>
	 * Note that this method return a different date each time it is called, only if the task is currently running.
	 * @return may return null.
	 */
	public Date getEstimationDate() {
		if (endDate > 0) {
			return new Date(endDate);
		}
		if ((currentWork == 0) || (startDate == 0) || (totalWork == 0)) {
			return null;
		}
		// First estimate the ending date using the starting date...
		double tt = (System.currentTimeMillis() * (1 + (totalWork / currentWork))) - (startDate * totalWork / currentWork);
		// Compute the latest increment estimation... mitigate the first estimation by 1/3 of the latest increment estimation.
		if ((prevDate > 0) && (prevInc > 0)) {
			tt = ((tt * 2) + (System.currentTimeMillis() * (1 + (totalWork / prevInc))) - (prevDate * totalWork / prevInc)) / 3;
		}
		return new Date(Math.round(tt));
	}
}
