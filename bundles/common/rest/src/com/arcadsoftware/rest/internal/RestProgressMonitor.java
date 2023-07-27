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
package com.arcadsoftware.rest.internal;

import java.util.ArrayList;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.eclipse.core.runtime.IProgressMonitor;

import com.arcadsoftware.osgi.ISODateFormater;

public class RestProgressMonitor implements IProgressMonitor {

	private class TaskTrace {
		
		private long date;
		private int percent;
		private String name;
		
		protected TaskTrace(String name, int percent) {
			this.name = name;
			date = System.currentTimeMillis();
			this.percent = percent; 
		}

		protected JSONObject getJSON() {
			 JSONObject task = new JSONObject();
			 try {
				task.put("name", name); //$NON-NLS-1$
				task.put("percent", percent); //$NON-NLS-1$
				task.put("startdate", ISODateFormater.toString(date)); //$NON-NLS-1$
			} catch (JSONException e) {}
			return task;
		}

		protected void getXML(StringBuilder sb) {
			sb.append("<task percent=\""); //$NON-NLS-1$
			sb.append(percent);
			sb.append("\" startdate=\""); //$NON-NLS-1$
			sb.append(ISODateFormater.toString(date));
			sb.append("\">"); //$NON-NLS-1$
			if (name != null) {
				for (char c: name.toCharArray()) {
					switch (c) {
					case '<':
						sb.append("&lt;"); //$NON-NLS-1$
						break;
					case '\'':
						sb.append("&apos;"); //$NON-NLS-1$
						break;
					case '>':
						sb.append("&gt;"); //$NON-NLS-1$
						break;
					case '&':
						sb.append("&amp;"); //$NON-NLS-1$
						break;
					default:
						sb.append(c);
					}
				}
			}
			sb.append("</task>"); //$NON-NLS-1$
		}
	}
	
	private transient final boolean keepTaskTrace;
	private transient final int userId;
	private final ArrayList<TaskTrace> taskTrace;
	private int id;
	private double totalWork;
	private double currentWork;
	private long startDate;
	private long endDate;
	private double prevInc;
	private long prevDate;
	private long nextDate;
	private boolean cancelled;
	private boolean done;
	
	public RestProgressMonitor(int userId, boolean keepTaskTrace) {
		super();
		this.userId = userId;
		this.keepTaskTrace = keepTaskTrace;
		taskTrace = new ArrayList<TaskTrace>();
	}

	@Override
	public synchronized void beginTask(String name, int totalWork) {
		if (startDate == 0) {
			startDate = System.currentTimeMillis();
		}
		this.totalWork = totalWork;
		currentWork = 0.0;
		addNewTask(name);
	}

	private void addNewTask(String name) {
		if ((name != null) && (taskTrace.isEmpty() || !name.equals(taskTrace.get(taskTrace.size() - 1).name))) {
			if (keepTaskTrace || taskTrace.isEmpty()) {
				taskTrace.add(new TaskTrace(name, getPercent()));
			} else {
				taskTrace.set(0, new TaskTrace(name, getPercent()));
			}
		}
	}

	@Override
	public synchronized void done() {
		done = true;
		terminate();
	}

	public synchronized void terminate() {
		if (endDate == 0) {
			prevInc = totalWork - currentWork;
			prevDate = nextDate;
			nextDate = System.currentTimeMillis();
			currentWork = totalWork;
			endDate = System.currentTimeMillis();
		}
	}
	
	@Override
	public synchronized void internalWorked(double work) {
		currentWork = work;
		prevInc = totalWork - currentWork;
		prevDate = nextDate;
		nextDate = System.currentTimeMillis();
		if (endDate > 0) {
			endDate = System.currentTimeMillis();
		}
	}

	@Override
	public synchronized boolean isCanceled() {
		return cancelled;
	}

	@Override
	public synchronized void setCanceled(boolean value) {
		cancelled = value;
	}

	@Override
	public synchronized void setTaskName(String name) {
		addNewTask(name);
	}

	@Override
	public synchronized void subTask(String name) {
		addNewTask(name);
	}

	@Override
	public synchronized void worked(int work) {
		prevInc = work;
		prevDate = nextDate;
		nextDate = System.currentTimeMillis();
		currentWork += work;
		if (endDate > 0) {
			endDate = System.currentTimeMillis();
		}
	}

	public synchronized void setId(int id) {
		this.id = id;
	}

	public synchronized boolean isDone() {
		// Return true in the work is terminated (not specifically done !
		return endDate != 0;
	}

	public synchronized long getStartTime() {
		return startDate;
	}

	public synchronized boolean isStarted() {
		return startDate != 0;
	}

	public synchronized int getID() {
		return id;
	}
	
	private int getPercent() {
		if (totalWork <= 0) {
			return 0;
		}
		int result = (int) Math.round((currentWork * 100.0) / totalWork);
		if (result < 0) {
			return 0;
		}
		if (result > 100) {
			return 100;
		}
		return result;
	}
	
	public long getEstimation() {
		if (endDate > 0) {
			return endDate;
		}
		if ((currentWork == 0) || (startDate == 0) || (totalWork == 0)) {
			return 0;
		}
		// First estimate the ending date using the starting date...
		double tt = (System.currentTimeMillis() * (1.0 + (totalWork / currentWork))) - (startDate * totalWork / currentWork);
		// Compute the latest increment estimation... mitigate the first estimation by 1/3 of the latest increment estimation.
		if ((prevDate > 0) && (prevInc > 0)) {
			tt = ((tt * 2.0) + (System.currentTimeMillis() * (1.0 + (totalWork / prevInc))) - (prevDate * totalWork / prevInc)) / 3.0;
		}
		return Math.round(tt);
	}

	public synchronized JSONObject getJSON(boolean ascending) {
		JSONObject result = new JSONObject();
		try {
			result.put("id", id); //$NON-NLS-1$
			result.put("percent", getPercent()); //$NON-NLS-1$
			result.put("cancelled", cancelled); //$NON-NLS-1$
			result.put("completed", done && !cancelled); //$NON-NLS-1$
			if (startDate != 0) {
				result.put("startdate", ISODateFormater.toString(startDate)); //$NON-NLS-1$
				if (endDate != 0) {
					result.put("enddate", ISODateFormater.toString(endDate)); //$NON-NLS-1$
					result.put("ended", true); //$NON-NLS-1$
				} else {
					result.put("estinationdate", ISODateFormater.toString(getEstimation())); //$NON-NLS-1$
					result.put("ended", false); //$NON-NLS-1$
				}
			}
			JSONArray tasks = new JSONArray();
			if (ascending) {
				for (TaskTrace tt: taskTrace) {
					tasks.put(tt.getJSON());
				}
			} else {
				for (int i = taskTrace.size() - 1; i >= 0; i--) {
					tasks.put(taskTrace.get(i).getJSON());
				}
			}
			result.put("tasks", tasks);
		} catch (JSONException e) {
			// Unreachable...
		}
		return result;
	}

	public synchronized String getXML(String tagName, boolean ascending) {
		StringBuilder sb = new StringBuilder('<' + tagName);
		sb.append(" id=\""); //$NON-NLS-1$
		sb.append(id);
		sb.append("\" percent=\""); //$NON-NLS-1$
		sb.append(getPercent());
		sb.append("\" cancelled=\""); //$NON-NLS-1$
		sb.append(cancelled);
		sb.append("\" completed=\""); //$NON-NLS-1$
		sb.append(done && !cancelled);
		if (startDate != 0) {
			sb.append("\" startdate=\""); //$NON-NLS-1$
			sb.append(ISODateFormater.toString(startDate));
			if (endDate != 0) {
				sb.append("\" enddate=\""); //$NON-NLS-1$
				sb.append(ISODateFormater.toString(endDate));
				sb.append("\" ended=\""); //$NON-NLS-1$
				sb.append(true);
			} else {
				sb.append("\" estimationdate=\""); //$NON-NLS-1$
				sb.append(ISODateFormater.toString(getEstimation()));
				sb.append("\" ended=\""); //$NON-NLS-1$
				sb.append(false);
			}
		}
		sb.append("\">"); //$NON-NLS-1$
		if (ascending) {
			for (TaskTrace tt: taskTrace) {
				tt.getXML(sb);
			}
		} else {
			for (int i = taskTrace.size() - 1; i >= 0; i--) {
				taskTrace.get(i).getXML(sb);
			}
		}
		sb.append("</");
		sb.append(tagName);
		sb.append('>');
		return sb.toString();
	}

	public int getUserId() {
		return userId;
	}

}
