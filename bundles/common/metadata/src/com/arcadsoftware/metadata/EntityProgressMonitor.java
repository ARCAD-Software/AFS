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
package com.arcadsoftware.metadata;

import java.sql.Date;
import java.util.ArrayList;

import org.eclipse.core.runtime.IProgressMonitor;

import com.arcadsoftware.metadata.internal.Messages;

/**
 * This ProgressMonitor implementation use 2 Entities to store the progression log:
 * 
 * <p>A "Header" Entity: which will be used to store the current 
 * state of the operation, this include the following attributes:
 * 
 * <ul>
 * <li><b>state</b>: which may be an Integer (may be a reference to a list of states: 0 = waiting, 1 = running, 2 = terminated, 3 = cancelled), or
 * a String, and in that case its value is the localized state code.
 * <li><b>startdate</b>: a Date the moment of the actual starting of the process.
 * <li><b>duration</b>: a Date, used to store the estimated ending date during execution, or the actual ending date when the process is done,
 * or an Integer, storing the duration in millisecond of the process (estimation during execution, actual duration when done).
 * <li><b>progress</b>: a Integer, with a value between 0 and length (100 by default), indicating the actual progression. 
 * If the length value of this attribute is negative or null, the value is a percentage.
 * <li><b>stage</b>: a String storing the latest task (or sub task) name.
 * </ul>
 * 
 * <p>A "Details" entity: is used to log the progression of the operation, it use the same attributes as the Headed does, plus:
 * 
 * <ul>
 * <li><b>header</b>: an integer which is a reference to the Header data.
 * <li><b>substage</b>: a boolean which indicate that the "stage" text is a subtask message, or a String which will be used to store the 
 * subtask message itself. In that case, the "stage" attribute always contains the global task message.  
 * </ul>
 * 
 * <p>All of these attributes are optional. if none are present, or if the entity is not set, then the corresponding information will be dropped.
 * If your entities use different attributes name, each of them must use the metadata "replace" with the name of the replaced attribute. 
 * For instance if in your entity the attribute "endDate" is used to store the estimated data of the end of the process, define a metadata tag
 * "replace" with the value "duration". These strings are case sensitive. 
 * 
 * <p>The Header data must be created before to use this class. Only one data will be used during the operation, it will be updated each time the
 * progression change. The details data are created (and linked to the header data) during the process. 
 * A process may produce zero to many details datas, so a way to clear these datas must be provided to the end-users.
 * 
 * <p>Please note that if both entities are null, nothing will be recorded... and in that case you should use another ProgressMonitor implementation !.
 * 
 * @author ARCAD Software
 */
public class EntityProgressMonitor implements IProgressMonitor {

	private final MetaDataEntity entityDetails;
	private final MetaDataEntity entityHeader;
	private final ArrayList<MetaDataAttribute> headerAttributes;
	private final ArrayList<MetaDataAttribute> detailAttributes;
	private final ArrayList<Object> lastHeader;
	private final ArrayList<Object> lastDetails;
	private final int headerPMax;
	private final int detailPMax;
	private final int headerID;
	private double totalWork;
	private double currentWork;
	private long startDate;
	private long endDate;
	private double prevInc;
	private long prevDate;
	private long nextDate;
	private boolean cancel;
	private boolean recordProgression;

	/**
	 * Create a ProgressMonitor using the given Entities to store the execution progression. 
	 * 
	 * @param details is the Entity used to store the complete log of the operation. May be null.
	 */
	public EntityProgressMonitor(MetaDataEntity details) {
		this(null, 0, details);
	}

	/**
	 * Create a ProgressMonitor using the given Entities to store the execution progression. 
	 * 
	 * @param header is the Entity used as an Header, only one data will be updated during the operation. May be null.
	 * @param reference is the ID of the Header Data. <b>It must created before the creation of this object.</b> May be zero.
	 */
	public EntityProgressMonitor(MetaDataEntity header, int reference) {
		this(header, reference, null);
	}

	/**
	 * Create a ProgressMonitor using the given Entities to store the execution progression. 
	 * 
	 * @param reference is the ID of a reference Entity (corresponding to the header attribute). <b>It must created before the creation of this object.</b> May be zero.
	 * @param details is the Entity used to store the complete log of the operation. May be null.
	 */
	public EntityProgressMonitor(int reference, MetaDataEntity details) {
		this(null, reference, details);
	}

	/**
	 * Create a ProgressMonitor using the given Entities to store the execution progression. 
	 * 
	 * @param header is the Entity used as an Header, only one data will be updated during the operation. May be null.
	 * @param reference is the ID of the Header Data. <b>It must created before the creation of this object.</b> May be zero.
	 * @param details is the Entity used to store the complete log of the operation. May be null.
	 */
	public EntityProgressMonitor(MetaDataEntity header, int reference, MetaDataEntity details) {
		super();
		recordProgression = true;
		headerAttributes = new ArrayList<MetaDataAttribute>();
		detailAttributes = new ArrayList<MetaDataAttribute>();
		entityHeader = header;
		headerID = reference;
		entityDetails = details;
		if (header != null) {
			headerAttributes.add(getAttribute(header, "state")); //$NON-NLS-1$
			headerAttributes.add(getAttribute(header, "startdate")); //$NON-NLS-1$
			headerAttributes.add(getAttribute(header, "duration")); //$NON-NLS-1$
			MetaDataAttribute a = getAttribute(header, "progress"); //$NON-NLS-1$
			headerAttributes.add(a);
			if ((a != null) && (a.getLength() > 1)) {
				headerPMax = a.getLength();
			} else {
				headerPMax = 100;
			}
			headerAttributes.add(getAttribute(header, "stage")); //$NON-NLS-1$
			clearAllNull(headerAttributes);
		} else {
			headerPMax = 100;
		}
		lastHeader = new ArrayList<Object>(5);
		if (headerID > 0) {
			lastHeader.add(null);
			lastHeader.add(null);
			lastHeader.add(null);
			lastHeader.add(null);
			lastHeader.add(null);
		}
		if (details != null) {
			detailAttributes.add(getAttribute(header, "state")); //$NON-NLS-1$
			detailAttributes.add(getAttribute(header, "startdate")); //$NON-NLS-1$
			detailAttributes.add(getAttribute(header, "duration")); //$NON-NLS-1$
			MetaDataAttribute a = getAttribute(header, "progress"); //$NON-NLS-1$
			detailAttributes.add(a);
			if ((a != null) && (a.getLength() > 1)) {
				detailPMax = a.getLength();
			} else {
				detailPMax = 100;
			}
			detailAttributes.add(getAttribute(header, "stage")); //$NON-NLS-1$
			detailAttributes.add(getAttribute(header, "substage")); //$NON-NLS-1$
			if (headerID > 0) {
				detailAttributes.add(getAttribute(header, "header")); //$NON-NLS-1$
			}
			clearAllNull(detailAttributes);
		} else {
			detailPMax = 100;
		}
		lastDetails = new ArrayList<Object>(7);
		lastDetails.add(null);
		lastDetails.add(null);
		lastDetails.add(null);
		lastDetails.add(null);
		lastDetails.add(null);
		lastDetails.add(null);
		if (headerID > 0) {
			lastDetails.add(headerID);
		}
	}

	private void clearAllNull(ArrayList<MetaDataAttribute> attributes) {
		for (MetaDataAttribute a: attributes) {
			if (a != null) {
				return;
			}
		}
		attributes.clear();
	}

	private MetaDataAttribute getAttribute(MetaDataEntity entity, String code) {
		MetaDataAttribute att = entity.getAttribute(code);
		if (att != null) {
			return att;
		}
		for (MetaDataAttribute a: entity.getAttributes().values()) {
			if (code.equals(a.getMetadata().getString("replace"))) { //$NON-NLS-1$
				return a;
			}
		}
		return null;
	}

	@Override
	public void beginTask(String name, int totalWork) {
		startDate = System.currentTimeMillis();
		if (totalWork > 0) {
			this.totalWork = totalWork;
		} else {
			this.totalWork = 100;
		}
		currentWork = 0;
		nextDate = startDate;
		udpadeHeader(name, 1, 0);
		addDetail(name, "", 1, 0); //$NON-NLS-1$
	}

	@Override
	public void done() {
		prevInc = totalWork - currentWork;
		prevDate = nextDate;
		nextDate = System.currentTimeMillis();
		currentWork = totalWork;
		endDate = System.currentTimeMillis();
		udpadeHeader(null, 2, headerPMax);
		addDetail(null, null, 2, detailPMax);
	}

	@Override
	public void internalWorked(double work) {
		currentWork = work;
		prevInc = totalWork - currentWork;
		prevDate = nextDate;
		nextDate = System.currentTimeMillis();
		if (currentWork >= totalWork) {
			endDate = System.currentTimeMillis();
		}
		udpadeHeader(null, null, (int) Math.round((headerPMax * currentWork) / totalWork));
		addDetail(null, null, null, (int) Math.round((detailPMax * currentWork) / totalWork));
	}

	@Override
	public boolean isCanceled() {
		return cancel;
	}

	@Override
	public void setCanceled(boolean value) {
		if (value != cancel) {
			cancel = value;
			endDate = System.currentTimeMillis();
			udpadeHeader(null, 3, null);
			addDetail(null, null, 3, null);
		}
	}

	@Override
	public void setTaskName(String name) {
		udpadeHeader(name, null, null);
		addDetail(name, "", null, null); //$NON-NLS-1$
	}

	@Override
	public void subTask(String name) {
		udpadeHeader(name, null, null);
		addDetail(null, name, null, null);
	}

	@Override
	public void worked(int work) {
		prevInc = work;
		prevDate = nextDate;
		nextDate = System.currentTimeMillis();
		currentWork += work;
		if (currentWork >= totalWork) {
			endDate = System.currentTimeMillis();
		}
		udpadeHeader(null, null, (int) Math.round((headerPMax * currentWork) / totalWork));
		addDetail(null, null, null, (int) Math.round((detailPMax * currentWork) / totalWork));
	}

	/**
	 * Return the entity used to log the current state of the operation.
	 * 
	 * @return may return null.
	 */
	public MetaDataEntity getHeaderEntity() {
		return entityHeader;
	}

	/**
	 * Return the Entity used to store the details log of the progression of the operation.
	 * 
	 * @return may return null.
	 */
	public MetaDataEntity getDetailEntity() {
		return entityDetails;
	}

	/**
	 * Get the ID of the "header" data used as reference in the details log.
	 * 
	 * @return may return a negative or null number.
	 */
	public int getHeaderReference() {
		return headerID;
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
		return new Date(endDate);
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

	/**
	 * If true the modification of the progression will be recorded into the Details log, if false only the task name modification will be recorded.
	 * 
	 * @return
	 */
	public boolean isRecordProgression() {
		return recordProgression;
	}

	/**
	 * If true the modification of the progression will be recorded into the Details log, if false only the task name modification will be recorded.
	 * 
	 * <p>
	 * Default value is true.
	 * 
	 * @param recordProgression
	 */
	public void setRecordProgression(boolean recordProgression) {
		this.recordProgression = recordProgression;
	}


	/**
	 * Replace the state ID by a localized label.
	 * 
	 * @param state ID (0 to 3).
	 * @return
	 */
	protected String getStateLabel(int state) {
		switch (state) {
		case 0: return Messages.ProgressState_0;
		case 1: return Messages.ProgressState_1;
		case 2: return Messages.ProgressState_2;
		case 3: return Messages.ProgressState_3;
		}
		return null;
	}

	/**
	 * Test if the two object are equals.
	 * 
	 * @param o1
	 * @param o2
	 * @return
	 */
	protected final boolean isNullOrNotEquals(Object o1, Object o2) {
		if (o1 == null) {
			return o2 != null;
		}
		return !o1.equals(o2);
	}

	/**
	 * Update the Header data.
	 * 
	 * @param task may be null
	 * @param state may be null
	 * @param progress may be null
	 */
	protected void udpadeHeader(String task, Integer state, Integer progress) {
		if ((entityHeader != null) && (headerID > 0) && (headerAttributes.size() > 0)) {
			boolean changed = false;
			// Update State...
			if ((state != null) && (headerAttributes.get(0) != null)) {
				if ("string".equalsIgnoreCase(headerAttributes.get(0).getType())) { //$NON-NLS-1$
					String s = getStateLabel(state);
					if (isNullOrNotEquals(lastHeader.get(0), s)) {
						changed = true;
						lastHeader.set(0, s);
					}
				} else if (isNullOrNotEquals(lastHeader.get(0), state)) {
					changed = true;
					lastHeader.set(0, state);
				}
			}
			// Set Start Date (constant !)
			lastHeader.set(1, getStartDate());
			// Set current estimation (variable but not enough to lead to an update in database...)
			if (changed && (headerAttributes.get(2) != null)) {
				if (headerAttributes.get(2).isNumeric()) {
					lastHeader.set(2, (int) (getEstimationDate().getTime() - startDate));
				} else {
					lastHeader.set(2, getEstimationDate().getTime());
				}
			}
			// Update the progression.
			if ((progress != null) && (headerAttributes.get(3) != null) && isNullOrNotEquals(lastHeader.get(3), progress)) {
				changed = true;
				lastHeader.set(3, progress);
			}
			// Update the current stage message.
			if ((task != null) && (headerAttributes.get(4) != null) && isNullOrNotEquals(task, lastHeader.get(4))) {
				changed = true;
				lastHeader.set(4, task);
			}
			if (changed) {
				entityHeader.dataUpdate(headerID, headerAttributes, lastHeader);
			}
		}
	}

	/**
	 * Insert a log details.
	 * 
	 * @param task may be null
	 * @param subtask may be null
	 * @param state may be null
	 * @param progress may be null
	 */
	protected void addDetail(String task, String subtask, Integer state, Integer progress) {
		if ((entityDetails != null) && (detailAttributes.size() > 0)) {
			boolean changed = false;
			// Set the state...
			if ((state != null) && (detailAttributes.get(0) != null)) {
				if ("string".equalsIgnoreCase(detailAttributes.get(0).getType())) { //$NON-NLS-1$
					String s = getStateLabel(state);
					if (isNullOrNotEquals(lastDetails.get(0), s)) {
						changed = true;
						lastDetails.set(0, s);
					}
				} else if (isNullOrNotEquals(lastDetails.get(0), state)) {
					changed = true;
					lastDetails.set(0, state);
				}
			}
			// Set startdate
			lastDetails.set(1, getStartDate());
			// Set duration
			if (changed && (detailAttributes.get(2) != null)) {
				if (detailAttributes.get(2).isNumeric()) {
					lastDetails.set(2, (int) (getEstimationDate().getTime() - startDate));
				} else {
					lastDetails.set(2, getEstimationDate().getTime());
				}
			}
			// set progression
			if ((progress != null) && (detailAttributes.get(3) != null) && isNullOrNotEquals(lastDetails.get(3), progress)) {
				changed = changed || recordProgression;
				lastDetails.set(3, progress);
			}
			// Set stage name
			if ((task != null) && (detailAttributes.get(4) != null) && isNullOrNotEquals(task, lastDetails.get(4))) {
				changed = true;
				lastDetails.set(4, task);
			}
			// Set substage
			if ((subtask != null) && (detailAttributes.get(5) != null)) {
				if (detailAttributes.get(5).isBoolean()) {
					if (subtask.isEmpty()) {
						changed = true;
						lastDetails.set(5, Boolean.FALSE);
					} else if (isNullOrNotEquals(subtask, lastDetails.get(4))) {
						changed = true;
						lastDetails.set(4, subtask);
						lastDetails.set(5, Boolean.TRUE);
					}
				} else {
					changed = true;
					lastDetails.set(5, subtask);
				}
			}
			// Header is already set.
			if (changed) {
				entityDetails.dataCreate(detailAttributes, lastDetails);
			}
		}		
	}
}
