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
package com.arcadsoftware.server.scheduler;

import java.util.Date;

/**
 *
 */
public interface IScheduledItem {
	
	public static final String ENTITY_SCHEDULEDITEM="scheduleditem"; //$NON-NLS-1$
	public final static String SCHEDULEDITEM_ATTR_TYPE = "type"; //$NON-NLS-1$
	public final static String SCHEDULEDITEM_ATTR_NAME = "name"; //$NON-NLS-1$
	public final static String SCHEDULEDITEM_ATTR_OCCURRENCE = "occurence"; //$NON-NLS-1$
	public final static String SCHEDULEDITEM_ATTR_OCCURRENCE_TIME = "occurencetime"; //$NON-NLS-1$
	public final static String SCHEDULEDITEM_ATTR_INSTANCE = "instance"; //$NON-NLS-1$
	
	/**
	 * 
	 * @return
	 */
	public int getId();
	
	/**
	 * 
	 * @return
	 */
	public String getName();
	
	/**
	 * 
	 * @return
	 */
	public String getType();
	
	/**
	 * 
	 * @return
	 */
	public boolean isPeriodic();
	
	/**
	 * 
	 * @return
	 */
	public int getHours();
	
	/**
	 * 
	 * @return
	 */
	public int getMinutes();
	
	/**
	 * 
	 * @return
	 */
	public int getDays();
	
	/**
	 * Returns formatted time period
	 * 
	 * @return cron-formatted expression; null if task must not be scheduled
	 * @see http://www.sauronsoftware.it/projects/cron4j/manual.php#p02
	 */
	public String getFormattedTime();
	
	/**
	 * 
	 * @return
	 */
	public String getOwner();
	
	/**
	 * 
	 * @param id
	 */
	public void setId(int id);
	
	/**
	 * 
	 * @param name
	 */
	public void setName(String name);
	
	/**
	 * 
	 * @param periodic
	 */
	public void setPeriodic(boolean periodic);
	
	/**
	 * 
	 * @param hours
	 */
	public void setHours(int hours);
	
	/**
	 * 
	 * @param minutes
	 */
	public void setMinutes(int minutes);
	
	/**
	 * 
	 * @param days
	 */
	public void setDays(int days);
	
	/**
	 * 
	 * @param date
	 */
	public void setFixedDate(Date date);
	
	/**
	 * 
	 * @return
	 */
	public Date getFixedDate();
	
	/**
	 * Get the user internal ID associated to the corresponding scheduled task.
	 * @return
	 */
	public int getUser();
	
	/**
	 * Set the User Internal ID associated to the corresponding scheduled task.
	 * 
	 * @param userId
	 */
	public void setUser(int userId);
	
	/**
	 * 
	 * @return
	 */
	public String getDaysInMonth();
	
	/**
	 * 
	 * @param daysInMonth
	 */
	public void setDaysInMonth(String daysInMonth);
	
	/**
	 * Check user gets enough rights
	 * 
	 * @return
	 */
	public boolean canExecute();
	
	/**
	 * 
	 * @return
	 */
	public boolean execute();
	
	/**
	 * Prepare Next Execution
	 */
	public void prepareNext();
	
	/**
	 * Clean Execution
	 */
	public void clean();
	
	/**
	 * Return true is this Scheduled item go past.
	 * @return
	 */
	public boolean isPassed();
}
