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
import java.util.List;

import com.arcadsoftware.beanmap.BeanMapList;

public interface ISchedulerService {

	public final static int ACTION_SUCCEED=0;
	public final static int ACTION_FAILED_ALREADYREGISTERED=1;
	public final static int ACTION_FAILED_INVALID_TASKID=2;
	public final static int ACTION_FAILED_INVALID_DATE=3;
	public final static int ACTION_FAILED_TASK_NOT_FOUND=4;
	public final static int ACTION_FAILED_SERVICE_NOT_FOUND=5;
	
	/**
	 * Register new record to be scheduled, and schedule it.
	 * 
	 * @param record
	 */
	public int register(IScheduledItem record);
	
	/**
	 * Unschedule corresponding scheduled task and unregister record
	 * 
	 * @param record
	 */
	public int unregister(IScheduledItem record);
	
	/**
	 * Unschedule corresponding scheduled task and unregister record
	 * 
	 * @param record
	 */
	public int unregister(int recordId);
	
	/**
	 * Reschedule record task is already registered. Register and start it if not started yet.
	 * @param record
	 */
	public void reset(IScheduledItem record);
	
	/**
	 * Unschedule and unregister list of records
	 * 
	 * @param record
	 */
	public void unregisterList(List<Integer> ids);
	
	/**
	 * Start scheduler managed by service
	 */
	public void start();

	/**
	 * Stop scheduler managed by service
	 */
	public void stop();
	
	/**
	 * 
	 * @param record
	 * @return
	 */
	public Date getNextScheduledTime(IScheduledItem record);
	
	/**
	 * 
	 * @param recordId
	 * @param startdate
	 * @param enddate
	 * @return
	 */
	public List<Date> getNextScheduledTimes(int recordId, Date startdate, Date enddate);
	
	/**
	 * 
	 * @param type
	 * @return
	 */
	public BeanMapList listScheduledItems(String type);
	
	/**
	 * 
	 * @param type
	 * @param start
	 * @param end
	 * @return
	 */
	public BeanMapList listScheduledItemsInPeriod(String type, Date start, Date end);
	
	/**
	 * 
	 * @return
	 */
	public List<IScheduledItem> getScheduledItems();	
}