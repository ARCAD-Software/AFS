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
package com.arcadsoftware.server.scheduler.internal;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.IMapperService;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.sql.MapperSQLService;
import com.arcadsoftware.server.scheduler.IScheduledItem;
import com.arcadsoftware.server.scheduler.ISchedulerService;
import com.arcadsoftware.server.scheduler.internal.task.ScheduledCronTask;

import it.sauronsoftware.cron4j.Predictor;
import it.sauronsoftware.cron4j.Scheduler;
import it.sauronsoftware.cron4j.SchedulingPattern;
import it.sauronsoftware.cron4j.Task;

public class SchedulerService implements ISchedulerService {

	private final Map<Integer, String> records; // record ID, scheduled task id
	private final Scheduler scheduler;
	private final Activator activator;

	public SchedulerService(Activator activator) {
		super();
		this.activator = activator;
		records = new HashMap<Integer, String>();
		// test class existence
		scheduler = new Scheduler();
	}

	public void release() {
		scheduler.stop();
	}

	/**
	 * Add scheduled task for given record
	 *
	 * @param record
	 * @return Scheduled Task identifier
	 */
	protected String addScheduledTask(IScheduledItem record) {
		final String delayPattern = record.getFormattedTime();
		activator.info("--- launching task for " + record.getName() + " - " + delayPattern);
		final ScheduledCronTask task = new ScheduledCronTask(activator, this, record);
		return scheduler.schedule(delayPattern, task);
	}

	@Override
	public int register(IScheduledItem record) {
		if (!records.containsKey(record.getId())) {
			if (record.isPassed()) {
				System.err.println("-> Invalid Date");
				activator.debug("Past date");
				return ISchedulerService.ACTION_FAILED_INVALID_DATE;
			}
			final String identifier = addScheduledTask(record);
			if (identifier != null) {
				records.put(Integer.valueOf(record.getId()), identifier);
				activator.info("--- Next run at " + getNextScheduledTime(record));
				System.out.println("-> Next run at " + getNextScheduledTime(record));
				return ISchedulerService.ACTION_SUCCEED;
			} else {
				System.err.println("-> Invalid Task Identifier");
				activator.info("** Invalid Task Identifier");
				return ISchedulerService.ACTION_FAILED_INVALID_TASKID;
			}
		} else {
			System.err.println("-> Already registered!");
			activator.info("** Already registered!");
			return ISchedulerService.ACTION_FAILED_ALREADYREGISTERED;
		}
	}

	@Override
	public int unregister(IScheduledItem record) {
		if (records.containsKey(record.getId())) {
			final StringBuilder log = new StringBuilder("UnRegistering: " + record.getId());
			log.append("\n--- Before unregistering ::: Next run at " + getNextScheduledTime(record));
			scheduler.deschedule(records.get(record.getId()));
			log.append("\n--- After unregistering ::: Next run at " + getNextScheduledTime(record));
			activator.info(log.toString());
			records.remove(record.getId());
		}
		return ISchedulerService.ACTION_SUCCEED;
	}

	@Override
	public int unregister(int recordId) {
		if (records.containsKey(recordId)) {
			final StringBuilder log = new StringBuilder("UnRegistering: " + recordId);
			log.append("\n--- Before unregistering ::: Next run at " + getNextScheduledTime(recordId));
			scheduler.deschedule(records.get(recordId));
			log.append("\n--- After unregistering ::: Next run at " + getNextScheduledTime(recordId));
			activator.info(log.toString());
			records.remove(recordId);
			return ISchedulerService.ACTION_SUCCEED;
		} else {
			return ISchedulerService.ACTION_FAILED_TASK_NOT_FOUND;
		}

	}

	@Override
	public void reset(IScheduledItem record) {
		if (records.containsKey(record.getId())) {
			final String taskId = records.get(record.getId());
			scheduler.reschedule(taskId, record.getFormattedTime());
			// Update Task Scheduled Item new values
			final Task schtask = scheduler.getTask(taskId);
			if ((schtask != null) && (schtask instanceof ScheduledCronTask)) {
				((ScheduledCronTask) schtask).setItem(record);
			}
		} else {
			register(record);
		}
	}

	@Override
	public void unregisterList(List<Integer> ids) {
		for (final Integer id : ids) {
			unregister(id);
		}
	}

	@Override
	public void start() {
		if (!scheduler.isStarted()) {
			scheduler.start();
		}
	}

	@Override
	public void stop() {
		if (scheduler.isStarted()) {
			scheduler.stop();
		}
	}

	@Override
	public Date getNextScheduledTime(IScheduledItem record) {
		if (records.containsKey(record.getId())) {
			final SchedulingPattern pattern = scheduler.getSchedulingPattern(records.get(record.getId()));
			// activator.info(" Scheduling Pattern = "+ pattern.toString());
			if (pattern != null) {
				final Predictor p = new Predictor(pattern);
				// activator.info(" Next run on"+ p.nextMatchingDate());
				return p.nextMatchingDate();
			}
		}

		return null;
	}

	public Date getNextScheduledTime(int recordId) {
		if (records.containsKey(recordId)) {

			final SchedulingPattern pattern = scheduler.getSchedulingPattern(records.get(recordId));
			if (pattern != null) {
				// activator.info(" Scheduling Pattern = "+ pattern.toString());
				final Predictor p = new Predictor(pattern);
				// activator.info(" Next run on"+ p.nextMatchingDate());
				return p.nextMatchingDate();
			}
		}

		return null;
	}

	@Override
	public List<Date> getNextScheduledTimes(int recordId, Date startdate, Date enddate) {
		final List<Date> dates = new ArrayList<Date>();
		if (records.containsKey(recordId)) {
			IScheduledItem schedItem = null;
			final String taskId = records.get(recordId);

			// get Task
			final Task task = scheduler.getTask(taskId);
			if ((task != null) && (task instanceof ScheduledCronTask)) {
				schedItem = ((ScheduledCronTask) task).getItem();
			}
			Date date = null;
			if (schedItem != null) {
				date = schedItem.getFixedDate();
				if (!schedItem.isPeriodic() && (date != null)) {
					// return date if defined and either no period is specified OR date occurs into the specified
					// period.

					if (date != null) {
						// No specified period, return date
						if (((startdate == null) && (enddate == null))
								|| (date.after(startdate) && date.before(enddate))) {
							dates.add(date);
						}
					}
				} else {
					final SchedulingPattern pattern = scheduler.getSchedulingPattern(taskId);
					// activator.info(" Scheduling Pattern = "+ pattern.toString());
					final Predictor p = new Predictor(pattern);
					// activator.info(" Next run on"+ p.nextMatchingDate());
					date = p.nextMatchingDate();
					while ((date != null) && date.before(enddate)) {
						if (date.after(startdate)) {
							dates.add(date);
							if (!schedItem.isPeriodic()) {
								break;
							}
						}
						date = p.nextMatchingDate();
					}
				}
			}
		}
		return dates;
	}

	@Override
	public BeanMapList listScheduledItems(String type) {
		final BeanMapList list = new BeanMapList();
		MetaDataEntity.loadEntity("scheduleditem");
		// get tasks
		final Collection<String> taskIds = records.values();
		for (final String taskId : taskIds) {
			final Task task = scheduler.getTask(taskId);
			if (task instanceof ScheduledCronTask) {
				final IScheduledItem item = ((ScheduledCronTask) task).getItem();
				if ((type == null) || item.getType().equals(type)) {
					final BeanMap bm = new BeanMap("scheduleditem", item.getId());
					bm.put("type", item.getType());
					bm.put("name", item.getName());
					list.add(bm);
				}
			}
		}
		return list;
	}

	/**
	 * List all scheduled items in future, within a given period Note: scheduler does not keep past items.
	 *
	 * @param type
	 * @param start
	 * @param end
	 */
	@Override
	public BeanMapList listScheduledItemsInPeriod(String type, Date start, Date end) {
		final BeanMapList list = new BeanMapList();
		// if no specified period, get all : passed and future
		// get tasks
		final Collection<String> taskIds = records.values();
		for (final String taskId : taskIds) {
			final Task task = scheduler.getTask(taskId);
			if (task instanceof ScheduledCronTask) {
				final IScheduledItem item = ((ScheduledCronTask) task).getItem();
				if ((type == null) || item.getType().equals(type)) {
					// System.out.println("Item::"+item.getClass().getName() + " - "+item.getId());
					final List<Date> dates = getNextScheduledTimes(item.getId(), start, end);
					for (final Date date : dates) {
						final BeanMap bm = new BeanMap(IScheduledItem.ENTITY_SCHEDULEDITEM, item.getId());
						bm.put(IScheduledItem.SCHEDULEDITEM_ATTR_TYPE, item.getType());
						bm.put(IScheduledItem.SCHEDULEDITEM_ATTR_NAME, item.getName());
						bm.put(IScheduledItem.SCHEDULEDITEM_ATTR_OCCURRENCE, date);
						bm.put(IScheduledItem.SCHEDULEDITEM_ATTR_OCCURRENCE_TIME, date.getTime());

						// System.out.println("** Date::"+date);
						final MapperSQLService sqlService = getMapperService();
						if (sqlService != null) {
							final BeanMap instance = sqlService.selection(item.getType(), item.getId(), null,
									false);
							bm.put(IScheduledItem.SCHEDULEDITEM_ATTR_INSTANCE, instance);
						}

						list.add(bm);
					}
				}
			}
		}
		return list;
	}

	private MapperSQLService getMapperService() {
		final Object service = activator.getService(IMapperService.clazz);
		if (service instanceof MapperSQLService) {
			return (MapperSQLService) service;
		}
		return null;
	}

	@Override
	public List<IScheduledItem> getScheduledItems() {
		final ArrayList<IScheduledItem> result = new ArrayList<IScheduledItem>();
		// get tasks
		final Collection<String> taskIds = records.values();
		for (final String taskId : taskIds) {
			final Task task = scheduler.getTask(taskId);
			if (task instanceof ScheduledCronTask) {
				final IScheduledItem item = ((ScheduledCronTask) task).getItem();
				result.add(item);
			}
		}
		return result;
	}
}
