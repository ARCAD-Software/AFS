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
package com.arcadsoftware.server.scheduler.internal.task;

import java.util.Calendar;
import java.util.Date;

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;

import com.arcadsoftware.server.scheduler.IScheduledItem;
import com.arcadsoftware.server.scheduler.ISchedulerService;
import com.arcadsoftware.server.scheduler.internal.Activator;

import it.sauronsoftware.cron4j.Task;
import it.sauronsoftware.cron4j.TaskExecutionContext;

public class ScheduledCronTask extends Task {

	private final Activator activator;
	private final ISchedulerService service;
	private IScheduledItem record = null;

	public ScheduledCronTask(Activator activator, ISchedulerService service, IScheduledItem record) {
		super();
		this.service = service;
		this.activator = activator;
		this.record = record;
	}

	public IScheduledItem getItem() {
		return record;
	}

	public void setItem(IScheduledItem item) {
		record = item;
	}

	@Override
	public void execute(TaskExecutionContext arg0) throws RuntimeException {
		final Date fixedDate = record.getFixedDate();
		// if record is scheduled on Fixed date, check the year is the current one; if not, reschedule for next year
		// occurrence.
		if (fixedDate != null) {
			final Calendar cal = Calendar.getInstance();
			final int currentYear = cal.get(Calendar.YEAR);

			cal.setTime(fixedDate);
			final int fixedYear = cal.get(Calendar.YEAR);
			if (fixedYear > currentYear) {
				// reschedule for next year
				service.reset(record);
				activator.log(record.getName() + " - Reschedule for next year at " + service.getNextScheduledTime(record));
				return;
			}
		}
		if (!record.canExecute() || !record.execute()) {
			activator.log("SCHEDULED ITEM WAS NOT EXECUTED");
		}
		// unregister record if is was set for one-shot only
		if ((fixedDate != null) && !record.isPeriodic()) {
				service.unregister(record);
				activator.info(record.getName() + " - One Shot task - Unregisted from Scheduling Service");
			record.clean();
		} else {
			// prepare the next execution
			record.prepareNext();
		}
		// log next
		final BundleContext context = activator.getContext();
		final ServiceReference<ISchedulerService> serviceReference = context
				.getServiceReference(ISchedulerService.class);
		if (serviceReference != null) {
			final ISchedulerService service = context.getService(serviceReference);
			if (service != null) {
				activator
						.info("\n" + record.getName() + " - Next execution at " + service.getNextScheduledTime(record));
			}
		}
	}
}
