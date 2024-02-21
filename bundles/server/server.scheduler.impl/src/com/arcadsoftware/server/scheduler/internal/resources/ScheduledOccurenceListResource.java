/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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
package com.arcadsoftware.server.scheduler.internal.resources;

import java.util.Calendar;
import java.util.Date;

import org.restlet.data.Method;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.rest.BeanMapParentResource;
import com.arcadsoftware.server.scheduler.ISchedulerService;

public class ScheduledOccurenceListResource extends BeanMapParentResource {

	private final static String ENTITY_GEN_SCHEDULED_ITEM = "scheduleditem"; //$NON-NLS-1$
	private final static String ATTRIBUTE_TYPE = "type"; //$NON-NLS-1$
	private final static String ATTRIBUTE_STARTPERIOD = "startperiod"; //$NON-NLS-1$
	private final static String ATTRIBUTE_ENDPERIOD = "endperiod"; //$NON-NLS-1$

	@Override
	public boolean hasRight(Method method) {
		return Method.GET.equals(method);
	}

	@Override
	protected BeanMapList list(BeanMap parameters) throws ResourceException {
		ISchedulerService service = getOSGiService(ISchedulerService.class);
		if (service == null) {
			return new BeanMapList();
		}
		// list of scheduled items of that period
		final Date start_date = getStartPeriod();
		return service.listScheduledItemsInPeriod(getItemsType(), start_date, getEndPeriod(start_date));
	}

	@Override
	public String getType() {
		return ENTITY_GEN_SCHEDULED_ITEM;
	}
	
	@Override
	protected BeanMap post(BeanMap request) throws ResourceException {
		// abstract implementation, never called.
		return null;
	}
	
	private String getItemsType() {
		return getAttribute(ATTRIBUTE_TYPE);
	}
	
	private Date getStartPeriod() {
		String start = getAttribute(ATTRIBUTE_STARTPERIOD);
		if ((start == null) || start.equals("0")) { //$NON-NLS-1$
			return new Date(); // now
		}
		try {
			return new Date(Long.parseLong(start));
		} catch (NumberFormatException e) {
			return new Date(); // now ?
		}
	}
	
	/**
	 * get End period; if not specified, set if to startDate + 1 Month
	 * @return
	 */
	private Date getEndPeriod(Date startDate) {
		String end = getAttribute(ATTRIBUTE_ENDPERIOD);
		if ((end == null) || end.equals("0")) { //$NON-NLS-1$
			// return current time + 1 year
			Calendar cal = Calendar.getInstance();
			cal.setTime(startDate);
			cal.add(Calendar.YEAR, 1);
			return new Date(cal.getTimeInMillis());
		}
		try {
			return new Date(Long.parseLong(end));
		} catch (NumberFormatException e) {
			return new Date(); // now ?
		}
	}
}
