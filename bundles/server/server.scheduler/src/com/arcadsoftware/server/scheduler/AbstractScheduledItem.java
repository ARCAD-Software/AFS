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

import java.util.Calendar;
import java.util.Date;

/**
 * 
 */
public abstract class AbstractScheduledItem implements IScheduledItem {

	public static final int MASK_MONDAY = 1;
	public static final int MASK_TUESDAY = 2;
	public static final int MASK_WEDNESDAY = 4;
	public static final int MASK_THURSDAY = 8;
	public static final int MASK_FRIDAY = 16;
	public static final int MASK_SATURDAY = 32;
	public static final int MASK_SUNDAY = 64;
	private static final Object[][] daysDefinition = new Object[][] { 
			{ MASK_SUNDAY, 0 }, 
			{ MASK_MONDAY, 1 },
			{ MASK_TUESDAY, 2 }, 
			{ MASK_WEDNESDAY, 3 }, 
			{ MASK_THURSDAY, 4 }, 
			{ MASK_FRIDAY, 5 },
			{ MASK_SATURDAY, 6 }
		};
	private static String CRON_FORMAT = "%s %s %s %s %s";
	private static String CRON_ALL = "*";
	private static String CRON_PERIOD = "*/%d";

	public static final Object[][] getDaysDefinitions() {
		return daysDefinition;
	}
	
	private int id;
	private String name;
	private boolean periodic;
	private int hours;
	private int minutes;
	private int days; // mask
	private String daysInMonth; // list of integers, separated by ,
	private int userId;
	private String dayOfWeekList = ""; // Already formatted string : Aternative to days that uses maskes :
	private Date fixedDate;

	public AbstractScheduledItem(int id, String name) {
		setId(id);
		setName(name);
	}

	@Override
	public int getId() {
		return id;
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public boolean isPeriodic() {
		return periodic;
	}

	@Override
	public int getHours() {
		return hours;
	}

	@Override
	public int getMinutes() {
		return minutes;
	}

	@Override
	public int getDays() {
		return days;
	}

	@Override
	public void setId(int id) {
		this.id = id;
	}

	@Override
	public void setName(String name) {
		this.name = name;
	}

	@Override
	public void setPeriodic(boolean periodic) {
		this.periodic = periodic;
	}

	@Override
	public void setHours(int hours) {
		this.hours = hours;
		if (fixedDate != null) {
			Calendar cal = Calendar.getInstance();
			cal.setTime(fixedDate);
			cal.set(Calendar.HOUR_OF_DAY, hours);
			fixedDate.setTime(cal.getTimeInMillis());
		}
	}

	@Override
	public void setMinutes(int minutes) {
		this.minutes = minutes;
		if (fixedDate != null) {
			Calendar cal = Calendar.getInstance();
			cal.setTime(fixedDate);
			cal.set(Calendar.MINUTE, minutes);
			fixedDate.setTime(cal.getTimeInMillis());
		}
	}

	@Override
	public void setDays(int days) {
		this.days = days;
	}

	@Override
	public Date getFixedDate() {
		return fixedDate;
	}

	@Override
	public void setFixedDate(Date date) {
		this.fixedDate = date;
	}

	@Override
	public int getUser() {
		return userId;
	}

	@Override
	public void setUser(int userId) {
		this.userId = userId;
	}

	@Override
	public String getDaysInMonth() {
		return daysInMonth;
	}

	@Override
	public void setDaysInMonth(String daysInMonth) {
		this.daysInMonth = daysInMonth;
	}

	@Override
	public boolean isPassed() {
		return (fixedDate != null) && fixedDate.before(new Date());
	}

	@Override
	public String getFormattedTime() {
		// time fixed every x, y, z days : min hour * * x,y,z
		// every X hours: * */X * * *
		// * * * * * :: {min} {hoursofday} {daysinmonth} {month} {days in week}
		String format = null;
		Date fixedDate = getFixedDate();
		if (fixedDate != null) {
			Calendar cal = Calendar.getInstance();
			cal.setTime(fixedDate);
			format = String.format(CRON_FORMAT, cal.get(Calendar.MINUTE), cal.get(Calendar.HOUR_OF_DAY),
					cal.get(Calendar.DAY_OF_MONTH), (cal.get(Calendar.MONTH) + 1), CRON_ALL);
			// System.out.println("Format fixed date :: ");
		} else {
			int minutes = getMinutes();
			int hours = getHours();
			String strMinutes = CRON_ALL;
			String strHours = CRON_ALL;
			String monthDays = CRON_ALL;
			String months = CRON_ALL;
			String daysInWeek = CRON_ALL;
			// periodic : repeated every X minutes, or every Y hours
			if (isPeriodic()) {
				// minutes
				if (minutes > 0) {
					strMinutes = String.format(CRON_PERIOD, minutes);
				} else {
					strMinutes = CRON_ALL;
				}
				// hours
				if (hours > 0) {
					strHours = String.format(CRON_PERIOD, hours);
				} else {
					strHours = CRON_ALL;
				}
				monthDays = CRON_ALL;
			} else {
				monthDays = getDaysInMonth();
				if ((monthDays == null) || (monthDays.length() == 0)) {
					monthDays = CRON_ALL;
				}
				strMinutes = String.valueOf(minutes);
				strHours = String.valueOf(hours);
			}
			// days of week
			if (days > 0) {
				daysInWeek = "";
				if (days > 0) {
					for (Object[] def : daysDefinition) {
						if ((days & (Integer) (def[0])) > 0) {
							if (daysInWeek.length() > 0)
								daysInWeek += ",";
							daysInWeek += String.valueOf(def[1]);
						}
					}
				}
				if (daysInWeek.length() == 0)
					daysInWeek = CRON_ALL;
			} else if ((dayOfWeekList != null) && (dayOfWeekList.length() > 0)) {
				daysInWeek = dayOfWeekList;
			}
			format = String.format(CRON_FORMAT, strMinutes, strHours, monthDays, months, daysInWeek);
		}
		return format;
	}

	public String getDayOfWeekList() {
		return dayOfWeekList;
	}

	public void setDayOfWeekList(String dayOfWeekList) {
		this.dayOfWeekList = dayOfWeekList;
	}

	/**
	 * Check user gets enough rights
	 * <p>
	 * Default implementation always return true.
	 */
	@Override
	public boolean canExecute() {
		return true;
	}

	@Override
	public void prepareNext() {
	}

	@Override
	public void clean() {
	}

	@Override
	public abstract String getType();
}
