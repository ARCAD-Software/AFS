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
package com.arcadsoftware.afs.client.core.tools;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import com.arcadsoftware.afs.client.core.internal.Resources;

public class DateUtils {

	public static final String INTERNAL_FROM_FORMAT = "yyyyMMdd-HH:mm:ss";
	public static final String INTERNAL_DURATION_FORMAT_WITHMS = "%1$01dd %2$02dh %3$02dmin %4$02ds %5$03dms";
	public static final String INTERNAL_DURATION_FORMAT = "%1$01dd %2$02dh %3$02dmin %4$02ds";
	public static final SimpleDateFormat DATETIME_FORMAT = new SimpleDateFormat(
			Resources.resString("formatter.date.datetime"));
	public static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat(Resources.resString("formatter.date.date"));
	private static final int ONE_DAY = 1000 * 60 * 60 * 24;
	private static final int ONE_HOUR = ONE_DAY / 24;
	private static final int ONE_MINUTE = ONE_HOUR / 60;
	private static final int ONE_SECOND = ONE_MINUTE / 60;

	public enum TimeField {
		DAY,
		HOUR,
		MINUTE,
		SECOND,
		MILLISECOND;
	}

	public static String convertDate(String fromFormat, String toFormat, String value) {
		if (value != null) {
			final SimpleDateFormat from = new SimpleDateFormat(fromFormat);
			final SimpleDateFormat to = new SimpleDateFormat(toFormat);
			try {
				final Date d = from.parse(value);
				final String result = to.format(d);
				return result;
			} catch (final ParseException e) {
			}
		}
		return null;
	}

	public synchronized static String formatDate(Date d) {
		if (d == null) {
			return "";
		}
		return DATETIME_FORMAT.format(d);
	}

	public synchronized static String formatDateSimple(Date d) {
		if (d == null) {
			return "";
		}
		return DATE_FORMAT.format(d);
	}

	public static String convertDate(String toFormat, String value) {
		return convertDate(INTERNAL_FROM_FORMAT, toFormat, value);
	}

	public static String getTimeDifferenceAsString(Date d1, Date d2, boolean displayMilliseconds) {
		final long[] diffs = getTimeDifference(d1, d2);
		if (displayMilliseconds) {
			return String.format(INTERNAL_DURATION_FORMAT_WITHMS, diffs[0], diffs[1], diffs[2], diffs[3], diffs[4]);
		}
		return String.format(INTERNAL_DURATION_FORMAT, diffs[0], diffs[1], diffs[2], diffs[3]);
	}

	/**
	 * Calculate the absolute difference between two Date without regard for time offsets
	 *
	 * @param d1
	 *            Date one
	 * @param d2
	 *            Date two
	 * @param field
	 *            The field we're interested in out of day, hour, minute, second, millisecond
	 * @return The value of the required field
	 */
	public static long getTimeDifference(Date d1, Date d2, TimeField field) {
		return getTimeDifference(d1, d2)[field.ordinal()];
	}

	/**
	 * Calculate the absolute difference between two Date without regard for time offsets
	 *
	 * @param d1
	 *            Date one
	 * @param d2
	 *            Date two
	 * @return The fields day, hour, minute, second and millisecond
	 */
	public static long getTimeDifferenceAsLong(Date d1, Date d2) {
		final Calendar cal = Calendar.getInstance();
		cal.setTimeZone(TimeZone.getTimeZone("UTC"));
		cal.setTime(d1);
		final long t1 = cal.getTimeInMillis();
		cal.setTime(d2);
		return Math.abs(cal.getTimeInMillis() - t1);
	}

	/**
	 * Calculate the absolute difference between two Date without regard for time offsets
	 *
	 * @param d1
	 *            Date one
	 * @param d2
	 *            Date two
	 * @return The fields day, hour, minute, second and millisecond
	 */
	public static long[] getTimeDifference(Date d1, Date d2) {
		final Calendar cal = Calendar.getInstance();
		cal.setTimeZone(TimeZone.getTimeZone("UTC"));
		cal.setTime(d1);
		final long t1 = cal.getTimeInMillis();
		cal.setTime(d2);
		final long diff = Math.abs(cal.getTimeInMillis() - t1);
		return getDuration(diff);
	}

	public static long[] getDuration(long diff) {
		final long[] result = new long[5];
		final long d = diff / ONE_DAY;
		diff %= ONE_DAY;
		final long h = diff / ONE_HOUR;
		diff %= ONE_HOUR;
		final long m = diff / ONE_MINUTE;
		diff %= ONE_MINUTE;
		final long s = diff / ONE_SECOND;
		final long ms = diff % ONE_SECOND;
		result[0] = d;
		result[1] = h;
		result[2] = m;
		result[3] = s;
		result[4] = ms;
		return result;
	}

	/**
	 * Calculate the absolute difference between two Calendar without regard for time offsets
	 *
	 * @param d1
	 *            Calendar one
	 * @param d2
	 *            Calendar two
	 * @return The fields day, hour, minute, second and millisecond
	 */
	public static long getDuration(long millisecond, int field) {
		final long[] result = getDuration(millisecond);
		switch (field) {
		case Calendar.DAY_OF_YEAR:
			return result[0];
		case Calendar.HOUR:
		case Calendar.HOUR_OF_DAY:
			return (result[0] * 24) + result[1];
		case Calendar.MINUTE:
			return (((result[0] * 24) + result[1]) * 60) + result[2];
		case Calendar.SECOND:
			return (((((result[0] * 24) + result[1]) * 60) + result[2]) * 60) + result[3];
		case Calendar.MILLISECOND:
			return (((((((result[0] * 24) + result[1]) * 60) + result[2]) * 60) + result[3]) * 1000) + result[4];
		default:
			return -1;
		}
	}

	/**
	 * Calculate the absolute difference between two Calendar without regard for time offsets
	 *
	 * @param d1
	 *            Calendar one
	 * @param d2
	 *            Calendar two
	 * @return The fields day, hour, minute, second and millisecond
	 */
	public static long[] getTimeDifference(Calendar d1, Calendar d2) {
		final long t1 = d1.getTimeInMillis();
		final long t2 = d2.getTimeInMillis();
		final long diff = Math.abs(t2 - t1);
		return getDuration(diff);
	}

	/**
	 * Calculate the absolute difference between two Calendar without regard for time offsets
	 *
	 * @param d1
	 *            Calendar one
	 * @param d2
	 *            Calendar two
	 * @return The fields day, hour, minute, second and millisecond
	 */
	public static long getTimeDifference(Calendar d1, Calendar d2, int field) {
		final long[] result = getTimeDifference(d1, d2);
		switch (field) {
		case Calendar.DAY_OF_YEAR:
			return result[0];
		case Calendar.HOUR:
		case Calendar.HOUR_OF_DAY:
			return (result[0] * 24) + result[1];
		case Calendar.MINUTE:
			return (((result[0] * 24) + result[1]) * 60) + result[2];
		case Calendar.SECOND:
			return (((((result[0] * 24) + result[1]) * 60) + result[2]) * 60) + result[3];
		case Calendar.MILLISECOND:
			return (((((((result[0] * 24) + result[1]) * 60) + result[2]) * 60) + result[3]) * 1000) + result[4];
		default:
			return -1;
		}
	}

	public static Date addToDate(Date d, long[] duration) {
		if (duration.length != 5) {
			return d;
		}
		final Calendar cDate = Calendar.getInstance();
		cDate.setTime(d);
		cDate.add(Calendar.DAY_OF_MONTH, (int) duration[0]);
		cDate.add(Calendar.HOUR, (int) duration[1]);
		cDate.add(Calendar.MINUTE, (int) duration[2]);
		cDate.add(Calendar.SECOND, (int) duration[3]);
		cDate.add(Calendar.MILLISECOND, (int) duration[4]);
		return cDate.getTime();
	}

	public static Date add(Date d, int amount, int field) {
		final Calendar cDate = Calendar.getInstance();
		cDate.setTime(d);
		cDate.add(field, amount);
		return cDate.getTime();
	}

	public static Calendar toCalendar(Date d) {
		final Calendar result = Calendar.getInstance();
		result.setTime(d);
		return result;
	}

	public static Calendar toCalendar(int year, int month, int day) {
		final Calendar result = Calendar.getInstance();
		result.set(Calendar.YEAR, year);
		result.set(Calendar.MONTH, month);
		result.set(Calendar.DAY_OF_MONTH, day);
		return result;
	}

	public static Date toDate(Date d, int hour, int minute) {
		final Calendar cal = DateUtils.toCalendar(d);
		cal.set(Calendar.HOUR_OF_DAY, hour);
		cal.set(Calendar.MINUTE, minute);
		return cal.getTime();
	}

	public static Date newDate(int year, int month, int day, int hour, int minute) {
		final Calendar cal = DateUtils.toCalendar(year, month, day);
		cal.set(Calendar.HOUR_OF_DAY, hour);
		cal.set(Calendar.MINUTE, minute);
		return cal.getTime();
	}

}
