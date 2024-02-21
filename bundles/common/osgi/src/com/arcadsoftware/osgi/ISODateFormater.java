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
package com.arcadsoftware.osgi;

import java.text.ParseException;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import com.arcadsoftware.osgi.internal.Messages;

/**
 * A helper static class to convert a Date to and from String in the ISO 8601 format, with the unique format:
 * 
 * <p>
 * <code>yyyy-mm-ddThh:mn:ss,msZ</code>
 * 
 * <p>
 * With international decimal representation according to ISO 8601:1-2019 section 3.1.3.9 Decimal sign.
 */
public final class ISODateFormater {

	public static final Date EPOCH = new Date(0);
	
	/**
	 * Test the string without parsing it to state if it may represent an ISO formated date. 
	 * 
	 * If this method return <code>false</code> then the sting is NOT an ISO Date.
	 * If it return <code>true</code> then the string may still NOT represent an ISO Date.
	 * 
	 * @param string an ISO date string formated.
	 * @return true if the string may be a ISO Date.
	 */
	static public boolean mayIsoDate(String string) {
		return (string != null) && //
				(string.length() >= 19) && //
				(string.length() < 31) && //
				(string.charAt(4) == '-') && //
				(string.charAt(10) == 'T') && //
				(string.charAt(16) == ':');
	}

	/**
	 * Convert a string representing an ISO Date format to a Calendar Object
	 * 
	 * @param string
	 * @return a GregorianCalendar representing the date in the GMT timezone.
	 * @throws ParseException If the string can not be parsed as an ISO Date Format.
	 */
	static public Calendar toCalendar(String string) throws ParseException {
		if (string == null) {
			return null;
		}
		if (string.length() < 19) {
			throw new ParseException(Messages.getString("ISODateFormater.InvalidLength"),string.length()); //$NON-NLS-1$
		}
		Calendar result = new GregorianCalendar(TimeZone.getTimeZone("GMT")); //$NON-NLS-1$
		result.set(Calendar.MILLISECOND, 0);
		int i = 0;
		try {
			result.set(Calendar.YEAR, Integer.parseInt(string.substring(0,4)));
			i = 5;
			result.set(Calendar.MONTH, Integer.parseInt(string.substring(5,7)) - 1);
			i = 8;
			result.set(Calendar.DAY_OF_MONTH, Integer.parseInt(string.substring(8,10)));
			i = 11;
			result.set(Calendar.HOUR_OF_DAY, Integer.parseInt(string.substring(11,13)));
			i = 14;
			result.set(Calendar.MINUTE, Integer.parseInt(string.substring(14,16)));
			i = 17;
			result.set(Calendar.SECOND, Integer.parseInt(string.substring(17,19)));
			if ((string.length() > 20) && (string.charAt(19) == ',')) {
				i = 20;
				result.set(Calendar.MILLISECOND, Integer.parseInt(string.substring(20,24)));
			}
		} catch (NumberFormatException e) {
			throw new ParseException(e.getMessage(), i);
		} catch (IndexOutOfBoundsException e) {
			throw new ParseException(e.getMessage(), i);
		}
		return result;
	}
	
	/**
	 * Convert a string representing an ISO Date format to a Date Object
	 * 
	 * @param string
	 * @return a Date.
	 * @throws ParseException If the string can not be parsed as an ISO Date Format.
	 */
	static public Date toDate(String string) throws ParseException {
		if (string == null) {
			return null;
		}
		Calendar cal = toCalendar(string);
		if (cal == null) {
			return null;
		}
		return cal.getTime();
	}

	/**
	 * Convert the Date to a GMT, ISO date formated string.
	 * 
	 * @param date
	 * @return
	 */
	static public String toString(Date date) {
		Calendar cal = new GregorianCalendar(TimeZone.getTimeZone("GMT")); //$NON-NLS-1$
		cal.setTime(date);
		return toString(cal);
	}

	/**
	 * Convert the Date to a GMT, ISO date formated string.
	 * 
	 * @param date
	 * @return
	 */
	static public String toString(long date) {
		Calendar cal = new GregorianCalendar(TimeZone.getTimeZone("GMT")); //$NON-NLS-1$
		cal.setTimeInMillis(date);
		return toString(cal);
	}

	/**
	 * Convert the Date to a GMT, ISO date formated string.
	 * 
	 * <p>
	 * Exclude milli-seconds.
	 * 
	 * @param date
	 * @return
	 */
	static public String toStringSec(Date date) {
		Calendar cal = new GregorianCalendar(TimeZone.getTimeZone("GMT")); //$NON-NLS-1$
		cal.setTime(date);
		return toStringSec(cal);
	}

	/**
	 * Convert the Date to a GMT, ISO date formated string.
	 * 
	 * <p>
	 * Exclude milli-seconds.
	 * 
	 * @param date
	 * @return
	 */
	static public String toStringSec(long date) {
		Calendar cal = new GregorianCalendar(TimeZone.getTimeZone("GMT")); //$NON-NLS-1$
		cal.setTimeInMillis(date);
		return toStringSec(cal);
	}
	
	/**
	 * Convert the Calendar to a GMT, ISO date formated string.
	 *  
	 * @param calendar
	 * @return
	 */
	static public String toString(Calendar calendar) {
		calendar.set(Calendar.ZONE_OFFSET, 0);
		return String.format("%04d-%02d-%02dT%02d:%02d:%02d,%04dZ", //$NON-NLS-1$
				calendar.get(Calendar.YEAR),
				calendar.get(Calendar.MONTH) + 1,
				calendar.get(Calendar.DAY_OF_MONTH),
				calendar.get(Calendar.HOUR_OF_DAY),
				calendar.get(Calendar.MINUTE),
				calendar.get(Calendar.SECOND),
				calendar.get(Calendar.MILLISECOND));
	}
	
	/**
	 * Convert the Calendar to a GMT, ISO date formated string.
	 * 
	 * <p>
	 * Exclude milli-seconds.
	 *  
	 * @param calendar
	 * @return
	 */
	static public String toStringSec(Calendar calendar) {
		calendar.set(Calendar.ZONE_OFFSET, 0);
		return String.format("%04d-%02d-%02dT%02d:%02d:%02dZ", //$NON-NLS-1$
				calendar.get(Calendar.YEAR),
				calendar.get(Calendar.MONTH) + 1,
				calendar.get(Calendar.DAY_OF_MONTH),
				calendar.get(Calendar.HOUR_OF_DAY),
				calendar.get(Calendar.MINUTE),
				calendar.get(Calendar.SECOND));
	}
	
}