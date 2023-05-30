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
package com.arcadsoftware.metadata.criteria;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Check if the attribute is lower (before) the given date.
 */
public class BeforeCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria {

	private String attribute;
	private Date value;
	private boolean trunc;
	private int days;
	private int hours;
	private int months;
	private int years;
	private int minuts;
	
	public BeforeCriteria() {
		super();
	}

	public BeforeCriteria(String attribute) {
		super();
		this.attribute = attribute;
	}

	public BeforeCriteria(String attribute, boolean trunc) {
		super();
		this.attribute = attribute;
		this.trunc = trunc;
	}

	public BeforeCriteria(String attribute, Date value, boolean trunc) {
		super();
		this.attribute = attribute;
		this.value = value;
		this.trunc = trunc;
	}

	public BeforeCriteria(String attribute, Date value, boolean trunc, int years, int months, int days, int hours,
			int minuts) {
		super();
		this.attribute = attribute;
		this.value = value;
		this.trunc = trunc;
		this.years = years;
		this.months = months;
		this.days = days;
		this.hours = hours;
		this.minuts = minuts;
	}

	public BeforeCriteria(String attribute, Date value) {
		super();
		this.attribute = attribute;
		this.value = value;
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		ReferenceLine attributeRef = context.getEntity().getAttributeLine(attribute);
		if (attributeRef != null) {
			context.useReference(attributeRef);
			return this; // Objet non modifié
			//return new BeforeCriteria(attribute,value,trunc,years,months,days,hours,minuts);
		}
		return ConstantCriteria.FALSE;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new BeforeCriteria(attribute,value,trunc,years,months,days,hours,minuts);
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof BeforeCriteria) &&
		nullsOrEquals(attribute,((BeforeCriteria)obj).attribute) &&
		nullsOrEquals(value,((BeforeCriteria)obj).value) &&
		(trunc == ((BeforeCriteria)obj).trunc) &&
		(minuts == ((BeforeCriteria)obj).minuts) &&
		(hours == ((BeforeCriteria)obj).hours) &&
		(days == ((BeforeCriteria)obj).days) &&
		(months == ((BeforeCriteria)obj).months) &&
		(years == ((BeforeCriteria)obj).years);
	}

	public Calendar getCalendar() {
		Calendar calendar = new GregorianCalendar();
		if (value != null) {
			calendar.setTime(value);
		}
		calendar.add(Calendar.MINUTE, minuts);
		calendar.add(Calendar.HOUR_OF_DAY, hours);
		calendar.add(Calendar.DAY_OF_YEAR, days);
		calendar.add(Calendar.MONTH, months);
		calendar.add(Calendar.YEAR, years);
		if (trunc) {
			calendar.set(Calendar.MILLISECOND, 999);
			calendar.set(Calendar.SECOND, 59);
			calendar.set(Calendar.MINUTE, 59);
			calendar.set(Calendar.HOUR_OF_DAY, 23);
		}
		return calendar;
	}
	
	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		Date date = bean.get(attribute, Date.class);
		if (date == null) {
			return false;
		}
		return date.before(getCalendar().getTime());
	}

	public String getAttribute() {
		return attribute;
	}

	public Date getValue() {
		return value;
	}

	public boolean isTrunc() {
		return trunc;
	}

	public int getDays() {
		return days;
	}

	public int getHours() {
		return hours;
	}

	public int getMonths() {
		return months;
	}

	public int getYears() {
		return years;
	}

	public int getMinuts() {
		return minuts;
	}

	public void setAttribute(String code) {
		attribute = code;
	}
	
	public void setValue(Date value) {
		this.value = value;
	}

	public void setTrunc(boolean trunc) {
		this.trunc = trunc;
	}

	public void setDays(int days) {
		this.days = days;
	}

	public void setHours(int hours) {
		this.hours = hours;
	}

	public void setMonths(int months) {
		this.months = months;
	}

	public void setYears(int years) {
		this.years = years;
	}

	public void setMinuts(int minuts) {
		this.minuts = minuts;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(attribute);
		sb.append(" << "); //$NON-NLS-1$
		if (value == null) {
			if (trunc) {
				sb.append(Messages.Criteria_CurrentDate);
			} else {
				sb.append(Messages.Criteria_CurrentTime);
			}
			if (years != 0) {
				if (years > 0) {
					sb.append('+');
				}
				sb.append(years);
				sb.append(Messages.Criteria_Year);
			}
			if (months != 0) {
				if (months > 0) {
					sb.append('+');
				}
				sb.append(months);
				sb.append(Messages.Criteria_Month); 
			}
			if (days != 0) {
				if (days > 0) {
					sb.append('+');
				}
				sb.append(days);
				sb.append(Messages.Criteria_Day);
			}
			if (hours != 0) {
				if (hours > 0) {
					sb.append('+');
				}
				sb.append(hours);
				sb.append(Messages.Criteria_Hour);
			}
			if (minuts != 0) {
				if (minuts > 0) {
					sb.append('+');
				}
				sb.append(minuts);
				sb.append(Messages.Criteria_Minute);
			}
		} else if (trunc) {
			sb.append(String.format(Messages.Criteria_DateFormat, getCalendar().getTime()));
		} else {
			sb.append(String.format(Messages.Criteria_DateTimeFormat, getCalendar().getTime()));
		}
		return sb.toString();
	}
}
