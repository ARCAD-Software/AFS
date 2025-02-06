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
package com.arcadsoftware.metadata.criteria;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Compare the last update of the data to two given dates.
 *   
 * TODO Add support for referenced entity (attribut parameter).
 *
 */
public class ChangedCriteria extends AbstractSearchCriteria implements IAttributeCriteria {

	private String attribute;
	private Date after;
	private Date before;
	private boolean trunc;
	private int afteryears;
	private int aftermonths;
	private int afterdays;
	private int afterhours;
	private int afterminuts;
	private int beforeyears;
	private int beforemonths;
	private int beforedays;
	private int beforehours;
	private int beforeminuts;
	
	public ChangedCriteria() {
		super();
	}

	public ChangedCriteria(Date after, Date before, boolean trunc) {
		this();
		this.after = after;
		this.before = before;
		this.trunc = trunc;
	}

	public ChangedCriteria(Date after, Date before, boolean trunc, int afteryears, int aftermonths,
			int afterdays, int afterhours, int afterminuts, int beforeyears, int beforemonths, int beforedays,
			int beforehours, int beforeminuts) {
		this();
		this.after = after;
		this.before = before;
		this.trunc = trunc;
		this.afteryears = afteryears;
		this.aftermonths = aftermonths;
		this.afterdays = afterdays;
		this.afterhours = afterhours;
		this.afterminuts = afterminuts;
		this.beforeyears = beforeyears;
		this.beforemonths = beforemonths;
		this.beforedays = beforedays;
		this.beforehours = beforehours;
		this.beforeminuts = beforeminuts;
	}

	public ChangedCriteria(String attribute, Date after, Date before, boolean trunc, int afteryears, int aftermonths,
			int afterdays, int afterhours, int afterminuts, int beforeyears, int beforemonths, int beforedays,
			int beforehours, int beforeminuts) {
		this(after, before, trunc, afteryears, aftermonths, afterdays, afterhours, afterminuts, beforeyears, beforemonths, beforedays, beforehours, beforeminuts);
		this.attribute = attribute;
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		MetaDataEntity e = context.getEntity();
		ReferenceLine rl = null;
		if ((attribute != null) && !attribute.isEmpty()) {
			rl = e.getAttributeLine(attribute);
			if ((rl == null) || (rl.getLastEntity() == null)) {
				return ConstantCriteria.FALSE;
			}
			e = rl.getLastEntity();
		}
		if (e.isReadOnly() || //
				!e.getMetadata().getBoolean(MetaDataEntity.METADATA_UPDATABLE) || // 
				(e.getMetadata().get("updateCol") == null)) { //$NON-NLS-1$
			return ConstantCriteria.FALSE;
		}
		if (rl != null) {
			context.useReference(rl);
		}
		return this; // objet non modifié...
	}

	public Calendar getAfterCalendar() {
		Calendar calendar = new GregorianCalendar();
		if (after != null) {
			calendar.setTime(after);
		}
		calendar.add(Calendar.MINUTE, afterminuts);
		calendar.add(Calendar.HOUR, afterhours);
		calendar.add(Calendar.DAY_OF_YEAR, afterdays);
		calendar.add(Calendar.MONTH, aftermonths);
		calendar.add(Calendar.YEAR, afteryears);
		if (trunc) {
			calendar.set(Calendar.MILLISECOND, 999);
			calendar.set(Calendar.SECOND, 59);
			calendar.set(Calendar.MINUTE, 59);
			calendar.set(Calendar.HOUR, 23);
		}
		return calendar;
	}
	
	public Calendar getBeforeCalendar() {
		Calendar calendar = new GregorianCalendar();
		if (before != null) {
			calendar.setTime(before);
		}
		calendar.add(Calendar.MINUTE, beforeminuts);
		calendar.add(Calendar.HOUR, beforehours);
		calendar.add(Calendar.DAY_OF_YEAR, beforedays);
		calendar.add(Calendar.MONTH, beforemonths);
		calendar.add(Calendar.YEAR, beforeyears);
		if (trunc) {
			calendar.set(Calendar.MILLISECOND, 999);
			calendar.set(Calendar.SECOND, 59);
			calendar.set(Calendar.MINUTE, 59);
			calendar.set(Calendar.HOUR, 23);
		}
		return calendar;
	}
	
	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		Date date = bean.getDate();
		if ((attribute != null) && !attribute.isEmpty()) {
			BeanMap b = bean.getBeanMap(attribute);
			if (b == null) {
				return false;
			}
			date = b.getDate();
		}
		if (date == null) {
			return true;
		}
		return date.before(getBeforeCalendar().getTime()) && date.after(getAfterCalendar().getTime());
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new ChangedCriteria(attribute, after, before, trunc, afteryears, aftermonths, afterdays, 
				afterhours, afterminuts, beforeyears, beforemonths, beforedays, beforehours, beforeminuts);
	}

	
	public boolean equals(Object obj) {
		// TODO Test truncated dates only !
		return (obj instanceof ChangedCriteria) &&
				nullsOrEquals(attribute,((ChangedCriteria)obj).attribute) &&
				nullsOrEquals(before,((ChangedCriteria)obj).before) &&
				nullsOrEquals(after,((ChangedCriteria)obj).after) &&
				(trunc == ((ChangedCriteria)obj).trunc) &&
				(afterminuts == ((ChangedCriteria)obj).afterminuts) &&
				(afterhours == ((ChangedCriteria)obj).afterhours) &&
				(afterdays == ((ChangedCriteria)obj).afterdays) &&
				(aftermonths == ((ChangedCriteria)obj).aftermonths) &&
				(afteryears == ((ChangedCriteria)obj).afteryears) &&
				(beforeminuts == ((ChangedCriteria)obj).beforeminuts) &&
				(beforehours == ((ChangedCriteria)obj).beforehours) &&
				(beforedays == ((ChangedCriteria)obj).beforedays) &&
				(beforemonths == ((ChangedCriteria)obj).beforemonths) &&
				(beforeyears == ((ChangedCriteria)obj).beforeyears);
	}

	public Date getAfter() {
		return after;
	}

	public Date getBefore() {
		return before;
	}

	public boolean isTrunc() {
		return trunc;
	}

	public int getAfteryears() {
		return afteryears;
	}

	public int getAftermonths() {
		return aftermonths;
	}

	public int getAfterdays() {
		return afterdays;
	}

	public int getAfterhours() {
		return afterhours;
	}

	public int getAfterminuts() {
		return afterminuts;
	}

	public int getBeforeyears() {
		return beforeyears;
	}

	public int getBeforemonths() {
		return beforemonths;
	}

	public int getBeforedays() {
		return beforedays;
	}

	public int getBeforehours() {
		return beforehours;
	}

	public int getBeforeminuts() {
		return beforeminuts;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if ((attribute != null) && !attribute.isEmpty()) {
			sb.append(attribute);
			sb.append(" has "); //$NON-NLS-1$
		}
		sb.append("changed between ["); //$NON-NLS-1$
		if (after == null) {
			if (trunc) {
				sb.append(Messages.Criteria_CurrentDate);
			} else {
				sb.append(Messages.Criteria_CurrentTime);
			}
			if (afteryears != 0) {
				if (afteryears > 0) {
					sb.append('+');
				}
				sb.append(afteryears);
				sb.append(Messages.Criteria_Year);
			}
			if (aftermonths != 0) {
				if (aftermonths > 0) {
					sb.append('+');
				}
				sb.append(aftermonths);
				sb.append(Messages.Criteria_Month); 
			}
			if (afterdays != 0) {
				if (afterdays > 0) {
					sb.append('+');
				}
				sb.append(afterdays);
				sb.append(Messages.Criteria_Day);
			}
			if (afterhours != 0) {
				if (afterhours > 0) {
					sb.append('+');
				}
				sb.append(afterhours);
				sb.append(Messages.Criteria_Hour);
			}
			if (afterminuts != 0) {
				if (afterminuts > 0) {
					sb.append('+');
				}
				sb.append(afterminuts);
				sb.append(Messages.Criteria_Minute);
			}
		} else if (trunc) {
			sb.append(String.format(Messages.Criteria_DateFormat, getAfterCalendar().getTime()));
		} else {
			sb.append(String.format(Messages.Criteria_DateTimeFormat, getAfterCalendar().getTime()));
		}
		sb.append(',');
		if (before == null) {
			if (trunc) {
				sb.append(Messages.Criteria_CurrentDate);
			} else {
				sb.append(Messages.Criteria_CurrentTime);
			}
			if (beforeyears != 0) {
				if (beforeyears > 0) {
					sb.append('+');
				}
				sb.append(beforeyears);
				sb.append(Messages.Criteria_Year);
			}
			if (beforemonths != 0) {
				if (beforemonths > 0) {
					sb.append('+');
				}
				sb.append(beforemonths);
				sb.append(Messages.Criteria_Month); 
			}
			if (beforedays != 0) {
				if (beforedays > 0) {
					sb.append('+');
				}
				sb.append(beforedays);
				sb.append(Messages.Criteria_Day);
			}
			if (beforehours != 0) {
				if (beforehours > 0) {
					sb.append('+');
				}
				sb.append(beforehours);
				sb.append(Messages.Criteria_Hour);
			}
			if (beforeminuts != 0) {
				if (beforeminuts > 0) {
					sb.append('+');
				}
				sb.append(beforeminuts);
				sb.append(Messages.Criteria_Minute);
			}
		} else if (trunc) {
			sb.append(String.format(Messages.Criteria_DateFormat, getBeforeCalendar().getTime()));
		} else {
			sb.append(String.format(Messages.Criteria_DateTimeFormat, getBeforeCalendar().getTime()));
		}
		sb.append(']');
		return sb.toString();
	}

	public void setAfter(Date after) {
		this.after = after;
	}

	public void setBefore(Date before) {
		this.before = before;
	}

	public void setTrunc(boolean trunc) {
		this.trunc = trunc;
	}

	public void setAfteryears(int afteryears) {
		this.afteryears = afteryears;
	}

	public void setAftermonths(int aftermonths) {
		this.aftermonths = aftermonths;
	}

	public void setAfterdays(int afterdays) {
		this.afterdays = afterdays;
	}

	public void setAfterhours(int afterhours) {
		this.afterhours = afterhours;
	}

	public void setAfterminuts(int afterminuts) {
		this.afterminuts = afterminuts;
	}

	public void setBeforeyears(int beforeyears) {
		this.beforeyears = beforeyears;
	}

	public void setBeforemonths(int beforemonths) {
		this.beforemonths = beforemonths;
	}

	public void setBeforedays(int beforedays) {
		this.beforedays = beforedays;
	}

	public void setBeforehours(int beforehours) {
		this.beforehours = beforehours;
	}

	public void setBeforeminuts(int beforeminuts) {
		this.beforeminuts = beforeminuts;
	}

	@Override
	public void setAttribute(String code) {
		attribute = code;
	}

	@Override
	public String getAttribute() {
		return attribute;
	}

}
