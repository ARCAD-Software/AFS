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
package com.arcadsoftware.metadata.internal;

import com.arcadsoftware.metadata.criteria.natural.Token;

public class DateUnitList {
	
	private int incrementYear;
	private int incrementMonth;
	private int incrementDay;
	private int incrementHour;
	private int incrementMinute;
	
	public DateUnitList() {
		super();
	}
	
	public void add(Token increment_unit) {
		for (int i = 0; i < increment_unit.image.length(); i++) {
			char c = increment_unit.image.charAt(i);
			if (((c < '0') || (c > '9')) && (c != '+') && (c != '-')) {
				add(increment_unit.image.substring(0, i), increment_unit.image.substring(i));
				return;
			}
		}
	}

	public void add(Token increment, Token unit) {
		add(increment.image, unit.image);
	}

	public void add(String increment, String unit) {
		if (!increment.isEmpty() && !unit.isEmpty()) {
			String ul = unit.toLowerCase();
			if (ul.startsWith("y")) {
				incrementYear += convert(increment);
			} else if (unit.equals("M") || ul.equals("mn") || ul.equals("mns") || ul.startsWith("min")) {
				incrementMinute += convert(increment);
			} else if (ul.startsWith("m")) {
				incrementMonth += convert(increment);
			} else if (ul.startsWith("d")) {
				incrementDay += convert(increment);
			} else if (ul.startsWith("h")) {
				incrementHour += convert(increment);
			}
		}
	}

	private int convert(String increment) {
		if ((increment == null) || increment.isEmpty()) {
			return 0;
		}
		if (increment.charAt(0) == '+') {
			increment = increment.substring(1);
		}
		try {
			return Integer.parseInt(increment);
		} catch (NumberFormatException e) {
			return 0;
		}
	}

	public int getYear() {
		return incrementYear;
	}

	public int getMonth() {
		return incrementMonth;
	}

	public int getDay() {
		return incrementDay;
	}

	public int getHour() {
		return incrementHour;
	}

	public int getMinute() {
		return incrementMinute;
	}
	
}
