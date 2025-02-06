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
package com.arcadsoftware.osgi.internal;

/**
 * Temp log entry, waiting to be logged.
 * @see LogTracker
 */
public class WaitingLogEntry {

	private final int level;
	private final String message;
	private final Object[] objects;

	public WaitingLogEntry(int level, String message, Object... objects) {
		super();
		this.objects = objects;
		this.level = level;
		this.message = message;
	}

	public int getLevel() {
		return level;
	}

	public String getMessage() {
		return message;
	}

	public Object[] getObjects() {
		return objects;
	}
}
