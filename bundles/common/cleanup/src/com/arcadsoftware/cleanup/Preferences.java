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
package com.arcadsoftware.cleanup;

import java.util.Dictionary;
import java.util.Enumeration;
import java.util.Properties;

public class Preferences {
	
	public static final String BOOL_ACTIVE = "cleanup.active";
	public static final String INT_RETENTION_DAYS = "cleanup.retention.days";
	public static final String INT_FREQUENCE = "cleanup.frequence";
	public static final String BOOL_IGNORE_CONDITIONS = "cleanup.ignore.conditions";
	public static final String BOOL_FOLLOW_SYMLINKS = "cleanup.follow.symlinks";
	public static final String STRING_LOGFILE_PATH = "cleanup.logfile.path";

	private final Properties cleanupPreferences;
	private final CleanupManager cleanupManager;

	public Preferences(CleanupManager cleanupManager) {
		cleanupPreferences = new Properties();
		this.cleanupManager = cleanupManager;
		loadDefaultValues();
	}

	public void notifyPreferencesChanged() {
		cleanupManager.loadPreferences();
	}

	protected void loadDefaultValues() {
		cleanupPreferences.setProperty(BOOL_ACTIVE, "true");
		cleanupPreferences.setProperty(INT_FREQUENCE, "1440");
		cleanupPreferences.setProperty(INT_RETENTION_DAYS, "14");
		cleanupPreferences.setProperty(BOOL_IGNORE_CONDITIONS, Boolean.FALSE.toString());
		cleanupPreferences.setProperty(BOOL_FOLLOW_SYMLINKS, Boolean.FALSE.toString());
		cleanupPreferences.setProperty(STRING_LOGFILE_PATH, "");
	}

	public Properties getAllPreferences() {
		return cleanupPreferences;
	}

	public void loadAll(Dictionary<String, Object> properties) {
		final Enumeration<String> keys = properties.keys();
		while (keys.hasMoreElements()) {
			final String key = keys.nextElement();
			if (cleanupPreferences.containsKey(key)) {
				cleanupPreferences.setProperty(key, String.valueOf(properties.get(key)));
			}
		}
		notifyPreferencesChanged();
	}

	protected String getPreference(String key, String defaultValue) {
		return cleanupPreferences.getProperty(key, defaultValue);
	}

	public String getStringPreference(String key) {
		return getPreference(key, "");
	}

	public boolean getBooleanPreference(String key) {
		return Boolean.parseBoolean(getPreference(key, "false"));
	}

	public int getIntPreference(String key) {
		return Integer.parseInt(getPreference(key, String.valueOf(Integer.MIN_VALUE)));
	}
}
