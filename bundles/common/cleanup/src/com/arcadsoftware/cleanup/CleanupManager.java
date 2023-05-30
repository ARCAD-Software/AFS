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
package com.arcadsoftware.cleanup;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.arcadsoftware.cleanup.logger.ICleanupLogger;
import com.arcadsoftware.cleanup.logger.SystemOutLogger;
import com.arcadsoftware.cleanup.operation.AbstractCleanOperation;

public class CleanupManager implements Runnable {

	/**
	 * The Clean-up Thread name.
	 * 
	 * <p>
	 * This thread is unique in the platform it will periodically run all declared operation (like a Timer).
	 */
	public static final String SERVICE_NAME = "AFS-Cleanup"; //$NON-NLS-1$

	private static class CleanupManagerHolder {
		private static final CleanupManager cleanupManagerSingleton = new CleanupManager();
	}

	public static CleanupManager getInstance() {
		return CleanupManagerHolder.cleanupManagerSingleton;
	}

	protected volatile int retentionDays;
	protected volatile int frequence;
	protected volatile File cleanupLogFile;
	protected volatile boolean ignoreConditions;
	protected volatile boolean followSymlinks;
	protected Date referenceDate;
	protected Thread cleanupThread;
	protected volatile boolean isActive;
	protected volatile boolean isRunning;
	protected volatile boolean sleeping;
	protected final Map<String, AbstractCleanOperation> cleanOperations;
	protected Preferences preferences;
	protected ICleanupLogger logger;

	protected CleanupManager() {
		super();
		cleanOperations = new HashMap<>();
		setPreferences(new Preferences(this));
		setLogger(new SystemOutLogger());
	}

	public void setLogger(ICleanupLogger logger) {
		this.logger = logger;
	}

	public void setPreferences(Preferences preferences) {
		this.preferences = preferences;
	}

	protected void loadPreferences() {
		isActive = preferences.getBooleanPreference(Preferences.BOOL_ACTIVE);
		retentionDays = preferences.getIntPreference(Preferences.INT_RETENTION_DAYS);
		frequence = getFrequency();
		ignoreConditions = preferences.getBooleanPreference(Preferences.BOOL_IGNORE_CONDITIONS);
		followSymlinks = preferences.getBooleanPreference(Preferences.BOOL_FOLLOW_SYMLINKS);
		final String logFile = preferences.getStringPreference(Preferences.STRING_LOGFILE_PATH);
		if ((logFile != null) && (logFile.length() > 0)) {
			cleanupLogFile = new File(logFile);
			if (cleanupLogFile.exists() && cleanupLogFile.isDirectory()) {
				logError("Cleanup log file not created: %1$s is a directory.", cleanupLogFile.getAbsolutePath());
			}
		} else {
			cleanupLogFile = null;
		}
		if (isRunning) {
			synchronized (this) {
				notifyAll();
			}
		}
	}

	public int getFrequency() {
		return preferences.getIntPreference(Preferences.INT_FREQUENCE) * 60000;
	}

	public void addCleanOperation(AbstractCleanOperation cleanOperation) {
		cleanOperation.setParentCleanupManager(this);
		cleanOperations.put(cleanOperation.getId(), cleanOperation);
	}

	public void removeCleanOperation(AbstractCleanOperation cleanOperation) {
		final AbstractCleanOperation storedCleanOperation = cleanOperations.remove(cleanOperation.getId());
		if (storedCleanOperation != null) {
			storedCleanOperation.setParentCleanupManager(null);
		}
	}

	public Map<String, AbstractCleanOperation> getCleanOperations() {
		return cleanOperations;
	}

	public Date getReferenceDate() {
		return referenceDate;
	}

	public int getRetentionDays() {
		return retentionDays;
	}

	public boolean mustFollowSymlinks() {
		return followSymlinks;
	}

	public void start() {
		if (!isRunning) {
			cleanupThread = new Thread(this, SERVICE_NAME);
			cleanupThread.start();
		} else if (sleeping) {
			synchronized (this) {
				notifyAll();
			}
		}
	}

	public void stop() {
		isRunning = false;
		try {
			cleanupThread.interrupt();
			cleanupThread = null;
		} catch (final Exception e) {
			logException(e);
		}
	}

	public boolean isRunning() {
		return isRunning;
	}

	public boolean isSleeping() {
		return sleeping;
	}

	@Override
	public void run() {
		isRunning = true;
		logInfo("Cleanup manager starts");
		while (isRunning) {
			if (sleep()) {
				final Calendar calendar = Calendar.getInstance();
				calendar.add(Calendar.DATE, -1 * retentionDays);
				referenceDate = calendar.getTime();
				try {
					// Execute cleaning
					if (isActive) {
						logInfo("Cleaning entities older than %1$s", referenceDate);
						writeCleanupLog(String.format("==> Cleanup started on %1$s%n", Calendar.getInstance().getTime()));
						writeCleanupLog(String.format("==> Cleaning files older than %1$s%n", referenceDate));
						writeCleanupLog(String.format("==> Executing %1$s cleaning operations%n%n", cleanOperations.size()));
						for (final Entry<String, AbstractCleanOperation> cleanOperation : cleanOperations.entrySet()) {
							writeCleanupLog(String.format("Starting operation %1$s %n", cleanOperation));
							try {
								cleanOperation.getValue().executeCleanOperation(ignoreConditions);
							} catch (final Exception e) {
								logException(e);
							}
						}
						logInfo("Cleaning is done");
					}
				} catch (final Exception e) {
					logException(e);
				}
			}
		}
		logInfo("Cleanup manager stopped");
	}

	protected boolean sleep() {
		try {
			synchronized (this) {
				// Waits until next cleanup
				// or if preferences changed (launch a cleanup)
				sleeping = true;
				writeCleanupLog(String.format("Cleanup manager next cleanup operation in %2$d minute(s) (Current time is %1$s)%n", Calendar.getInstance().getTime(), frequence / 60000));
				wait(frequence);
				sleeping = false;
				return true;
			}
		} catch (final InterruptedException ie) {
			isRunning = false;
			Thread.currentThread().interrupt();
		} catch (final Exception e) {
			isRunning = false;
			logException(e);
		}
		return false;
	}

	public void writeCleanupLog(final String text) {
		if ((cleanupLogFile != null) && (text != null) && !text.isEmpty()) {
			cleanupLogFile.getParentFile().mkdirs();
			try (FileOutputStream fos = new FileOutputStream(cleanupLogFile, true)) {
				fos.write(text.getBytes(StandardCharsets.UTF_8));
			} catch (final IOException e) {
				logException(e);
			}
		}
	}

	public void logInfo(String message, Object... parms) {
		log(ICleanupLogger.LOG_INFO, message, parms);
	}

	public void logWarning(String message, Object... parms) {
		log(ICleanupLogger.LOG_WARNING, message, parms);
	}

	public void logError(String message, Object... parms) {
		log(ICleanupLogger.LOG_ERROR, message, parms);
	}

	public void logDebug(String message, Object... parms) {
		log(ICleanupLogger.LOG_DEBUG, message, parms);
	}

	public void logException(Throwable e) {
		logger.logException(getLogLabel() + " unexpected exception.", e);
	}

	protected String getLogLabel() {
		return String.format("[%1$s] ", SERVICE_NAME); //$NON-NLS-1$
	}

	protected void log(int level, String message, Object... parms) {
		logger.log(level, getLogLabel() + String.format(message, parms));
	}
}
