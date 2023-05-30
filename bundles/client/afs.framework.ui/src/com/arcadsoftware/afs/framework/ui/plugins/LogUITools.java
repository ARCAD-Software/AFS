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
package com.arcadsoftware.afs.framework.ui.plugins;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.statushandlers.StatusManager;
import org.osgi.framework.Bundle;

import com.arcadsoftware.afs.framework.messages.IMSGConstants;
import com.arcadsoftware.afs.framework.messages.LogMessage;
import com.arcadsoftware.afs.framework.messages.StatusMessage;
import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.afs.framework.plugins.LogTools;
import com.arcadsoftware.afs.framework.ui.internal.Activator;

public class LogUITools extends LogTools {

	public static MultiStatus createServiceInformation(Bundle bundle, int severity, String message, Throwable exception) {
		String symbolicName = bundle.getSymbolicName();
		MultiStatus serviceInformationStatus = new MultiStatus(symbolicName, severity, message, exception);
		for (BundleProperty bp: BUNDLE_PROPERTIES) {
			Object o = bundle.getHeaders().get(bp.getKey());
			if (o != null) {
				String value = o.toString();
				serviceInformationStatus.add(
						createStatus(bundle, severity, bp.getLabel() + ": " + value) //$NON-NLS-1$
				);
			}
		}
		return serviceInformationStatus;
	}

	public static void logWarning(LogBundleWrapper logInfo, String warningMessage) {
		logInfo.getLog().log(new Status(IStatus.WARNING, logInfo.getSymbolicName(), warningMessage));
	}

	public static void logInfo(LogBundleWrapper logInfo, String infoMessage) {
		logInfo.getLog().log(new Status(IStatus.INFO, logInfo.getSymbolicName(), infoMessage));
	}

	public static void logError(LogBundleWrapper logInfo, Object parentObject, String problemTitle, String detailMessage, Exception e) {
		logError(logInfo, parentObject, problemTitle, detailMessage, e, true);
	}

	public static void logError(LogBundleWrapper logInfo, Object parentObject, final String problemTitle, 
			final String detailMessage, Exception e, boolean showDialog) {
		MultiStatus errorStatus;
		if ((e == null) || (e.getMessage() == null)) {
			errorStatus = createServiceInformation(logInfo.getBundle(), IStatus.ERROR, problemTitle, e);
		} else {
			errorStatus = createServiceInformation(logInfo.getBundle(), IStatus.ERROR, e.getLocalizedMessage(), e);
		}
		// Add in error specific info
		String className = parentObject.getClass().getName();
		String messageText = Activator.resString("logTools.msg.error.classIdentification"); //$NON-NLS-1$
		String message = String.format(messageText, className);
		message = message + "\n\n" + detailMessage;//$NON-NLS-1$
		errorStatus.add(new Status(IStatus.ERROR, logInfo.getSymbolicName(), IStatus.ERROR, message, e));
		if (showDialog) {
			StatusManager.getManager().handle(errorStatus, StatusManager.LOG | StatusManager.SHOW);
		} else {
			StatusManager.getManager().handle(errorStatus, StatusManager.LOG);
		}
	}

	public static void logError(Bundle bundle, String errorMessage) {
		Status errorStatus = createServiceInformation(bundle, IStatus.ERROR, errorMessage, null);
		StatusManager.getManager().handle(errorStatus, StatusManager.LOG | StatusManager.SHOW);
	}

	public static void logWarning(Bundle bundle, String errorMessage) {
		Status errorStatus = createServiceInformation(bundle, IStatus.WARNING, errorMessage, null);
		StatusManager.getManager().handle(errorStatus, StatusManager.LOG | StatusManager.SHOW);
	}

	public static void logError(Bundle bundle, Throwable e) {
		Status errorStatus = createServiceInformation(bundle, IStatus.ERROR, e.getLocalizedMessage(), e);
		StatusManager.getManager().handle(errorStatus, StatusManager.LOG | StatusManager.SHOW);
	}

	public static void logError(Bundle bundle, UserMessage um) {
		if (um != null) {
			MultiStatus errorStatus = createServiceInformation(bundle, IStatus.ERROR, um.getLabel(), null);
			errorStatus.add(
					new Status(IStatus.ERROR, bundle.getSymbolicName(), IStatus.ERROR, um.getTextLevel2(), null));
			StatusManager.getManager().handle(errorStatus, StatusManager.LOG);
		}
	}

	public static void logInfo(Bundle bundle, Throwable e) {
		Status errorStatus = createServiceInformation(bundle, IStatus.INFO, e.getLocalizedMessage(), e);
		StatusManager.getManager().handle(errorStatus, StatusManager.LOG);
	}

	public static void logWarning(Bundle bundle, Throwable e) {
		Status errorStatus = createServiceInformation(bundle, IStatus.WARNING, e.getLocalizedMessage(), e);
		StatusManager.getManager().handle(errorStatus, StatusManager.LOG);
	}

	public static void logWarning(Bundle bundle, UserMessage um) {
		MultiStatus errorStatus = createServiceInformation(bundle, IStatus.WARNING, um.getLabel(), null);
		errorStatus.add(new Status(IStatus.WARNING, bundle.getSymbolicName(), IStatus.WARNING, um.getTextLevel2(), null));
		StatusManager.getManager().handle(errorStatus, StatusManager.LOG);
	}

	public static void log(Bundle bundle, StatusMessage message, boolean display) {
		int statusValue = (message.getStatus() == IMSGConstants.MESSAGE_STATUS_OK ? IStatus.OK : IStatus.ERROR);
		MultiStatus status = createServiceInformation(bundle, statusValue, message.getTextLevel1(), null);
		status.add(new Status(statusValue, bundle.getSymbolicName(), statusValue, message.getTextLevel2(), null));
		for (LogMessage log: message.getDetails()) {
			switch (log.getLevel()) {
			case IMSGConstants.MESSAGE_LEVEL_ERROR:
				statusValue = IStatus.ERROR;
				break;
			case IMSGConstants.MESSAGE_LEVEL_WARNING:
				statusValue = IStatus.WARNING;
				break;
			default:
				statusValue = IStatus.INFO;
				break;
			}
			MultiStatus logStatus = new MultiStatus(bundle.getSymbolicName(), statusValue, log.getTextLevel1(), null);
			logStatus.add(new Status(statusValue, bundle.getSymbolicName(), statusValue, log.getTextLevel2(), null));
			status.add(logStatus);
		}
		int style = StatusManager.LOG;
		if (display) {
			style = style | StatusManager.SHOW;
		}
		StatusManager.getManager().handle(status, style);
	}
}
