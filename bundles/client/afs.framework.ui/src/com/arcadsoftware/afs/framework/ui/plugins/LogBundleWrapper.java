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
package com.arcadsoftware.afs.framework.ui.plugins;

import org.eclipse.core.runtime.ILog;
import org.osgi.framework.Bundle;

public class LogBundleWrapper {

	private Bundle bundle;
	private String classIdentificationMessage;
	private String generalErrorMessage;
	private ILog log;

	public String getSymbolicName() {
		return bundle.getSymbolicName();
	}

	public Bundle getBundle() {
		return bundle;
	}

	public String getClassIdentificationMessage() {
		return classIdentificationMessage;
	}

	public void setClassIdentificationMessage(String classIdentificationMessage) {
		this.classIdentificationMessage = classIdentificationMessage;
	}

	public String getGeneralErrorMessage() {
		return generalErrorMessage;
	}

	public void setGeneralErrorMessage(String generalErrorMessage) {
		this.generalErrorMessage = generalErrorMessage;
	}

	public void setBundle(Bundle bundle) {
		this.bundle = bundle;
	}

	public ILog getLog() {
		return log;
	}

	public void setLog(ILog log) {
		this.log = log;
	}
}
