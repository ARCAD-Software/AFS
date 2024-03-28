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
package com.arcadsoftware.restlog;

import org.osgi.service.log.LogService;
import org.restlet.Context;
import org.restlet.Request;

public class LogReaderLevelRestlet extends LogReaderRestlet {

	public LogReaderLevelRestlet(Context context, Activator activator) {
		super(context, activator);
	}

	@Override
	protected int getLevel(Request request) {
		Object o = request.getAttributes().get("level"); //$NON-NLS-1$
		if (o != null) {
			switch(o.toString().toLowerCase()) {
			case "err": //$NON-NLS-1$
			case "error": //$NON-NLS-1$
			case "fatal": //$NON-NLS-1$
			case "1": //$NON-NLS-1$
				return LogService.LOG_ERROR;
			case "warning": //$NON-NLS-1$
			case "warn": //$NON-NLS-1$
			case "2": //$NON-NLS-1$
				return LogService.LOG_WARNING;
			case "info": //$NON-NLS-1$
			case "standard": //$NON-NLS-1$
			case "3": //$NON-NLS-1$
				return LogService.LOG_INFO;
			case "debug": //$NON-NLS-1$
			case "trace": //$NON-NLS-1$
			case "all": //$NON-NLS-1$
			case "4": //$NON-NLS-1$
				return LogService.LOG_DEBUG;
			}
		}
		return super.getLevel(request);
	}
	
}
