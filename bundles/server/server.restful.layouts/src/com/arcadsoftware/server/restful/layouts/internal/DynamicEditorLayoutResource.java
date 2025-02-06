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
package com.arcadsoftware.server.restful.layouts.internal;

import java.io.File;
import java.util.Calendar;
import java.util.GregorianCalendar;

import org.restlet.resource.ResourceException;

import com.arcadsoftware.rest.XmlFileResource;

public class DynamicEditorLayoutResource extends XmlFileResource {

	private File file;
	
	@Override
	protected void doInit() throws ResourceException {
		Calendar cal = new GregorianCalendar();
		Object o = getRequest().getAttributes().get("type"); //$NON-NLS-1$
		if (o == null) {
			setExisting(false);
		} else {
			final String type = o.toString();
			String name = getFileName();
			// Get the preferred language code...
			String langcode = getLanguageCode(getClientPreferedLanguage());
			// Try Named layout file.
			if ((name != null) && (name.length() != 0)) {
				if (name.charAt(0) == '/') {
					name = name.substring(1);
				}
				// Construct the File key (URL).
				String filekey = type + "/" + name; //$NON-NLS-1$
				// Look for the requested file...
				file = Activator.getInstance().getFile(filekey, langcode, cal);
				if (file == null) {
					file = Activator.getInstance().getFile(filekey, null, cal);
				}
			}
			// Try generic layout file
			String useDefault = getRequestForm().getFirstValue("useDefault"); //$NON-NLS-1$
			if ((file == null) && ((useDefault == null) || (useDefault.length() == 0) || "true".equalsIgnoreCase(useDefault))) { //$NON-NLS-1$
				file = Activator.getInstance().getFile(type, langcode, cal);
				if (file == null) {
					file = Activator.getInstance().getFile(type, null, cal);
				}
			}
		}
		super.doInit();
		setLastModification(cal.getTime());
	}

	@Override
	protected void logError(Exception e) {
		Activator.getInstance().log(e);
	}

	@Override
	protected File defineFile() {
		return file;
	}
}
