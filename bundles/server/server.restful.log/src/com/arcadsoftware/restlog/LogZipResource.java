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
package com.arcadsoftware.restlog;

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.Dictionary;
import java.util.Enumeration;

import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.rest.ZipFileRepresentation;

public class LogZipResource extends UserLinkedResource {

	private final ArrayList<File> logs = new ArrayList<File>();
	
	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		if (!hasRight(1)) {
			throw new ResourceException(Status.CLIENT_ERROR_FORBIDDEN);
		}
		setVariants(MediaType.APPLICATION_GNU_ZIP, MediaType.APPLICATION_ZIP);
		Dictionary<String, Object> conf = getOSGiConfiguration("org.ops4j.pax.logging"); //$NON-NLS-1$
		if (conf == null) {
			setExisting(false);
		} else {
			Enumeration<String> it = conf.keys();
			long lm = Long.MIN_VALUE;
			while (it.hasMoreElements()) {
				String k = it.nextElement();
				if ((k != null) && k.endsWith(".filename")) {
					Object fn = conf.get(k);
					if (fn != null) {
						File f = new File(fn.toString());
						if (f.isFile()) {
							logs.add(f);
							long d = f.lastModified();
							if (d > lm) {
								lm = d;
							}
						}
					}
				}
			}
			if (logs.isEmpty()) {
				setExisting(false);
			} else {
				setLastModification(new Date(lm));
				getAllowedMethods().add(Method.GET);
			}
		}
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		return new ZipFileRepresentation(variant.getMediaType(), "log.zip", logs);
	}
	
	
}
