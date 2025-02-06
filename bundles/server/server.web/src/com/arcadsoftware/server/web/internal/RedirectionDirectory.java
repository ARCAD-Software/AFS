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
package com.arcadsoftware.server.web.internal;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Dictionary;
import java.util.Enumeration;

import org.restlet.Context;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.data.CharacterSet;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Preference;
import org.restlet.data.Status;
import org.restlet.representation.StringRepresentation;
import org.restlet.resource.Directory;

public class RedirectionDirectory extends Directory {

	final private String rootDirName;
	final private Activator activator;
	final private String path;

	public RedirectionDirectory(Activator activator, Context context, String path, String rootDirName, String rootUri) {
		super(context, rootUri);
		this.path = path;
		this.rootDirName = rootDirName;
		this.activator = activator;
	}

	@Override
	public void handle(Request request, Response response) {
		// TODO Add "templates files" which are "linked" to OSGi properties... !
		super.handle(request, response);
		int err = response.getStatus().getCode();
		if ((err >= 300) && (err < 1000)) {
			String baseName = "web/" + rootDirName + '/'; //$NON-NLS-1$
			String name = Integer.toString(err);
			// Configuration based redirection.
			Dictionary<String, Object> conf = activator.getCurrentConfiguration();
			if (conf != null) {
				Object redirection = conf.get(rootDirName + '.' + name + ".redirect"); //$NON-NLS-1$
				if (!(redirection instanceof String)) {
					redirection = conf.get(rootDirName + ".default.redirect"); //$NON-NLS-1$
				}
				if (redirection instanceof String) {
					response.setStatus(Status.REDIRECTION_TEMPORARY);
					response.setLocationRef(path + (String) redirection);
					return;
				}
			}
			// Resource based redirection.
			URL url = getEntry(baseName, name + ".htm"); //$NON-NLS-1$
			if (url == null) {
				url = getEntry(baseName, name + ".HTM"); //$NON-NLS-1$
			}
			if (url == null) {
				url = getEntry(baseName, name + ".html"); //$NON-NLS-1$
			}
			if (url == null) {
				url = getEntry(baseName, name + ".xhtml"); //$NON-NLS-1$
			}
			if (url != null) {
				try {
					// As we return a file we must change the status of the response...
					response.setStatus(Status.SUCCESS_OK);
					File file = activator.toFile(url);
					response.setEntity(new StringRepresentation(new String(Files.readAllBytes(file.toPath()), StandardCharsets.UTF_8), 
							getMediaType(file), getPreferedLanguage(request), CharacterSet.UTF_8));
				} catch (IOException e) {
					activator.error(e);
				}
			}
		}
	}

	private URL getEntry(String baseName, String name) {
		// look for any maching ressource in all attached fragments...
		Enumeration<URL> entries = activator.getContext().getBundle().findEntries(baseName, name, true);
		if ((entries != null) && entries.hasMoreElements()) {
			return entries.nextElement(); 
		}
		return null;
	}

	private MediaType getMediaType(File file) {
		if (file.getName().endsWith(".xhtml")) { //$NON-NLS-1$
			return MediaType.APPLICATION_XHTML;
		}
		return MediaType.TEXT_HTML;
	}

	private Language getPreferedLanguage(Request request) {
		Language result = Language.ENGLISH_US;
		if ((request == null) || (request.getClientInfo() == null) || (request.getClientInfo().getAcceptedLanguages() == null)) {
			return result;
		}
		float quality = Float.MIN_VALUE;
		for (Preference<Language> pref : request.getClientInfo().getAcceptedLanguages()) {
			if ((pref != null) && (pref.getQuality() > quality)) {
				quality = pref.getQuality();
				result = pref.getMetadata();
			}
		}
		return result;
	}
	
}
