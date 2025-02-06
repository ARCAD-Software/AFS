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
package com.arcadsoftware.rest.internal;

import java.util.ResourceBundle;

import org.restlet.Context;
import org.restlet.Restlet;
import org.restlet.data.Language;
import org.restlet.data.Preference;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.data.Status;

import com.arcadsoftware.rest.OSGiApplication;

/**
 * Return 503 status code (Service unavailable) to any request.
 */
public class InactiveSiteRestlet extends Restlet {

	/**
	 * @param context
	 */
	public InactiveSiteRestlet(Context context) {
		super(context);
	}

	/* (non-Javadoc)
	 * @see org.restlet.Restlet#handle(org.restlet.data.Request, org.restlet.data.Response)
	 */
	@Override
	public void handle(Request request, Response response) {
		super.handle(request, response);
		// Every call generate an error message...
		String uri = ((OSGiApplication)getApplication()).getInactiveRedirectURI();
		ResourceBundle rb = ClientMessages.getResourceBundle(getClientPreferedLanguage(request));
		if ((uri == null) || (uri.length() == 0)) {
			uri = rb.getString("InactiveSiteRestlet.ErrorDefaultURI"); //$NON-NLS-1$
		}
		if (!uri.toLowerCase().startsWith("http://")) { //$NON-NLS-1$
			if (uri.charAt(0) == '/') {
				uri = request.getRootRef().getHostIdentifier() + uri;
			} else {
				uri = request.getRootRef().getHostIdentifier() + "/" + uri; //$NON-NLS-1$
			}
		}
		response.setStatus(new Status(503,rb.getString("InactiveSiteRestlet.ErrorName"),rb.getString("InactiveSiteRestlet.ErrorDescription"), uri)); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}
	
	protected Language getClientPreferedLanguage(Request request) {
		Language result = Language.ENGLISH_US;
		float quality = Float.MIN_VALUE;
		try {
			for (Preference<Language> pref : request.getClientInfo().getAcceptedLanguages()) {
				if (pref.getQuality() > quality) {
					quality = pref.getQuality();
					result = pref.getMetadata();
				}
			}
		} catch (NullPointerException e) {/*Too many null test to track*/}
		return result;
	}
}
