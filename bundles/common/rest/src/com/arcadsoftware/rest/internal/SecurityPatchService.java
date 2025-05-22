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

import org.restlet.Context;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.data.Header;
import org.restlet.routing.Filter;
import org.restlet.service.Service;
import org.restlet.util.Series;

import com.arcadsoftware.rest.OSGiApplication;

public class SecurityPatchService extends Service {

	private final OSGiApplication application;
	
	public SecurityPatchService(OSGiApplication application) {
		super();
		this.application = application;
	}
	
	@Override
	public Filter createInboundFilter(Context context) {
		return new Filter(context) {
			@Override
			protected void afterHandle(Request request, Response response) {
				// Remove version information from "server" header !
				response.getServerInfo().setAgent("ARCAD Foundation Services"); //$NON-NLS-1$
				// Add security related headers
				if (!Boolean.getBoolean("com.arcadsoftware.cors.header.disabled") && //$NON-NLS-1$
						!Boolean.getBoolean("com.arcadsoftware.security.headers.disabled")) { //$NON-NLS-1$
					Series<Header> responseHeaders = response.getHeaders();
					// CSP:
					StringBuilder sb = new StringBuilder("default-src 'self'"); //$NON-NLS-1$
					String v = System.getProperty("com.arcadsoftware.csp.image.sources"); //$NON-NLS-1$
					if (v != null) {
						v = v.trim();
						if (!v.isEmpty() && !"'self'".equalsIgnoreCase(v)) { //$NON-NLS-1$
							sb.append("; img-src " + v); //$NON-NLS-1$
						}
					}
					v = System.getProperty("com.arcadsoftware.csp.media.sources"); //$NON-NLS-1$
					if (v != null) {
						v = v.trim();
						if (!v.isEmpty() && !"'self'".equalsIgnoreCase(v)) { //$NON-NLS-1$
							sb.append("; media-src " + v); //$NON-NLS-1$
						}
					}
					v = System.getProperty("com.arcadsoftware.csp.font.sources"); //$NON-NLS-1$
					if (v != null) {
						v = v.trim();
						if (!v.isEmpty() && !"'self'".equalsIgnoreCase(v)) { //$NON-NLS-1$
							sb.append("; font-src " + v); //$NON-NLS-1$
						}
					}
					v = System.getProperty("com.arcadsoftware.csp.style.sources"); //$NON-NLS-1$
					if (v != null) {
						v = v.trim();
						if (!v.isEmpty() && !"'self'".equalsIgnoreCase(v)) { //$NON-NLS-1$
							sb.append("; style-src " + v); //$NON-NLS-1$
						}
					}
					v = System.getProperty("com.arcadsoftware.csp.script.sources"); //$NON-NLS-1$
					if ((v != null) && !v.trim().isEmpty()) {
						sb.append("; script-src " + v.trim()); //$NON-NLS-1$
					} else {
						sb.append("; script-src 'self' 'unsafe-eval'"); //$NON-NLS-1$
					}
					v = System.getProperty("com.arcadsoftware.csp.script.elem.sources"); //$NON-NLS-1$
					if ((v != null) && !v.trim().isEmpty()) {
						sb.append("; script-src-elem " + v.trim()); //$NON-NLS-1$
					}
					v = System.getProperty("com.arcadsoftware.csp.script.attr.sources"); //$NON-NLS-1$
					if ((v != null) && !v.trim().isEmpty()) {
						sb.append("; script-src-attr " + v.trim()); //$NON-NLS-1$
					}
					// source: https://developer.mozilla.org/fr/docs/Web/HTTP/Headers/Content-Security-Policy
					responseHeaders.add(new Header("Content-Security-Policy", sb.toString())); //$NON-NLS-1$
					// For browser not ignoring Content-Security-Policy directive
					// https://developer.mozilla.org/fr/docs/Web/HTTP/Headers/X-Frame-Options
					responseHeaders.add(new Header("X-Frame-Options", "SAMEORIGIN")); //$NON-NLS-1$ //$NON-NLS-2$
					// MIME: 
					responseHeaders.add(new Header("X-Content-Type-Options", "nosniff")); //$NON-NLS-1$ //$NON-NLS-2$
					// CORS:
					if (!Boolean.getBoolean("com.arcadsoftware.corsservice.disabled")) { //$NON-NLS-1$
						responseHeaders.add(new Header("Referrer-Policy", "origin-when-cross-origin")); //$NON-NLS-1$ //$NON-NLS-2$
					}
					// CORP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Cross-Origin_Resource_Policy_(CORP)
					responseHeaders.add(new Header("Cross-Origin-Resource-Policy", "same-origin")); //$NON-NLS-1$ //$NON-NLS-2$
					if (application.isHttps()) {
						// Put a max age (of using HTTPS) equals to 2 years as recommended in:
						// https://hstspreload.org/
						response.getHeaders().add(new Header("Strict-Transport-Security", "max-age=63072000")); //$NON-NLS-1$ //$NON-NLS-2$
					}
				}
			}
		};
	}
}
