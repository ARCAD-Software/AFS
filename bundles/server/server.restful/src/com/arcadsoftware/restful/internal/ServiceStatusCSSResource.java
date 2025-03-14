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
package com.arcadsoftware.restful.internal;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.restlet.data.MediaType;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;
import org.restlet.representation.StreamRepresentation;

import com.arcadsoftware.rest.OSGiResource;

/**
 * Serve the static CSS file used by the HTML representation of the default status resource.
 * 
 * @author ARCAD Software
 */
public class ServiceStatusCSSResource extends OSGiResource {

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		getVariants().clear();
		setVariants(MediaType.TEXT_CSS);
	}
	
	@Override
	public Representation get(Variant variant) throws ResourceException {
		if (MediaType.TEXT_CSS.equals(variant.getMediaType())) {
			return new StreamRepresentation(MediaType.TEXT_CSS) {
				@Override
				public void write(OutputStream o) throws IOException {
					try (InputStream i = ServiceStatusCSSResource.this.getBundleContext().getBundle().getResource("/data/status.css").openStream()) { //$NON-NLS-1$
						byte[] b = new byte[1024];
						int r = 1;
						while(r > 0) {
							r = i.read(b);
							if (r > -1) {
								o.write(b, 0, r);
							}
						}
						o.flush();
					}
				}
			
				@Override
				public InputStream getStream() throws IOException {
					try {
						return ServiceStatusCSSResource.this.getBundleContext().getBundle().getResource("/data/status.css").openStream(); //$NON-NLS-1$
					} catch (Exception e) {
						return null;
					}
				}
			};
		}
		return super.get(variant);
	}
}