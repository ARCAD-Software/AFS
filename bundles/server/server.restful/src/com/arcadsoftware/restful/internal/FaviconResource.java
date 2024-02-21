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

public class FaviconResource extends OSGiResource {

	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		getVariants().clear();
		setVariants(MediaType.IMAGE_ICON);
	}
	
	@Override
	public Representation get(Variant variant) throws ResourceException {
		if (MediaType.IMAGE_ICON.equals(variant.getMediaType())) {
			return new StreamRepresentation(MediaType.IMAGE_ICON) {
				@Override
				public void write(OutputStream o) throws IOException {
					try (InputStream i = FaviconResource.this.getBundleContext().getBundle().getResource("/data/favicon.ico").openStream()) { //$NON-NLS-1$
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
						return FaviconResource.this.getBundleContext().getBundle().getResource("/data/favicon.ico").openStream(); //$NON-NLS-1$
					} catch (Exception e) {
						return null;
					}
				}
			};
		}
		return super.get(variant);
	}
}