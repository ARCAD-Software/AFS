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
package com.arcadsoftware.metadata.binary;

import org.osgi.framework.ServiceReference;
import org.restlet.data.Reference;
import org.restlet.data.Status;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.metadata.rest.DataItemResource;
import com.arcadsoftware.osgi.IBinariesTranferService;
import com.arcadsoftware.osgi.IBinariesTranferService.BinaryAccess;

public class BinaryRedirectionResource extends DataItemResource {

	private String uri;
	private String category;
	private boolean isReadOnly;

	private IBinariesTranferService service;

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		if (!isExisting()) {
			return;
		}
		if ((getItems() == null) || (getItems().size() != 1)) {
			setExisting(false);
			return;
		}
		category = getEntity().getMetadata().getString("binary"); //$NON-NLS-1$
		if ((category == null) || (category.length() == 0)) {
			setExisting(false);
			return;
		}
		// Preselect the first category as default one (back compatibility)
		String[] categories = category.split(" "); //$NON-NLS-1$
		if ((categories == null) || (categories.length == 0)) {
			setExisting(false);
			return;
		}
		if (categories.length > 1) {
			category = categories[0];
		}
		// Look for the user selected binary file.
		String key = getAttribute("binkey"); //$NON-NLS-1$
		if ((key != null) && (key.length() > 0)) {
			for (String c: categories) {
				if (c.indexOf('/') >= 0) {
					String[] s = c.split("/"); //$NON-NLS-1$
					if ((s != null) && (s.length > 0) && s[s.length - 1].equalsIgnoreCase(key)) {
						category = c;
						break;
					}
				} else if (c.equalsIgnoreCase(key)) {
					category = c;
					break;
				}
			}
		}
		ServiceReference ref = Activator.getBundleContext().getServiceReference(IBinariesTranferService.clazz);
		if (ref == null) {
			setExisting(false);
			return;
		}
		service = (IBinariesTranferService) Activator.getBundleContext().getService(ref);
		if (service != null) {
			// Test the user Access right to set the read-only flag.
			isReadOnly = !hasRightUpdate(getEntity(), getItems().get(0), getClientPreferedLanguage());
		}
	}

	private void redirect() {
		Reference ori = getRequest().getOriginalRef();
		getRequest().getProtocol();
		Reference ref = new Reference(ori, uri);
		if (uri.startsWith("http://localhost")) { //$NON-NLS-1$
			// Force the resource to use the protocol, host and port of original request...
			ref.setProtocol(ori.getSchemeProtocol());
			ref.setHostDomain(ori.getHostDomain(true));
			int port = ori.getHostPort();
			if (port != -1) {
				ref.setHostPort(port);
			} else {
				ref.setHostPort(null);
			}
			getOSGiApplication().getActivator().debug("Binary redirection converted to {}", ref.toString());
		}
		setLocationRef(ref);
		setStatus(Status.REDIRECTION_TEMPORARY);
	}
	
	@Override
	protected Representation head(Variant variant) throws ResourceException {
		return get(variant);
	}

	@Override
	protected Representation delete(Variant variant) throws ResourceException {
		// Deleting an attachment is associated to a modification of the data, not a deletion.
		if (isReadOnly) {
			throw new ResourceException(Status.CLIENT_ERROR_FORBIDDEN);
		}
		if (service != null) {
			uri = service.generateKey(category, getItems().get(0).getId(), BinaryAccess.DELETE);
			if (uri == null) {
				setExisting(false);
			}
		}
		redirect();
		return null;
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		if (!hasRightRead(getEntity(), getItems().get(0), getClientPreferedLanguage())) {
			throw new ResourceException(Status.CLIENT_ERROR_FORBIDDEN);
		}
		if (service != null) {
			// Back compatibility: a user allowed to update used to get, on a GET, a key
			// that stayed usable to upload. Only a filesystem store honors READ_WRITE.
			uri = service.generateKey(category, getItems().get(0).getId(),
					isReadOnly ? BinaryAccess.READ : BinaryAccess.READ_WRITE);
			if (uri == null) {
				setExisting(false);
			}
		}
		redirect();
		return null;
	}

	@Override
	protected Representation post(Representation entity, Variant variant) throws ResourceException {
		return put(entity, variant);
	}

	@Override
	protected Representation put(Representation representation, Variant variant) throws ResourceException {
		if (isReadOnly) {
			throw new ResourceException(Status.CLIENT_ERROR_FORBIDDEN);
		}
		if (service != null) {
			uri = service.generateKey(category, getItems().get(0).getId(), BinaryAccess.WRITE);
			if (uri == null) {
				setExisting(false);
			}
		}
		redirect();
		return null;
	}
}