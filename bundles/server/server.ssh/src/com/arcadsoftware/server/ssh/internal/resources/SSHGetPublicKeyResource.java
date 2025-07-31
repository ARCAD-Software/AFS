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
package com.arcadsoftware.server.ssh.internal.resources;

import java.nio.charset.StandardCharsets;

import org.restlet.data.MediaType;
import org.restlet.data.Status;
import org.restlet.representation.Representation;
import org.restlet.representation.StringRepresentation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.server.ssh.services.SSHService;
import com.arcadsoftware.ssh.model.SSHKey;

public class SSHGetPublicKeyResource extends UserLinkedResource {
	@Override
	protected Representation delete(final Variant variant) {
		throw new ResourceException(Status.CLIENT_ERROR_METHOD_NOT_ALLOWED);
	}

	@Override
	protected void doInit() {
		super.doInit();
		setVariants(MediaType.TEXT_PLAIN);
	}

	@Override
	protected Representation get(final Variant variant) {
		final int sshKeyId = getAttribute("id", 0); //$NON-NLS-1$
		if (sshKeyId > 0) {
			try {
				final SSHService sshService = getOSGiService(SSHService.class);
				if (sshService == null) {
					throw new ResourceException(Status.SERVER_ERROR_SERVICE_UNAVAILABLE, "The SSH Service is not running on the Server. This service is disable for the moment.");
				}
				final SSHKey sshKey = sshService.get(sshKeyId);
				final byte[] publicKey = sshService.getPublicKey(sshKey);
				return new StringRepresentation(new String(publicKey, StandardCharsets.UTF_8));
			} catch (final Exception e) {
				getLoggedPlugin().error(e.getLocalizedMessage(), e);
				throw new ResourceException(Status.SERVER_ERROR_INTERNAL, e);
			}
		} else {
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, "id attribute must be greater than 0");
		}
	}

	@Override
	protected Representation post(final Representation entity, final Variant variant) {
		throw new ResourceException(Status.CLIENT_ERROR_METHOD_NOT_ALLOWED);
	}

	@Override
	protected Representation put(final Representation representation, final Variant variant) {
		throw new ResourceException(Status.CLIENT_ERROR_METHOD_NOT_ALLOWED);
	}
}
