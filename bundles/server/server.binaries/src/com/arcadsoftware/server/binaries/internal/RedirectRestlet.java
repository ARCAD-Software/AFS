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
package com.arcadsoftware.server.binaries.internal;

import org.restlet.Context;
import org.restlet.Restlet;
import org.restlet.data.Reference;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.data.Status;

public class RedirectRestlet extends Restlet {

	private Activator activator;

	private static final String LOCALHOST = "http://localhost"; //$NON-NLS-1$

	public RedirectRestlet(Context context, Activator activator) {
		super(context);
		this.activator = activator;
	}

	@Override
	public void handle(Request request, Response response) {
		// User
		//ConnectionUserBean user = (ConnectionUserBean) request.getAttributes().get(ConnectionUserBean.CONNECTED_USER);

		String type = null;
		Object o = request.getAttributes().get("type"); //$NON-NLS-1$
		if (o instanceof String) {
			type = (String) o;
		}
		if (type == null) {
			response.setStatus(Status.CLIENT_ERROR_BAD_REQUEST);
			return;
		}
		// TODO Faille de sécurité si type = attachment alors on peut télécharger une pièce jointe sans en avoir le droit.
		// TODO Faille de sécurité on peut télécharger n'importe élément enregistré sur le disque !
		// id
		int id = 0;
		o = request.getAttributes().get("id"); //$NON-NLS-1$
		if (o instanceof Integer) {
			id = (Integer) o;
		} else if (o != null) {
			try {
				id = Integer.parseInt(o.toString());
			} catch (NumberFormatException e) {
			}
		}
		if (id == 0) {
			response.setStatus(Status.CLIENT_ERROR_BAD_REQUEST);
			return;
		}
		String uri = activator.getBinTransfer().generateKey(type, id, false);
		if (uri == null) {
			response.setStatus(Status.SERVER_ERROR_SERVICE_UNAVAILABLE);
			return;
		}
		Reference ref = new Reference(request.getResourceRef(), uri);
		if (uri.startsWith(LOCALHOST)) {
			// Local resource use same, protocol, host and port.
			ref.setProtocol(request.getProtocol());
			ref.setHostDomain(request.getResourceRef().getHostDomain(true));
			ref.setHostPort(request.getResourceRef().getHostPort());
		}
		response.setLocationRef(ref);
		response.setStatus(Status.REDIRECTION_TEMPORARY);
	}

}
