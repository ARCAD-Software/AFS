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
package com.arcadsoftware.restful.connection.local;

import org.restlet.Context;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.Restlet;
import org.restlet.data.Form;
import org.restlet.data.Reference;

public class TestPasswordRestlet extends Restlet {

	public TestPasswordRestlet(Context context) {
		super(context);
	}

	@Override
	public void handle(Request request, Response response) {
		super.handle(request, response);
		Reference ref = request.getResourceRef();
		Form form = new Form();
		form.set("login", (String) request.getAttributes().get("a")); //$NON-NLS-1$ //$NON-NLS-2$
		form.set("oldpassword", (String) request.getAttributes().get("b")); //$NON-NLS-1$ //$NON-NLS-2$
		form.set("newpassword", (String) request.getAttributes().get("c")); //$NON-NLS-1$ //$NON-NLS-2$
		response.redirectPermanent(new Reference(ref.getScheme(), ref.getHostDomain(isStarted()), ref.getHostPort(), "localauth/test/password", form.getQueryString(), null));
	}
}
