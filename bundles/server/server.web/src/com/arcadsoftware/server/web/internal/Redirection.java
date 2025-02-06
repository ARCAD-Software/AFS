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

import org.restlet.Context;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.Restlet;

public class Redirection extends Restlet {

	private final String path;
	
	public Redirection(Context context, String path) {
		super(context);
		this.path = path;
	}

	@Override
	public void handle(Request request, Response response) {
		response.redirectPermanent(path);
	}

	
}
