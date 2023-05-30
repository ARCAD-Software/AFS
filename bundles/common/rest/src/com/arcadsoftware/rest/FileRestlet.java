/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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
package com.arcadsoftware.rest;

import java.io.File;

import org.restlet.Context;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.Restlet;
import org.restlet.data.Method;
import org.restlet.data.Status;
import org.restlet.representation.FileRepresentation;

public class FileRestlet extends Restlet {

	private final File file;
	
	public FileRestlet(Context context, File file) {
		super(context);
		this.file = file;
	}

	@Override
	public void handle(Request request, Response response) {
		super.handle(request, response);
		response.getAllowedMethods().add(Method.GET);
		response.getAllowedMethods().add(Method.OPTIONS);
		response.getAllowedMethods().add(Method.HEAD);
		if (!file.isFile()) {
            response.setStatus(Status.CLIENT_ERROR_NOT_FOUND);
		} else if (Method.GET.equals(request.getMethod())) {
			response.setEntity(new FileRepresentation(file, FileMediaType.guess(file.getName())));
			response.setStatus(Status.SUCCESS_OK);
		} else if (Method.HEAD.equals(request.getMethod()) || Method.OPTIONS.equals(request.getMethod())) {
				response.setStatus(Status.SUCCESS_NO_CONTENT);
		} else {
			response.setStatus(Status.CLIENT_ERROR_METHOD_NOT_ALLOWED);
		}
	}
	
}
