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
package com.arcadsoftware.metadata;

import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

public class MapperException extends ResourceException {

	private static final long serialVersionUID = 6876921606134604348L;

	public MapperException(int code) {
		super(code);
	}

	public MapperException(int code, String name, String description,
			String uri) {
		super(code, name, description, uri);
	}

	public MapperException(int code, String name, String description,
			String uri, Throwable cause) {
		super(code, name, description, uri, cause);
	}

	public MapperException(int code, Throwable cause) {
		super(code, cause);
	}

	public MapperException(int code, String description) {
		super(code, (String) null, description);
	}

	public MapperException(String description, Throwable cause) {
		super(new Status(Status.SERVER_ERROR_INTERNAL, cause, description), cause);
	}

	public MapperException(String description) {
		super(new Status(Status.SERVER_ERROR_INTERNAL, description));
	}

	public MapperException(Status status) {
		super(status);
	}

	public MapperException(Status status, String description) {
		super(status, description);
	}

	public MapperException(Status status, String description,
			Throwable cause) {
		super(status, description, cause);
	}

	public MapperException(Status status, Throwable cause) {
		super(status, cause);
	}

	public MapperException(Throwable cause) {
		super(cause);
	}

}
